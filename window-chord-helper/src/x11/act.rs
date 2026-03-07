use anyhow::{Context, Result};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{
    Atom, ChangeWindowAttributesAux, ClientMessageEvent, ConnectionExt as _, EventMask, Window,
};
use x11rb::protocol::Event;

use super::X11Context;
use crate::types::{Extents, Geometry, OperationResult};

impl X11Context {
    /// Set window geometry (move and/or resize).
    ///
    /// The caller specifies the desired *visible content* rectangle.  For CSD
    /// windows the geometry is expanded by `_GTK_FRAME_EXTENTS` so that the
    /// invisible shadow borders extend off-screen.
    ///
    /// Any maximized/fullscreen states are removed first.  We subscribe to
    /// `PropertyNotify` on the window — the same mechanism a window manager
    /// uses — to know when the WM and the application have finished updating
    /// `_NET_WM_STATE` and `_GTK_FRAME_EXTENTS`.
    pub fn set_window_geometry(&self, window: Window, geom: &Geometry) -> Result<()> {
        let initial_extents = self.get_gtk_frame_extents(window)?;
        let states = self.get_window_states(window)?;
        let dominated = [
            self.atoms().net_wm_state_fullscreen,
            self.atoms().net_wm_state_maximized_horz,
            self.atoms().net_wm_state_maximized_vert,
        ];
        let needs_unmaximize = dominated.iter().any(|a| states.contains(a));

        let csd_extents = if needs_unmaximize {
            self.select_property_notify(window)?;

            self.remove_window_states(window, &dominated)?;
            self.conn().flush()?;

            self.wait_until(window, std::time::Duration::from_millis(500), |ctx| {
                let s = ctx.get_window_states(window)?;
                Ok(!dominated.iter().any(|a| s.contains(a)))
            })?;

            // CSD apps update _GTK_FRAME_EXTENTS asynchronously after
            // un-maximizing.  The property may also be absent while the
            // window is maximized and only appear after the transition.
            // Wait for the value to change and then stabilize, since it
            // may pass through intermediate values.
            self.wait_until(window, std::time::Duration::from_millis(500), |ctx| {
                let ext = ctx.get_gtk_frame_extents(window)?;
                Ok(ext != initial_extents)
            })?;
            loop {
                let current = self.get_gtk_frame_extents(window)?;
                let changed = self.wait_until(
                    window,
                    std::time::Duration::from_millis(50),
                    |ctx| {
                        let ext = ctx.get_gtk_frame_extents(window)?;
                        Ok(ext != current)
                    },
                )?;
                if !changed {
                    break;
                }
            }

            self.deselect_property_notify(window)?;

            self.get_gtk_frame_extents(window)?
        } else {
            initial_extents
        };

        let adjusted = if let Some(ext) = csd_extents {
            Geometry {
                x: geom.x - ext.left as i32,
                y: geom.y,
                width: geom.width + ext.left + ext.right,
                height: geom.height,
            }
        } else {
            *geom
        };

        // Cross-monitor moves fail when the target height exceeds the current
        // monitor's height.  Work around this with a two-step move: first move
        // with a conservative height, then resize to the target height.
        let current_geom = self.get_window_geometry(window)?;
        let current_monitor_h = self.monitor_height_at(current_geom.x, current_geom.width)?;

        if adjusted.height > current_monitor_h {
            let interim = Geometry {
                height: current_monitor_h,
                ..adjusted
            };
            self.send_moveresize(window, &interim)?;
            self.conn().flush()?;

            self.send_moveresize(window, &adjusted)?;
        } else {
            self.send_moveresize(window, &adjusted)?;
        }

        self.conn().flush()?;
        Ok(())
    }

    /// Return the height of the monitor whose horizontal span contains
    /// the center of a window at position `x` with width `w`.
    fn monitor_height_at(&self, x: i32, w: u32) -> Result<u32> {
        let center_x = x + (w as i32) / 2;
        let monitors = self.get_monitors()?;
        for m in &monitors {
            let mx = m.geometry.x;
            let mw = m.geometry.width as i32;
            if center_x >= mx && center_x < mx + mw {
                return Ok(m.geometry.height);
            }
        }
        Ok(1080)
    }

    fn select_property_notify(&self, window: Window) -> Result<()> {
        let attrs = ChangeWindowAttributesAux::new().event_mask(EventMask::PROPERTY_CHANGE);
        self.conn()
            .change_window_attributes(window, &attrs)?
            .check()
            .context("Failed to select PropertyNotify on window")?;
        Ok(())
    }

    fn deselect_property_notify(&self, window: Window) -> Result<()> {
        let attrs = ChangeWindowAttributesAux::new().event_mask(EventMask::NO_EVENT);
        self.conn()
            .change_window_attributes(window, &attrs)?
            .check()
            .context("Failed to deselect PropertyNotify on window")?;
        Ok(())
    }

    /// Block until `predicate` returns true, draining `PropertyNotify` events
    /// in between.  Returns whether the predicate was satisfied before the
    /// timeout.
    fn wait_until(
        &self,
        _window: Window,
        timeout: std::time::Duration,
        predicate: impl Fn(&Self) -> Result<bool>,
    ) -> Result<bool> {
        if predicate(self)? {
            return Ok(true);
        }

        let deadline = std::time::Instant::now() + timeout;

        loop {
            match self.conn().poll_for_event()? {
                Some(Event::PropertyNotify(_)) => {
                    if predicate(self)? {
                        return Ok(true);
                    }
                }
                Some(_) => {}
                None => {
                    if std::time::Instant::now() >= deadline {
                        return Ok(false);
                    }
                    std::thread::sleep(std::time::Duration::from_millis(1));
                }
            }
        }
    }

    /// Returns `None` for non-CSD windows.
    fn get_gtk_frame_extents(&self, window: Window) -> Result<Option<Extents>> {
        let reply = self
            .conn()
            .get_property(
                false,
                window,
                self.atoms().gtk_frame_extents,
                x11rb::protocol::xproto::AtomEnum::ANY,
                0,
                4,
            )?
            .reply()
            .context("Failed to read _GTK_FRAME_EXTENTS")?;

        if reply.format == 32 && reply.value.len() >= 16 {
            let vals: Vec<u32> = reply
                .value
                .chunks_exact(4)
                .take(4)
                .map(|c| u32::from_ne_bytes(c.try_into().unwrap()))
                .collect();
            Ok(Some(Extents {
                left: vals[0],
                right: vals[1],
                top: vals[2],
                bottom: vals[3],
            }))
        } else {
            Ok(None)
        }
    }

    fn send_moveresize(&self, window: Window, geom: &Geometry) -> Result<()> {
        let flags: u32 = (1 << 8) | (1 << 9) | (1 << 10) | (1 << 11) | (2 << 12);

        let data = [flags, geom.x as u32, geom.y as u32, geom.width, geom.height];

        let event = ClientMessageEvent {
            response_type: 33, // ClientMessage
            format: 32,
            sequence: 0,
            window,
            type_: self.atoms().net_moveresize_window,
            data: data.into(),
        };

        self.conn()
            .send_event(
                false,
                self.root(),
                EventMask::SUBSTRUCTURE_REDIRECT | EventMask::SUBSTRUCTURE_NOTIFY,
                event,
            )?
            .check()
            .context("Failed to send _NET_MOVERESIZE_WINDOW client message")?;

        Ok(())
    }

    pub fn modify_window_state(
        &self,
        window: Window,
        add_states: &[Atom],
        remove_states: &[Atom],
    ) -> Result<()> {
        if !remove_states.is_empty() {
            self.remove_window_states(window, remove_states)?;
        }
        if !add_states.is_empty() {
            self.add_window_states(window, add_states)?;
        }
        self.conn().flush()?;
        Ok(())
    }

    fn add_window_states(&self, window: Window, states: &[Atom]) -> Result<()> {
        for &state in states {
            self.send_wm_state_message(window, 1, state, 0)?;
        }
        Ok(())
    }

    fn remove_window_states(&self, window: Window, states: &[Atom]) -> Result<()> {
        for &state in states {
            self.send_wm_state_message(window, 0, state, 0)?;
        }
        Ok(())
    }

    fn send_wm_state_message(
        &self,
        window: Window,
        action: u32,
        property1: Atom,
        property2: Atom,
    ) -> Result<()> {
        let data = [action, property1, property2, 1, 0];

        let event = ClientMessageEvent {
            response_type: 33,
            format: 32,
            sequence: 0,
            window,
            type_: self.atoms().net_wm_state,
            data: data.into(),
        };

        self.conn()
            .send_event(
                false,
                self.root(),
                EventMask::SUBSTRUCTURE_REDIRECT | EventMask::SUBSTRUCTURE_NOTIFY,
                event,
            )?
            .check()
            .context("Failed to send _NET_WM_STATE client message")?;

        Ok(())
    }

    pub fn execute_operations(
        &self,
        operations: &[crate::types::Operation],
    ) -> Result<Vec<OperationResult>> {
        let mut results = Vec::new();

        for (i, operation) in operations.iter().enumerate() {
            let result = match operation {
                crate::types::Operation::SetWindowGeometry { window, geometry } => {
                    self.execute_set_geometry(window, geometry)
                }
                crate::types::Operation::SetWindowState {
                    window,
                    add_states,
                    remove_states,
                } => self.execute_set_state(window, add_states, remove_states),
                crate::types::Operation::MoveWindow { window, geometry } => {
                    self.execute_set_geometry(window, geometry)
                }
            };

            results.push(match result {
                Ok(()) => OperationResult {
                    operation: i,
                    success: true,
                    error: None,
                },
                Err(e) => OperationResult {
                    operation: i,
                    success: false,
                    error: Some(format!("{:#}", e)),
                },
            });
        }

        Ok(results)
    }

    fn execute_set_geometry(&self, window_str: &str, geometry: &Geometry) -> Result<()> {
        let window = parse_window_id(window_str)?;
        self.set_window_geometry(window, geometry)
    }

    fn execute_set_state(
        &self,
        window_str: &str,
        add_states: &[String],
        remove_states: &[String],
    ) -> Result<()> {
        let window = parse_window_id(window_str)?;

        let add_atoms: Vec<Atom> = add_states
            .iter()
            .filter_map(|name| self.atoms().name_to_atom(name))
            .collect();

        let remove_atoms: Vec<Atom> = remove_states
            .iter()
            .filter_map(|name| self.atoms().name_to_atom(name))
            .collect();

        self.modify_window_state(window, &add_atoms, &remove_atoms)
    }
}

fn parse_window_id(window_str: &str) -> Result<Window> {
    let stripped = window_str
        .strip_prefix("0x")
        .ok_or_else(|| anyhow::anyhow!("Window ID must start with '0x': {}", window_str))?;

    u32::from_str_radix(stripped, 16).with_context(|| format!("Invalid window ID: {}", window_str))
}
