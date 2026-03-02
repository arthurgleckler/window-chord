use anyhow::{Context, Result};
use x11rb::connection::Connection;
use x11rb::protocol::randr::{self, ConnectionExt as _};
use x11rb::protocol::xproto::{ConnectionExt as _, Window};

use super::ewmh::{get_string_property, get_u32_array_property, get_u32_property, get_wm_class};
use super::X11Context;
use crate::types::{Extents, Geometry, Monitor, WindowInfo};

impl X11Context {
    /// Get the active (focused) window ID.
    pub fn get_active_window(&self) -> Result<Option<Window>> {
        let window_id = get_u32_property(self.conn(), self.root(), self.atoms().net_active_window)?;
        Ok(window_id)
    }

    pub fn get_all_windows(&self) -> Result<Vec<Window>> {
        let windows =
            get_u32_array_property(self.conn(), self.root(), self.atoms().net_client_list)?;
        Ok(windows)
    }

    /// Returns frame geometry, not client geometry.
    pub fn get_window_geometry(&self, window: Window) -> Result<Geometry> {
        let geom = self
            .conn()
            .get_geometry(window)?
            .reply()
            .context("Failed to get window geometry")?;

        let translated = self
            .conn()
            .translate_coordinates(window, self.root(), 0, 0)?
            .reply()
            .context("Failed to translate coordinates")?;

        Ok(Geometry {
            x: translated.dst_x as i32,
            y: translated.dst_y as i32,
            width: geom.width as u32,
            height: geom.height as u32,
        })
    }

    /// Try _GTK_FRAME_EXTENTS first, fall back to _NET_FRAME_EXTENTS.
    pub fn get_window_extents(&self, window: Window) -> Result<Extents> {
        if let Ok(extents) =
            get_u32_array_property(self.conn(), window, self.atoms().gtk_frame_extents)
        {
            if extents.len() >= 4 {
                return Ok(Extents {
                    left: extents[0],
                    right: extents[1],
                    top: extents[2],
                    bottom: extents[3],
                });
            }
        }

        if let Ok(extents) =
            get_u32_array_property(self.conn(), window, self.atoms().net_frame_extents)
        {
            if extents.len() >= 4 {
                return Ok(Extents {
                    left: extents[0],
                    right: extents[1],
                    top: extents[2],
                    bottom: extents[3],
                });
            }
        }

        Ok(Extents::zero())
    }

    pub(super) fn get_window_states(&self, window: Window) -> Result<Vec<u32>> {
        get_u32_array_property(self.conn(), window, self.atoms().net_wm_state)
    }

    pub fn get_window_info(&self, window: Window) -> Result<WindowInfo> {
        let geometry = self.get_window_geometry(window)?;
        let extents = self.get_window_extents(window)?;

        let wm_class = get_wm_class(self.conn(), window, self.atoms().wm_class)
            .ok()
            .flatten();

        let wm_name = get_string_property(self.conn(), window, self.atoms().net_wm_name)
            .ok()
            .flatten()
            .or_else(|| {
                get_string_property(self.conn(), window, self.atoms().wm_name)
                    .ok()
                    .flatten()
            });

        let desktop = get_u32_property(self.conn(), window, self.atoms().net_wm_desktop)
            .ok()
            .flatten();

        let state_atoms = self.get_window_states(window)?;
        let states: Vec<String> = state_atoms
            .iter()
            .filter_map(|&atom| self.atoms().atom_to_name(atom))
            .map(|s| s.to_string())
            .collect();

        Ok(WindowInfo {
            id: format!("0x{:x}", window),
            geometry,
            extents,
            wm_class,
            wm_name,
            desktop,
            states,
        })
    }

    /// Batch-query all window information, pipelining X11 requests.
    pub fn get_all_window_info(&self) -> Result<Vec<WindowInfo>> {
        let windows = self.get_all_windows()?;

        let geom_cookies: Vec<_> = windows
            .iter()
            .map(|&w| self.conn().get_geometry(w))
            .collect::<Result<Vec<_>, _>>()?;

        let translate_cookies: Vec<_> = windows
            .iter()
            .map(|&w| self.conn().translate_coordinates(w, self.root(), 0, 0))
            .collect::<Result<Vec<_>, _>>()?;

        let gtk_ext_cookies: Vec<_> = windows
            .iter()
            .map(|&w| {
                self.conn().get_property(
                    false,
                    w,
                    self.atoms().gtk_frame_extents,
                    x11rb::protocol::xproto::AtomEnum::ANY,
                    0,
                    4,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;

        let net_ext_cookies: Vec<_> = windows
            .iter()
            .map(|&w| {
                self.conn().get_property(
                    false,
                    w,
                    self.atoms().net_frame_extents,
                    x11rb::protocol::xproto::AtomEnum::ANY,
                    0,
                    4,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;

        let class_cookies: Vec<_> = windows
            .iter()
            .map(|&w| {
                self.conn().get_property(
                    false,
                    w,
                    self.atoms().wm_class,
                    x11rb::protocol::xproto::AtomEnum::ANY,
                    0,
                    1024,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;

        let name_cookies: Vec<_> = windows
            .iter()
            .map(|&w| {
                self.conn().get_property(
                    false,
                    w,
                    self.atoms().net_wm_name,
                    x11rb::protocol::xproto::AtomEnum::ANY,
                    0,
                    1024,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;

        let desktop_cookies: Vec<_> = windows
            .iter()
            .map(|&w| {
                self.conn().get_property(
                    false,
                    w,
                    self.atoms().net_wm_desktop,
                    x11rb::protocol::xproto::AtomEnum::ANY,
                    0,
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;

        let state_cookies: Vec<_> = windows
            .iter()
            .map(|&w| {
                self.conn().get_property(
                    false,
                    w,
                    self.atoms().net_wm_state,
                    x11rb::protocol::xproto::AtomEnum::ANY,
                    0,
                    u32::MAX,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;

        self.conn().flush()?;

        let geom_replies: Vec<_> = geom_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let translate_replies: Vec<_> = translate_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let gtk_ext_replies: Vec<_> = gtk_ext_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let net_ext_replies: Vec<_> = net_ext_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let class_replies: Vec<_> = class_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let name_replies: Vec<_> = name_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let desktop_replies: Vec<_> = desktop_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let state_replies: Vec<_> = state_cookies
            .into_iter()
            .map(|c| c.reply())
            .collect::<Result<Vec<_>, _>>()?;

        let mut result = Vec::new();
        for (i, &window) in windows.iter().enumerate() {
            let geometry = Geometry {
                x: translate_replies[i].dst_x as i32,
                y: translate_replies[i].dst_y as i32,
                width: geom_replies[i].width as u32,
                height: geom_replies[i].height as u32,
            };

            let extents = {
                let gtk_reply = &gtk_ext_replies[i];
                if gtk_reply.format == 32 && gtk_reply.value.len() >= 16 {
                    let values: Vec<u32> = gtk_reply
                        .value
                        .chunks_exact(4)
                        .take(4)
                        .map(|chunk| {
                            let bytes: [u8; 4] = chunk.try_into().unwrap();
                            u32::from_ne_bytes(bytes)
                        })
                        .collect();
                    Extents {
                        left: values[0],
                        right: values[1],
                        top: values[2],
                        bottom: values[3],
                    }
                } else {
                    let net_reply = &net_ext_replies[i];
                    if net_reply.format == 32 && net_reply.value.len() >= 16 {
                        let values: Vec<u32> = net_reply
                            .value
                            .chunks_exact(4)
                            .take(4)
                            .map(|chunk| {
                                let bytes: [u8; 4] = chunk.try_into().unwrap();
                                u32::from_ne_bytes(bytes)
                            })
                            .collect();
                        Extents {
                            left: values[0],
                            right: values[1],
                            top: values[2],
                            bottom: values[3],
                        }
                    } else {
                        Extents::zero()
                    }
                }
            };

            let wm_class = {
                let reply = &class_replies[i];
                if !reply.value.is_empty() {
                    let parts: Vec<&[u8]> = reply.value.split(|&b| b == 0).collect();
                    if parts.len() >= 2 && !parts[1].is_empty() {
                        Some(String::from_utf8_lossy(parts[1]).into_owned())
                    } else if !parts[0].is_empty() {
                        Some(String::from_utf8_lossy(parts[0]).into_owned())
                    } else {
                        None
                    }
                } else {
                    None
                }
            };

            let wm_name = {
                let reply = &name_replies[i];
                if !reply.value.is_empty() {
                    Some(String::from_utf8_lossy(&reply.value).into_owned())
                } else {
                    None
                }
            };

            let desktop = {
                let reply = &desktop_replies[i];
                if reply.format == 32 && reply.value.len() >= 4 {
                    let bytes: [u8; 4] = reply.value[0..4].try_into().unwrap();
                    Some(u32::from_ne_bytes(bytes))
                } else {
                    None
                }
            };

            let states = {
                let reply = &state_replies[i];
                if reply.format == 32 && !reply.value.is_empty() {
                    let state_atoms: Vec<u32> = reply
                        .value
                        .chunks_exact(4)
                        .map(|chunk| {
                            let bytes: [u8; 4] = chunk.try_into().unwrap();
                            u32::from_ne_bytes(bytes)
                        })
                        .collect();
                    state_atoms
                        .iter()
                        .filter_map(|&atom| self.atoms().atom_to_name(atom))
                        .map(|s| s.to_string())
                        .collect()
                } else {
                    vec![]
                }
            };

            result.push(WindowInfo {
                id: format!("0x{:x}", window),
                geometry,
                extents,
                wm_class,
                wm_name,
                desktop,
                states,
            });
        }

        Ok(result)
    }

    pub fn get_monitors(&self) -> Result<Vec<Monitor>> {
        let root = self.root();

        let resources = randr::get_screen_resources(self.conn(), root)?
            .reply()
            .context("Failed to get screen resources")?;

        let mut monitors = Vec::new();

        for &crtc in &resources.crtcs {
            let crtc_info = randr::get_crtc_info(self.conn(), crtc, resources.config_timestamp)?
                .reply()
                .context("Failed to get CRTC info")?;

            if crtc_info.width == 0 || crtc_info.height == 0 {
                continue;
            }

            let name = if let Some(&output) = crtc_info.outputs.first() {
                let output_info =
                    randr::get_output_info(self.conn(), output, resources.config_timestamp)?
                        .reply()
                        .context("Failed to get output info")?;

                String::from_utf8_lossy(&output_info.name).into_owned()
            } else {
                format!("CRTC-{}", crtc)
            };

            let primary_output = randr::get_output_primary(self.conn(), root)?
                .reply()
                .context("Failed to get primary output")?
                .output;

            let primary = crtc_info.outputs.contains(&primary_output);

            monitors.push(Monitor {
                name,
                geometry: Geometry {
                    x: crtc_info.x as i32,
                    y: crtc_info.y as i32,
                    width: crtc_info.width as u32,
                    height: crtc_info.height as u32,
                },
                primary,
            });
        }

        Ok(monitors)
    }
}
