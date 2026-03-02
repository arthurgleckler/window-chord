use anyhow::{Context, Result};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{Atom, AtomEnum, ConnectionExt as _, Window};

/// All EWMH atoms we need, interned once at startup.
#[derive(Debug, Clone)]
pub struct EwmhAtoms {
    // Root window properties
    pub net_active_window: Atom,
    pub net_client_list: Atom,
    pub net_client_list_stacking: Atom,
    pub net_number_of_desktops: Atom,
    pub net_current_desktop: Atom,

    // Window properties
    pub net_wm_state: Atom,
    pub net_frame_extents: Atom,
    pub gtk_frame_extents: Atom,
    pub net_wm_name: Atom,
    pub net_wm_desktop: Atom,

    // Window state atoms
    pub net_wm_state_fullscreen: Atom,
    pub net_wm_state_maximized_horz: Atom,
    pub net_wm_state_maximized_vert: Atom,
    pub net_wm_state_hidden: Atom,
    pub net_wm_state_modal: Atom,
    pub net_wm_state_sticky: Atom,
    pub net_wm_state_above: Atom,
    pub net_wm_state_below: Atom,

    // ICCCM properties
    pub wm_class: Atom,
    pub wm_name: Atom,

    // Action atoms
    pub net_moveresize_window: Atom,

    // Utility atoms
    pub utf8_string: Atom,
}

impl EwmhAtoms {
    pub fn intern(conn: &impl Connection) -> Result<Self> {
        let atom_names = [
            // Root window properties
            "_NET_ACTIVE_WINDOW",
            "_NET_CLIENT_LIST",
            "_NET_CLIENT_LIST_STACKING",
            "_NET_NUMBER_OF_DESKTOPS",
            "_NET_CURRENT_DESKTOP",
            // Window properties
            "_NET_WM_STATE",
            "_NET_FRAME_EXTENTS",
            "_GTK_FRAME_EXTENTS",
            "_NET_WM_NAME",
            "_NET_WM_DESKTOP",
            // Window states
            "_NET_WM_STATE_FULLSCREEN",
            "_NET_WM_STATE_MAXIMIZED_HORZ",
            "_NET_WM_STATE_MAXIMIZED_VERT",
            "_NET_WM_STATE_HIDDEN",
            "_NET_WM_STATE_MODAL",
            "_NET_WM_STATE_STICKY",
            "_NET_WM_STATE_ABOVE",
            "_NET_WM_STATE_BELOW",
            // ICCCM
            "WM_CLASS",
            "WM_NAME",
            // Action
            "_NET_MOVERESIZE_WINDOW",
            // Utility
            "UTF8_STRING",
        ];

        let cookies: Vec<_> = atom_names
            .iter()
            .map(|name| conn.intern_atom(false, name.as_bytes()))
            .collect::<Result<Vec<_>, _>>()?;

        conn.flush()?;

        let atoms: Vec<Atom> = cookies
            .into_iter()
            .enumerate()
            .map(|(i, cookie)| {
                cookie
                    .reply()
                    .with_context(|| format!("Failed to intern atom: {}", atom_names[i]))
                    .map(|reply| reply.atom)
            })
            .collect::<Result<Vec<_>>>()?;

        // Order must match atom_names array above.
        Ok(Self {
            net_active_window: atoms[0],
            net_client_list: atoms[1],
            net_client_list_stacking: atoms[2],
            net_number_of_desktops: atoms[3],
            net_current_desktop: atoms[4],
            net_wm_state: atoms[5],
            net_frame_extents: atoms[6],
            gtk_frame_extents: atoms[7],
            net_wm_name: atoms[8],
            net_wm_desktop: atoms[9],
            net_wm_state_fullscreen: atoms[10],
            net_wm_state_maximized_horz: atoms[11],
            net_wm_state_maximized_vert: atoms[12],
            net_wm_state_hidden: atoms[13],
            net_wm_state_modal: atoms[14],
            net_wm_state_sticky: atoms[15],
            net_wm_state_above: atoms[16],
            net_wm_state_below: atoms[17],
            wm_class: atoms[18],
            wm_name: atoms[19],
            net_moveresize_window: atoms[20],
            utf8_string: atoms[21],
        })
    }

    pub fn atom_to_name(&self, atom: Atom) -> Option<&'static str> {
        match atom {
            a if a == self.net_wm_state_fullscreen => Some("_NET_WM_STATE_FULLSCREEN"),
            a if a == self.net_wm_state_maximized_horz => Some("_NET_WM_STATE_MAXIMIZED_HORZ"),
            a if a == self.net_wm_state_maximized_vert => Some("_NET_WM_STATE_MAXIMIZED_VERT"),
            a if a == self.net_wm_state_hidden => Some("_NET_WM_STATE_HIDDEN"),
            a if a == self.net_wm_state_modal => Some("_NET_WM_STATE_MODAL"),
            a if a == self.net_wm_state_sticky => Some("_NET_WM_STATE_STICKY"),
            a if a == self.net_wm_state_above => Some("_NET_WM_STATE_ABOVE"),
            a if a == self.net_wm_state_below => Some("_NET_WM_STATE_BELOW"),
            _ => None,
        }
    }

    pub fn name_to_atom(&self, name: &str) -> Option<Atom> {
        match name {
            "_NET_WM_STATE_FULLSCREEN" => Some(self.net_wm_state_fullscreen),
            "_NET_WM_STATE_MAXIMIZED_HORZ" => Some(self.net_wm_state_maximized_horz),
            "_NET_WM_STATE_MAXIMIZED_VERT" => Some(self.net_wm_state_maximized_vert),
            "_NET_WM_STATE_HIDDEN" => Some(self.net_wm_state_hidden),
            "_NET_WM_STATE_MODAL" => Some(self.net_wm_state_modal),
            "_NET_WM_STATE_STICKY" => Some(self.net_wm_state_sticky),
            "_NET_WM_STATE_ABOVE" => Some(self.net_wm_state_above),
            "_NET_WM_STATE_BELOW" => Some(self.net_wm_state_below),
            _ => None,
        }
    }
}

pub fn get_u32_property(
    conn: &impl Connection,
    window: Window,
    property: Atom,
) -> Result<Option<u32>> {
    let reply = conn
        .get_property(false, window, property, AtomEnum::ANY, 0, 1)?
        .reply()?;

    if reply.value.is_empty() {
        return Ok(None);
    }

    if reply.format == 32 && reply.value.len() >= 4 {
        let bytes: [u8; 4] = reply.value[0..4].try_into()?;
        Ok(Some(u32::from_ne_bytes(bytes)))
    } else {
        Ok(None)
    }
}

pub fn get_u32_array_property(
    conn: &impl Connection,
    window: Window,
    property: Atom,
) -> Result<Vec<u32>> {
    let reply = conn
        .get_property(false, window, property, AtomEnum::ANY, 0, u32::MAX)?
        .reply()?;

    if reply.value.is_empty() || reply.format != 32 {
        return Ok(Vec::new());
    }

    let values = reply
        .value
        .chunks_exact(4)
        .map(|chunk| {
            let bytes: [u8; 4] = chunk.try_into().unwrap();
            u32::from_ne_bytes(bytes)
        })
        .collect();

    Ok(values)
}

pub fn get_string_property(
    conn: &impl Connection,
    window: Window,
    property: Atom,
) -> Result<Option<String>> {
    let reply = conn
        .get_property(false, window, property, AtomEnum::ANY, 0, 1024)?
        .reply()?;

    if reply.value.is_empty() {
        return Ok(None);
    }

    match String::from_utf8(reply.value) {
        Ok(s) => Ok(Some(s)),
        Err(e) => Ok(Some(String::from_utf8_lossy(&e.into_bytes()).into_owned())),
    }
}

/// Returns the class part of WM_CLASS (format: "instance\0class\0").
pub fn get_wm_class(
    conn: &impl Connection,
    window: Window,
    wm_class_atom: Atom,
) -> Result<Option<String>> {
    let reply = conn
        .get_property(false, window, wm_class_atom, AtomEnum::ANY, 0, 1024)?
        .reply()?;

    if reply.value.is_empty() {
        return Ok(None);
    }

    let parts: Vec<&[u8]> = reply.value.split(|&b| b == 0).collect();
    if parts.len() >= 2 && !parts[1].is_empty() {
        Ok(Some(String::from_utf8_lossy(parts[1]).into_owned()))
    } else if !parts[0].is_empty() {
        Ok(Some(String::from_utf8_lossy(parts[0]).into_owned()))
    } else {
        Ok(None)
    }
}
