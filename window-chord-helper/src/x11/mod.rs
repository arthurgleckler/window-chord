use anyhow::{Context, Result};
use x11rb::connection::Connection;
use x11rb::protocol::xproto::{Screen, Window};
use x11rb::rust_connection::RustConnection;

pub mod act;
pub mod ewmh;
pub mod query;

pub use ewmh::EwmhAtoms;

pub struct X11Context {
    conn: RustConnection,
    screen_num: usize,
    root: Window,
    atoms: EwmhAtoms,
}

impl X11Context {
    pub fn new() -> Result<Self> {
        let (conn, screen_num) =
            RustConnection::connect(None).context("Failed to connect to X11 server")?;

        let screen = &conn.setup().roots[screen_num];
        let root = screen.root;

        let atoms = EwmhAtoms::intern(&conn).context("Failed to intern EWMH atoms")?;

        Ok(Self {
            conn,
            screen_num,
            root,
            atoms,
        })
    }

    pub fn conn(&self) -> &RustConnection {
        &self.conn
    }

    pub fn root(&self) -> Window {
        self.root
    }

    pub fn screen(&self) -> &Screen {
        &self.conn.setup().roots[self.screen_num]
    }

    pub fn atoms(&self) -> &EwmhAtoms {
        &self.atoms
    }
}
