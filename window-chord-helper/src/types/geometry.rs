use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Geometry {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}

/// Window frame extents (decorations)
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Extents {
    pub left: u32,
    pub right: u32,
    pub top: u32,
    pub bottom: u32,
}

impl Extents {
    pub fn zero() -> Self {
        Self {
            left: 0,
            right: 0,
            top: 0,
            bottom: 0,
        }
    }
}

impl From<x11rb::protocol::xproto::GetGeometryReply> for Geometry {
    fn from(reply: x11rb::protocol::xproto::GetGeometryReply) -> Self {
        Self {
            x: reply.x as i32,
            y: reply.y as i32,
            width: reply.width as u32,
            height: reply.height as u32,
        }
    }
}
