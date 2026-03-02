use super::geometry::{Extents, Geometry};
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
#[serde(tag = "command")]
pub enum Command {
    #[serde(rename = "query")]
    Query(QueryCommand),
    #[serde(rename = "act")]
    Act(ActCommand),
}

#[derive(Debug, Deserialize)]
pub struct QueryCommand {
    #[serde(default)]
    pub include: QueryInclude,
}

#[derive(Debug, Default, Deserialize)]
pub struct QueryInclude {
    #[serde(default)]
    pub active_window: bool,
    #[serde(default)]
    pub all_windows: bool,
    #[serde(default)]
    pub monitors: bool,
    #[serde(default)]
    pub window_details: Option<Vec<String>>,
}

#[derive(Debug, Serialize)]
pub struct QueryResponse {
    pub status: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub active_window: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub monitors: Option<Vec<Monitor>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub windows: Option<Vec<WindowInfo>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct Monitor {
    pub name: String,
    pub geometry: Geometry,
    pub primary: bool,
}

#[derive(Debug, Serialize)]
pub struct WindowInfo {
    pub id: String,
    pub geometry: Geometry,
    pub extents: Extents,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wm_class: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wm_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub desktop: Option<u32>,
    pub states: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct ActCommand {
    pub operations: Vec<Operation>,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
pub enum Operation {
    #[serde(rename = "set_window_geometry")]
    SetWindowGeometry { window: String, geometry: Geometry },
    #[serde(rename = "set_window_state")]
    SetWindowState {
        window: String,
        #[serde(default)]
        add_states: Vec<String>,
        #[serde(default)]
        remove_states: Vec<String>,
    },
    #[serde(rename = "move_window")]
    MoveWindow { window: String, geometry: Geometry },
}

#[derive(Debug, Serialize)]
pub struct ActResponse {
    pub status: String,
    pub results: Vec<OperationResult>,
}

#[derive(Debug, Serialize)]
pub struct OperationResult {
    pub operation: usize,
    pub success: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}
