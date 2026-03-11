/// waylos — Wayland Launch Or Select
///
/// Usage: waylos <app_id> [launch_command...]
///
/// Activates a toplevel whose app_id contains <app_id> (case-insensitive).
/// If there are multiple matches, cycles through them on repeated
/// invocations.  If no match is found and a launch command is given, runs
/// it in the background.  Exit 0 on success, 1 if no match and no command,
/// 2 on error.

use cosmic_protocols::toplevel_info::v1::client::{
    zcosmic_toplevel_handle_v1, zcosmic_toplevel_info_v1,
};
use cosmic_protocols::toplevel_management::v1::client::zcosmic_toplevel_manager_v1;
use std::process;
use wayland_client::protocol::{wl_registry, wl_seat};
use wayland_client::{Connection, Dispatch, QueueHandle, event_created_child};
use wayland_protocols::ext::foreign_toplevel_list::v1::client::{
    ext_foreign_toplevel_handle_v1, ext_foreign_toplevel_list_v1,
};

struct State {
    info: Option<zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1>,
    manager: Option<zcosmic_toplevel_manager_v1::ZcosmicToplevelManagerV1>,
    seat: Option<wl_seat::WlSeat>,
    toplevels: Vec<Window>,
}

struct Window {
    foreign: ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
    cosmic: Option<zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1>,
    app_id: Option<String>,
    identifier: Option<String>,
}

// --- Registry ---

impl Dispatch<wl_registry::WlRegistry, ()> for State {
    fn event(
        state: &mut Self,
        proxy: &wl_registry::WlRegistry,
        event: wl_registry::Event,
        _: &(),
        _: &Connection,
        qh: &QueueHandle<Self>,
    ) {
        if let wl_registry::Event::Global {
            name,
            interface,
            version,
        } = event
        {
            match interface.as_str() {
                "wl_seat" if state.seat.is_none() => {
                    state.seat =
                        Some(proxy.bind::<wl_seat::WlSeat, _, _>(name, version.min(9), qh, ()));
                }
                "ext_foreign_toplevel_list_v1" => {
                    proxy
                        .bind::<ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1, _, _>(
                            name,
                            version.min(1),
                            qh,
                            (),
                        );
                }
                "zcosmic_toplevel_info_v1" => {
                    state.info = Some(
                        proxy.bind::<zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1, _, _>(
                            name,
                            version.min(2),
                            qh,
                            (),
                        ),
                    );
                }
                "zcosmic_toplevel_manager_v1" => {
                    state.manager = Some(
                        proxy.bind::<zcosmic_toplevel_manager_v1::ZcosmicToplevelManagerV1, _, _>(
                            name, version, qh, (),
                        ),
                    );
                }
                _ => {}
            }
        }
    }
}

// --- Seat ---

impl Dispatch<wl_seat::WlSeat, ()> for State {
    fn event(
        _: &mut Self,
        _: &wl_seat::WlSeat,
        _: wl_seat::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

// --- Foreign toplevel list ---

impl Dispatch<ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1, ()> for State {
    fn event(
        state: &mut Self,
        _: &ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1,
        event: ext_foreign_toplevel_list_v1::Event,
        _: &(),
        _: &Connection,
        qh: &QueueHandle<Self>,
    ) {
        if let ext_foreign_toplevel_list_v1::Event::Toplevel { toplevel } = event {
            // Wrap the foreign handle into a cosmic handle for activation.
            let cosmic = state.info.as_ref().map(|info| {
                info.get_cosmic_toplevel(&toplevel, qh, ())
            });
            state.toplevels.push(Window {
                foreign: toplevel,
                cosmic,
                app_id: None,
                identifier: None,
            });
        }
    }

    event_created_child!(
        State,
        ext_foreign_toplevel_list_v1::ExtForeignToplevelListV1,
        [
            ext_foreign_toplevel_list_v1::EVT_TOPLEVEL_OPCODE =>
                (ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1, ()),
        ]
    );
}

// --- Foreign toplevel handle ---

impl Dispatch<ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1, ()> for State {
    fn event(
        state: &mut Self,
        handle: &ext_foreign_toplevel_handle_v1::ExtForeignToplevelHandleV1,
        event: ext_foreign_toplevel_handle_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
        match event {
            ext_foreign_toplevel_handle_v1::Event::AppId { app_id } => {
                if let Some(w) = state.toplevels.iter_mut().find(|w| &w.foreign == handle) {
                    w.app_id = Some(app_id);
                }
            }
            ext_foreign_toplevel_handle_v1::Event::Identifier { identifier } => {
                if let Some(w) = state.toplevels.iter_mut().find(|w| &w.foreign == handle) {
                    w.identifier = Some(identifier);
                }
            }
            _ => {}
        }
    }
}

// --- Cosmic toplevel info ---

impl Dispatch<zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1, ()> for State {
    fn event(
        _: &mut Self,
        _: &zcosmic_toplevel_info_v1::ZcosmicToplevelInfoV1,
        _: zcosmic_toplevel_info_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

// --- Cosmic toplevel handle ---

impl Dispatch<zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1, ()> for State {
    fn event(
        _: &mut Self,
        _: &zcosmic_toplevel_handle_v1::ZcosmicToplevelHandleV1,
        _: zcosmic_toplevel_handle_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

// --- Cosmic toplevel manager ---

impl Dispatch<zcosmic_toplevel_manager_v1::ZcosmicToplevelManagerV1, ()> for State {
    fn event(
        _: &mut Self,
        _: &zcosmic_toplevel_manager_v1::ZcosmicToplevelManagerV1,
        _: zcosmic_toplevel_manager_v1::Event,
        _: &(),
        _: &Connection,
        _: &QueueHandle<Self>,
    ) {
    }
}

fn state_path(target: &str) -> std::path::PathBuf {
    let dir = std::env::var("XDG_RUNTIME_DIR")
        .unwrap_or_else(|_| "/tmp".into());
    std::path::PathBuf::from(dir).join(format!("waylos-{target}.last"))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: waylos <app_id> [launch_command...]");
        process::exit(2);
    }
    let target = args[1].to_lowercase();
    let launch_cmd = &args[2..];

    let conn = match Connection::connect_to_env() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("waylos: cannot connect to compositor: {e}");
            process::exit(2);
        }
    };
    let mut eq = conn.new_event_queue();
    let qh = eq.handle();
    let mut state = State {
        info: None,
        manager: None,
        seat: None,
        toplevels: Vec::new(),
    };

    conn.display().get_registry(&qh, ());
    eq.roundtrip(&mut state).expect("roundtrip 1");
    eq.roundtrip(&mut state).expect("roundtrip 2");

    let debug = std::env::var("WAYLOS_DEBUG").is_ok();
    if debug {
        eprintln!(
            "waylos: info={} manager={} seat={} toplevels={}",
            state.info.is_some(),
            state.manager.is_some(),
            state.seat.is_some(),
            state.toplevels.len()
        );
        for w in &state.toplevels {
            eprintln!("  app_id={:?} id={:?}", w.app_id, w.identifier);
        }
    }

    let matches: Vec<_> = state
        .toplevels
        .iter()
        .filter(|w| {
            w.app_id
                .as_ref()
                .map(|id| id.to_lowercase().contains(&target))
                .unwrap_or(false)
        })
        .collect();

    // Cycle: read the last-activated identifier and pick the next one.
    let last = std::fs::read_to_string(state_path(&target)).ok();
    let matched = if matches.len() > 1 {
        if let Some(last_id) = &last {
            let last_id = last_id.trim();
            if let Some(pos) = matches.iter().position(|w| {
                w.identifier.as_deref() == Some(last_id)
            }) {
                Some(matches[(pos + 1) % matches.len()])
            } else {
                matches.first().copied()
            }
        } else {
            matches.first().copied()
        }
    } else {
        matches.first().copied()
    };

    if debug {
        eprintln!("waylos: last={last:?} matched_id={:?}", matched.and_then(|w| w.identifier.as_deref()));
    }

    match matched {
        Some(w) => {
            let manager = state.manager.as_ref().unwrap_or_else(|| {
                eprintln!("waylos: compositor lacks toplevel management");
                process::exit(2)
            });
            let cosmic = w.cosmic.as_ref().unwrap_or_else(|| {
                eprintln!("waylos: no cosmic handle for toplevel");
                process::exit(2)
            });
            let seat = state.seat.as_ref().unwrap_or_else(|| {
                eprintln!("waylos: no seat");
                process::exit(2)
            });
            manager.activate(cosmic, seat);
            conn.flush().expect("flush");
            // Remember which window we activated for cycling.
            if let Some(id) = &w.identifier {
                let _ = std::fs::write(state_path(&target), id);
            }
        }
        None if !launch_cmd.is_empty() => {
            process::Command::new(&launch_cmd[0])
                .args(&launch_cmd[1..])
                .stdin(process::Stdio::null())
                .stdout(process::Stdio::null())
                .stderr(process::Stdio::null())
                .spawn()
                .unwrap_or_else(|e| {
                    eprintln!("waylos: failed to launch {}: {e}", launch_cmd[0]);
                    process::exit(2);
                });
        }
        None => {
            let config_dir = std::env::var("XDG_CONFIG_HOME")
                .map(std::path::PathBuf::from)
                .unwrap_or_else(|_| {
                    std::path::PathBuf::from(std::env::var("HOME").unwrap_or_default())
                        .join(".config")
                });
            let config_path = config_dir.join("waylos/commands.json");
            if let Ok(data) = std::fs::read_to_string(&config_path) {
                if let Ok(map) = serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(&data) {
                    if let Some(serde_json::Value::Array(arr)) = map.get(&args[1]) {
                        let cmd: Vec<&str> = arr.iter().filter_map(|v| v.as_str()).collect();
                        if !cmd.is_empty() {
                            process::Command::new(cmd[0])
                                .args(&cmd[1..])
                                .stdin(process::Stdio::null())
                                .stdout(process::Stdio::null())
                                .stderr(process::Stdio::null())
                                .spawn()
                                .unwrap_or_else(|e| {
                                    eprintln!("waylos: failed to launch '{}': {e}", cmd[0]);
                                    process::exit(2);
                                });
                            return;
                        }
                    }
                }
            }
            eprintln!("waylos: no window matching '{}'", args[1]);
            process::exit(1);
        }
    }
}
