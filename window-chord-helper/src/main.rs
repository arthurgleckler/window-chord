use anyhow::{Context, Result};
use std::io::{self, BufRead, Write};

mod types;
mod x11;

use types::commands::{ActResponse, Command, QueryResponse};
use x11::X11Context;

fn main() -> Result<()> {
    let ctx = X11Context::new().context("Failed to initialize X11 connection")?;

    let stdin = io::stdin().lock();
    let mut stdout = io::stdout().lock();

    for line in stdin.lines() {
        let line = line.context("Failed to read from stdin")?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let response = match serde_json::from_str::<Command>(line) {
            Ok(command) => match command {
                Command::Query(q) => serde_json::to_string(&handle_query(&ctx, q))
                    .unwrap_or_else(|e| format!(r#"{{"status":"error","error":"{}"}}"#, e)),
                Command::Act(a) => serde_json::to_string(&handle_act(&ctx, a))
                    .unwrap_or_else(|e| format!(r#"{{"status":"error","error":"{}"}}"#, e)),
            },
            Err(e) => {
                format!(r#"{{"status":"error","error":"Parse error: {}"}}"#, e)
            }
        };

        writeln!(stdout, "{}", response).context("Failed to write to stdout")?;
        stdout.flush().context("Failed to flush stdout")?;
    }

    Ok(())
}

fn handle_query(ctx: &X11Context, query: types::commands::QueryCommand) -> QueryResponse {
    let include = query.include;

    let active_window = if include.active_window {
        match ctx.get_active_window() {
            Ok(w) => w.map(|w| format!("0x{:x}", w)),
            Err(_) => None,
        }
    } else {
        None
    };

    let monitors = if include.monitors {
        ctx.get_monitors().ok()
    } else {
        None
    };

    let windows = if include.all_windows {
        ctx.get_all_window_info().ok()
    } else if let Some(window_ids) = include.window_details {
        let mut infos = Vec::new();
        for id_str in window_ids {
            if let Some(stripped) = id_str.strip_prefix("0x") {
                if let Ok(window_id) = u32::from_str_radix(stripped, 16) {
                    if let Ok(info) = ctx.get_window_info(window_id) {
                        infos.push(info);
                    }
                }
            }
        }
        Some(infos)
    } else {
        None
    };

    QueryResponse {
        status: "success".to_string(),
        active_window,
        monitors,
        windows,
        error: None,
    }
}

fn handle_act(ctx: &X11Context, act: types::commands::ActCommand) -> ActResponse {
    match ctx.execute_operations(&act.operations) {
        Ok(results) => {
            let all_success = results.iter().all(|r| r.success);
            ActResponse {
                status: if all_success { "success" } else { "partial" }.to_string(),
                results,
            }
        }
        Err(e) => ActResponse {
            status: "error".to_string(),
            results: vec![types::commands::OperationResult {
                operation: 0,
                success: false,
                error: Some(format!("{:#}", e)),
            }],
        },
    }
}
