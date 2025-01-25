use crate::{ExpressionId, Expressions};
use anyhow::{anyhow, Result};
use desmos_compiler::lang::backends::llvm::jit::{ExplicitJitFn, JitValue};
use desmos_compiler::lang::backends::llvm::CompiledExpr;
use desmos_compiler::lang::backends::llvm::{codegen::compile_all_exprs, CompiledExprs};

use flume::r#async::RecvStream;
use iced::Vector;
use std::collections::HashMap;
use std::thread;

// Define the server requests
#[derive(Debug)]
pub enum PointsServerRequest {
    Compute {
        id: ExpressionId,
        range: f32,
        mid: Vector,
        resolution: u32,
    },
    Compile {
        id: ExpressionId,
        expr_source: String,
    },
}

#[derive(Debug, Clone)]
pub enum Event {
    Computed {
        id: ExpressionId,
        points: Vec<Vector>,
    },
    Error {
        id: ExpressionId,
        message: String,
    },
    Sender(flume::Sender<PointsServerRequest>),
}

pub fn points_server() -> RecvStream<'static, Event> {
    let (equation_tx, equation_rx) = flume::unbounded();
    let (event_tx, event_rx) = flume::unbounded();

    thread::spawn(move || {
        let context = inkwell::context::Context::create();
        let mut expressions = Expressions::new();
        let mut compiled_exprs = CompiledExprs::new();
        event_tx.send(Event::Sender(equation_tx)).unwrap();

        loop {
            let mut compute_requests = HashMap::new();

            let mut request = equation_rx.recv().unwrap();
            loop {
                match request {
                    PointsServerRequest::Compile {
                        id,
                        ref expr_source,
                    } => {
                        if let Err(e) = expressions.insert_expr(id, expr_source) {
                            event_tx
                                .send(Event::Error {
                                    id,
                                    message: format!("Failed to parse expression: {}", e),
                                })
                                .unwrap();
                            request = match equation_rx.try_recv() {
                                Ok(request) => request,
                                Err(_) => break,
                            };
                            continue;
                        }

                        compiled_exprs = compile_all_exprs(&context, &expressions);

                        for (&id, e) in &compiled_exprs.errors {
                            event_tx
                                .send(Event::Error {
                                    id,
                                    message: format!("Failed to parse expression: {}", e),
                                })
                                .unwrap();
                        }
                    }
                    PointsServerRequest::Compute { id, range, mid, .. } => {
                        compute_requests.insert(id, Some((range, mid)));
                    }
                }
                request = match equation_rx.try_recv() {
                    Ok(request) => request,
                    Err(_) => break,
                };
            }

            for (&id, (range, mid)) in compute_requests
                .iter()
                .filter_map(|(id, request)| request.map(|v| (id, v)))
            {
                let compiled = compiled_exprs.compiled.get(&id);
                let points_result = match compiled {
                    Some(CompiledExpr::Explicit { lhs }) => points(lhs, range, mid, 9, 13),
                    Some(_) => Err(anyhow!("Only explicit expressions are supported")),
                    None => Err(anyhow!("Failed to retrieve compiled expression")),
                };

                match points_result {
                    Ok(points) => {
                        event_tx.send(Event::Computed { id, points }).unwrap();
                    }
                    Err(e) => {
                        let message = if let Some(e) = compiled_exprs.errors.get(&id) {
                            format!("Error during computation: {}", e)
                        } else {
                            format!("Error during computation: {}", e)
                        };
                        event_tx.send(Event::Error { id, message }).unwrap();
                    }
                }
            }
        }
    });

    event_rx.into_stream()
}

pub fn points(
    function: &ExplicitJitFn,
    range: f32,
    mid: Vector,
    min_depth: u32,
    max_depth: u32,
) -> Result<Vec<Vector>> {
    let mut points = Vec::new();

    let x_min = (mid.x - range / 2.0) as f64;

    let y_min = match unsafe { function.call(x_min) } {
        JitValue::Number(num) => num,
        JitValue::List(_) => return Err(anyhow!("expected a numeric return value, got list type")),
    };

    let x_max = x_min + range as f64;
    let y_max = match unsafe { function.call(x_max) } {
        JitValue::Number(num) => num,
        JitValue::List(_) => return Err(anyhow!("expected a numeric return value, got list type")),
    };

    points.push(Vector {
        x: x_min as f32,
        y: y_min as f32,
    });

    subdivide(
        function,
        x_min,
        y_min,
        x_max,
        y_max,
        0,
        min_depth,
        max_depth,
        &mut points,
    )?;

    points.push(Vector {
        x: x_max as f32,
        y: y_max as f32,
    });

    Ok(points)
}

fn subdivide(
    function: &ExplicitJitFn,
    x_min: f64,
    y_min: f64,
    x_max: f64,
    y_max: f64,
    depth: u32,
    min_depth: u32,
    max_depth: u32,
    points: &mut Vec<Vector>,
) -> Result<()> {
    let x_mid = (x_min + x_max) / 2.0;

    let y_mid = match unsafe { function.call(x_mid) } {
        JitValue::Number(num) => num,
        JitValue::List(_) => return Err(anyhow!("expected a numeric return value, got list type")),
    };

    // Check if subdivision is necessary
    if depth <= max_depth
        && (depth < min_depth || should_descend(x_min, x_max, y_min, y_mid, y_max))
    {
        subdivide(
            function,
            x_min,
            y_min,
            x_mid,
            y_mid,
            depth + 1,
            min_depth,
            max_depth,
            points,
        )?;
        subdivide(
            function,
            x_mid,
            y_mid,
            x_max,
            y_max,
            depth + 1,
            min_depth,
            max_depth,
            points,
        )?;
    } else {
        points.push(Vector {
            x: x_mid as f32,
            y: y_mid as f32,
        });
    }

    Ok(())
}

fn should_descend(x_min: f64, x_max: f64, y_min: f64, y_mid: f64, y_max: f64) -> bool {
    if y_min.is_nan() && y_mid.is_nan() && y_max.is_nan() {
        // Entirely in an undefined region
        return false;
    }

    if y_min.is_nan() || y_mid.is_nan() || y_max.is_nan() {
        // Straddling defined and undefined regions
        return true;
    }

    // Calculate gradients for left and right intervals
    let x_step = (x_max - x_min) / 2.0; // Half the interval for gradient calculation
    let grad_left = ((y_mid - y_min) / x_step).abs();
    let grad_right = ((y_max - y_mid) / x_step).abs();

    if grad_left > 5.0
        || grad_right > 5.0
        || grad_right.is_infinite()
        || grad_left.is_infinite()
        || grad_left.is_nan()
        || grad_right.is_nan()
    {
        // High gradient implies rapid change
        return true;
    }

    // Check for relative differences (peaks/troughs)
    let relative_change_left = ((y_mid - y_min) / y_min.abs().max(1e-10)).abs();
    let relative_change_right = ((y_max - y_mid) / y_mid.abs().max(1e-10)).abs();

    if relative_change_left > 0.1
        || relative_change_right > 0.1
        || relative_change_left.is_infinite()
        || relative_change_right.is_infinite()
        || relative_change_left.is_nan()
        || relative_change_right.is_nan()
    {
        // Significant relative change implies oscillation or peak
        return true;
    }

    // Default: Do not descend
    false
}
