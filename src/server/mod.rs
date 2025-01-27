use crate::{ExpressionId, Expressions, Somsed};
use anyhow::{anyhow, Result};
use desmos_compiler::lang;
use desmos_compiler::lang::backends::llvm::jit::{ExplicitFn, ExplicitJitFn};
use desmos_compiler::lang::backends::llvm::CompiledExpr;
use desmos_compiler::lang::backends::llvm::{codegen::compile_all_exprs, CompiledExprs};

use flume::r#async::RecvStream;
use iced::Vector;
use lang::value::Value;
use std::collections::HashSet;
use std::thread;

// Define the server requests
#[derive(Debug)]
pub enum PointsServerRequest {
    Resize {
        range: f32,
        mid: Vector,
    },
    Compile {
        id: ExpressionId,
        expr_source: String,
    },
    Remove {
        id: ExpressionId,
    },
}

#[derive(Debug, Clone)]
pub enum ComputationResult {
    Constant(Value),
    Explicit(Vec<Vector>),
    Implicit(Vec<Vector>),
}

#[derive(Debug, Clone)]
pub enum Event {
    Computed {
        id: ExpressionId,
        points: ComputationResult,
    },
    Success {
        id: ExpressionId,
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

        let mut mid = Somsed::DEFAULT_MID;
        let mut range = Somsed::DEFAULT_RANGE;
        loop {
            let mut compute_requests = HashSet::new();

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
                                Err(_) => {
                                    expressions.remove_expr(id);
                                    break;
                                }
                            };
                            continue;
                        }

                        compiled_exprs = compile_all_exprs(&context, &expressions);

                        for &id in expressions.exprs.keys() {
                            if !compiled_exprs.errors.contains_key(&id) {
                                event_tx.send(Event::Success { id }).unwrap();
                            }
                        }

                        for (&id, e) in &compiled_exprs.errors {
                            event_tx
                                .send(Event::Error {
                                    id,
                                    message: format!("Failed to parse expression: {}", e),
                                })
                                .unwrap();
                        }

                        if compiled_exprs.compiled.contains_key(&id) {
                            compute_requests.insert(id);
                        }
                    }
                    PointsServerRequest::Remove { id } => {
                        compiled_exprs.compiled.remove(&id);
                        compiled_exprs.errors.remove(&id);
                    }
                    PointsServerRequest::Resize {
                        range: new_range,
                        mid: new_mid,
                    } => {
                        range = new_range;
                        mid = new_mid;
                        compiled_exprs.compiled.keys().for_each(|id| {
                            compute_requests.insert(id.clone());
                        });
                    }
                }
                request = match equation_rx.try_recv() {
                    Ok(request) => request,
                    Err(_) => break,
                };
            }

            for &id in compute_requests.iter() {
                let compiled = compiled_exprs.compiled.get(&id);
                let points_result = match compiled {
                    Some(CompiledExpr::Explicit { lhs }) => match lhs {
                        ExplicitJitFn::Number(lhs) => {
                            points_explicit(lhs, range, mid, 9, 14).map(ComputationResult::Explicit)
                        }
                        ExplicitJitFn::Point(_) => todo!(),
                        ExplicitJitFn::List(_) => todo!(),
                    },
                    Some(CompiledExpr::Constant { value }) => {
                        value.clone().try_into().map(ComputationResult::Constant)
                    }
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

                if !equation_rx.is_empty() {
                    continue;
                }
            }
        }
    });

    event_rx.into_stream()
}

pub fn points_explicit(
    function: &ExplicitFn<f64>,
    range: f32,
    mid: Vector,
    min_depth: u32,
    max_depth: u32,
) -> Result<Vec<Vector>> {
    let mut points = Vec::new();

    let min_x = (mid.x - range / 2.0) as f32;

    let min = Vector::new(min_x, unsafe { function.call(min_x as f64) } as f32);

    let max_x = min_x + range;
    let max = Vector::new(max_x, unsafe { function.call(max_x as f64) } as f32);

    points.push(min);

    subdivide(function, min, max, 0, min_depth, max_depth, &mut points)?;

    points.push(max);

    Ok(points)
}

fn subdivide(
    function: &ExplicitFn<f64>,
    min: Vector,
    max: Vector,
    depth: u32,
    min_depth: u32,
    max_depth: u32,
    points: &mut Vec<Vector>,
) -> Result<()> {
    let x_mid = (min.x + max.x) / 2.0;

    let mid = Vector::new(x_mid, unsafe { function.call(x_mid as f64) } as f32);

    // Check if subdivision is necessary
    if depth <= max_depth && (depth < min_depth || should_descend(min, mid, max)) {
        subdivide(function, min, mid, depth + 1, min_depth, max_depth, points)?;
        subdivide(function, mid, max, depth + 1, min_depth, max_depth, points)?;
    } else {
        points.push(mid);
    }

    Ok(())
}

fn should_descend(min: Vector, mid: Vector, max: Vector) -> bool {
    if min.y.is_nan() && mid.y.is_nan() && max.y.is_nan() {
        // Entirely in an undefined region
        return false;
    }

    if min.y.is_nan() || mid.y.is_nan() || max.y.is_nan() {
        // Straddling defined and undefined regions
        return true;
    }

    // Calculate gradients for left and right intervals
    let x_step = (max.x - min.x) / 2.0; // Half the interval for gradient calculation
    let grad_left = ((mid.y - min.y) / x_step).abs();
    let grad_right = ((max.y - mid.y) / x_step).abs();

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
    let relative_change_left = ((mid.y - min.y) / min.y.abs().max(1e-10)).abs();
    let relative_change_right = ((max.y - mid.y) / mid.y.abs().max(1e-10)).abs();

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
