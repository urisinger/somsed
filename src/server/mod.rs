use crate::{ExpressionId, Expressions, Vector};
use anyhow::{anyhow, Result};
use desmos_compiler::lang::backends::llvm::jit::ExplicitJitFn;
use desmos_compiler::lang::backends::llvm::CompiledExpr;
use desmos_compiler::lang::backends::llvm::{codegen::compile_all_exprs, CompiledExprs};

use flume::r#async::RecvStream;
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
        points: Vec<Option<Vector>>,
    },
    Error {
        id: ExpressionId,
        message: String,
    },
    Sender(flume::Sender<PointsServerRequest>),
}

pub fn points_server() -> RecvStream<'static, Event> {
    let (equation_tx, equation_rx) = flume::bounded(10);
    let (event_tx, event_rx) = flume::bounded(10);

    thread::spawn(move || {
        let context = inkwell::context::Context::create();
        let mut expressions = Expressions::new();
        let mut compiled_exprs = CompiledExprs::new();
        event_tx.send(Event::Sender(equation_tx)).unwrap();
        while let Ok(request) = equation_rx.recv() {
            match request {
                PointsServerRequest::Compile { id, expr_source } => {
                    if let Err(e) = expressions.insert_expr(id, &expr_source) {
                        event_tx
                            .send(Event::Error {
                                id,
                                message: format!("Failed to parse expression: {}", e),
                            })
                            .unwrap();
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
                PointsServerRequest::Compute {
                    id,
                    range,
                    mid,
                    resolution,
                } => {
                    let compiled = compiled_exprs.compiled.get(&id);
                    let points_result = match compiled {
                        Some(CompiledExpr::Explicit { lhs }) => {
                            points(lhs, range, mid, resolution)
                        }
                        Some(_) => Err(anyhow!("Only explicit expressions are supported")),
                        None => Err(anyhow!("Failed to retrieve compiled expression")),
                    };

                    match points_result {
                        Ok(points) => {
                            event_tx.send(Event::Computed { id, points }).unwrap();
                        }
                        Err(e) => {
                            event_tx
                                .send(Event::Error {
                                    id,
                                    message: format!("Error during computation: {}", e),
                                })
                                .unwrap();
                        }
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
    resolution: u32,
) -> Result<Vec<Option<Vector>>> {
    let mut points = Vec::with_capacity(resolution as usize);

    let min = mid.x - range / 2.0;
    let dx = range / resolution as f32;

    for i in 0..resolution {
        let x = i as f64 * dx as f64 + min as f64;

        let y = unsafe {
            match function {
                ExplicitJitFn::Number(jit_func) => jit_func.call(x),
                ExplicitJitFn::List(_) => {
                    return Err(anyhow!("expected a numeric return value, got list type"))
                }
            }
        };

        let point = if y.is_nan() || y.is_infinite() {
            None
        } else {
            Some(Vector {
                x: x as f32,
                y: y as f32,
            })
        };

        points.push(point);
    }

    Ok(points)
}
