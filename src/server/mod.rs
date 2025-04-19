use crate::{ExpressionId, Expressions, Somsed};
use anyhow::{anyhow, Context, Result};
use desmos_compiler::lang::codegen::backend::cranelift::CraneliftBackend;
use desmos_compiler::lang::codegen::backend::jit::{ExplicitFn, ExplicitJitFn, JitValue};
use desmos_compiler::lang::codegen::backend::ExecutionEngine;
use desmos_compiler::lang::codegen::ir::{IRType, SegmentKey};
use desmos_compiler::lang::codegen::IRGen;
use desmos_compiler::lang::expr::{Expr, ResolvedExpr};
use flume::r#async::RecvStream;
use iced::Vector;
use std::collections::{HashMap, HashSet};
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
    Constant(JitValue),
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
        let mut expressions = Expressions::new();

        event_tx.send(Event::Sender(equation_tx)).unwrap();

        let mut mid = Somsed::DEFAULT_MID;
        let mut range = Somsed::DEFAULT_RANGE;

        let mut backend = None;
        let mut errors = HashMap::new();

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

                        let ir;
                        (ir, errors) = IRGen::generate_ir(&expressions);
                        backend = Some(CraneliftBackend::new().unwrap());

                        backend
                            .as_mut()
                            .unwrap()
                            .compile_module(&ir)
                            .expect("compilation error, restarting server");

                        for &id in expressions.exprs.keys() {
                            if !errors.contains_key(&id) {
                                event_tx.send(Event::Success { id }).unwrap();
                            }
                        }

                        for (&id, e) in &errors {
                            event_tx
                                .send(Event::Error {
                                    id,
                                    message: format!("Error during compilation: {}", e),
                                })
                                .unwrap();
                        }
                        compute_requests.insert(id);
                    }
                    PointsServerRequest::Remove { id } => {
                        expressions.exprs.remove(&id);
                    }
                    PointsServerRequest::Resize {
                        range: new_range,
                        mid: new_mid,
                    } => {
                        range = new_range;
                        mid = new_mid;
                        expressions.exprs.keys().for_each(|id| {
                            compute_requests.insert(*id);
                        });
                    }
                }
                request = match equation_rx.try_recv() {
                    Ok(request) => request,
                    Err(_) => break,
                };
            }

            for &id in compute_requests.iter() {
                let backend = backend
                    .as_ref()
                    .expect("backend should have been initlized");
                let points_result: Result<ComputationResult> = (|| match &expressions.exprs[&id] {
                    ResolvedExpr {
                        expr: Expr::Explicit { lhs },
                        used_idents,
                    } if !used_idents.is_empty() => match backend.get_explicit_fn(
                        &format!("explicit_{}(f64)", id.0),
                        &lhs.ty(&expressions, &[IRType::NUMBER, IRType::NUMBER])?,
                    ) {
                        Some(ExplicitJitFn::Number(lhs)) => {
                            points_explicit(&|n: f64| lhs.call(n), range, mid, 9, 14)
                                .map(ComputationResult::Explicit)
                        }
                        Some(ExplicitJitFn::Point(_)) => {
                            Err(anyhow!("Cant Graph explicit graph of points"))
                        }
                        Some(_) => Err(anyhow!("Cant Graph explicit graph of list")),
                        None => Err(anyhow!("Explicit fn not found")),
                    },

                    ResolvedExpr {
                        expr: Expr::Explicit { lhs: node },
                        ..
                    } => Ok(ComputationResult::Constant(
                        backend
                            .eval(
                                &format!("explicit_{}()", id.0),
                                &node.ty(&expressions, &[])?,
                            )
                            .with_context(|| anyhow!("Const fn not found"))?,
                    )),
                    ResolvedExpr {
                        expr: Expr::VarDef { rhs: node, ident },
                        ..
                    } => Ok(ComputationResult::Constant(
                        backend
                            .eval(&format!("{}()", ident), &node.ty(&expressions, &[])?)
                            .with_context(|| anyhow!("Const fn not found"))?,
                    )),
                    _ => Err(anyhow!("Only explicit expressions are supported")),
                })();

                match points_result {
                    Ok(points) => {
                        event_tx.send(Event::Computed { id, points }).unwrap();
                    }
                    Err(e) => {
                        let message = if let Some(e) = errors.get(&id) {
                            format!("Error during computation: {:?}", e)
                        } else {
                            format!("Error during computation: {:?}", e)
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
    function: &impl Fn(f64) -> f64,
    range: f32,
    mid: Vector,
    min_depth: u32,
    max_depth: u32,
) -> Result<Vec<Vector>> {
    let mut points = Vec::new();

    let min_x = mid.x - range / 2.0;

    let min = Vector::new(min_x, function(min_x as f64) as f32);

    let max_x = min_x + range;
    let max = Vector::new(max_x, function(max_x as f64) as f32);

    points.push(min);

    subdivide(function, min, max, 0, min_depth, max_depth, &mut points)?;

    points.push(max);

    Ok(points)
}

fn subdivide(
    function: &impl Fn(f64) -> f64,
    min: Vector,
    max: Vector,
    depth: u32,
    min_depth: u32,
    max_depth: u32,
    points: &mut Vec<Vector>,
) -> Result<()> {
    let x_mid = (min.x + max.x) / 2.0;

    let mid = Vector::new(x_mid, function(x_mid as f64) as f32);

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
