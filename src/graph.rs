use std::collections::HashMap;

use anyhow::{anyhow, Result};

use desmos_compiler::{
    expressions::ExpressionId,
    lang::backends::llvm::{jit::ExplicitJitFn, CompiledExpr, CompiledExprs},
};
use iced::{
    event::Status,
    mouse::{self, Cursor},
    widget::canvas::{event, Cache, Event, Frame, Geometry, Path, Program, Stroke},
    Color, Point, Size, Theme, Vector,
};

use crate::Message;

pub struct GraphRenderer<'a, 'context> {
    scale: f32,
    mid: Vector,
    resolution: u32,

    exprs: &'a CompiledExprs<'context>,
    graph_caches: &'a HashMap<ExpressionId, Cache>,
}

impl<'a, 'context> GraphRenderer<'a, 'context> {
    pub fn new(
        exprs: &'a CompiledExprs<'context>,
        graph_caches: &'a HashMap<ExpressionId, Cache>,
        scale: f32,
        mid: Vector,
        resolution: u32,
    ) -> Self {
        Self {
            exprs,
            graph_caches,
            scale,
            mid,
            resolution,
        }
    }
}

pub fn points(
    ast: &ExplicitJitFn,
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
            match ast {
                ExplicitJitFn::Number(jit_func) => jit_func.call(x),
                ExplicitJitFn::List(_) => {
                    return Err(anyhow!("expected a numeric return value, got list type"))
                }
            }
        };

        points.push(Some(Vector {
            x: x as f32,
            y: y as f32,
        }));
    }

    Ok(points)
}

pub enum GraphState {
    None,
    Moving { start: Point },
}

impl Default for GraphState {
    fn default() -> Self {
        Self::None
    }
}

pub fn translate_point(point: Vector, mid: Vector, scale: f32, size: Size) -> Point {
    Point {
        x: translate_coord(point.x, mid.x, scale, size.width),
        y: translate_coord(point.y, mid.y, -scale, size.height),
    }
}

pub fn translate_coord(point: f32, mid: f32, scale: f32, size: f32) -> f32 {
    (point - mid) * scale + size / 2.0
}

impl<'a, 'context> Program<Message> for GraphRenderer<'a, 'context> {
    type State = GraphState;
    fn draw(
        &self,
        _: &Self::State,
        renderer: &iced::Renderer,
        _: &Theme,
        bounds: iced::Rectangle,
        _: Cursor,
    ) -> Vec<Geometry> {
        let graphs = self.exprs.compiled.iter().map(|(i, graph)| {
            self.graph_caches[i].draw(renderer, bounds.size(), |frame| match graph {
                CompiledExpr::Explicit { lhs } => {
                    match points(lhs, bounds.width / self.scale, self.mid, self.resolution) {
                        Ok(points) => {
                            for i in 0..points.len() - 1 {
                                match (points[i], points[i + 1]) {
                                    (Some(point), Some(next_point)) => {
                                        let point = translate_point(
                                            point,
                                            self.mid,
                                            self.scale,
                                            bounds.size(),
                                        );

                                        let next_point = translate_point(
                                            next_point,
                                            self.mid,
                                            self.scale,
                                            bounds.size(),
                                        );

                                        if point.y > frame.size().height * 2.0
                                            || point.y < -frame.size().height
                                            || point.y.is_nan()
                                            || !point.y.is_finite()
                                            || !next_point.y.is_finite()
                                            || !next_point.y.is_finite()
                                        {
                                            continue;
                                        }

                                        frame.stroke(
                                            &Path::line(point, next_point),
                                            Stroke::default()
                                                .with_width(3.0)
                                                .with_color(Color::from_rgb8(45, 112, 179)),
                                        );
                                    }
                                    _ => (),
                                }
                            }
                        }
                        Err(e) => eprintln!("error in eval, {}", e),
                    }
                }
                CompiledExpr::Implicit { .. } => (),
            })
        });

        let mut graphs: Vec<_> = graphs.collect();

        let mut grid = Frame::new(renderer, bounds.size());
        grid.stroke(
            &Path::line(
                Point::new(
                    translate_coord(0.0, self.mid.x, self.scale, bounds.size().width),
                    bounds.size().height,
                ),
                Point::new(
                    translate_coord(0.0, self.mid.x, self.scale, bounds.size().width),
                    0.0,
                ),
            ),
            Stroke::default().with_width(3.0),
        );

        grid.stroke(
            &Path::line(
                Point::new(
                    bounds.size().width,
                    translate_coord(0.0, self.mid.y, -self.scale, bounds.size().height),
                ),
                Point::new(
                    0.0,
                    translate_coord(0.0, self.mid.y, -self.scale, bounds.size().height),
                ),
            ),
            Stroke::default().with_width(3.0),
        );
        graphs.push(grid.into_geometry());
        graphs
    }

    fn update(
        &self,
        state: &mut Self::State,
        event: event::Event,
        bounds: iced::Rectangle,
        cursor: mouse::Cursor,
    ) -> (Status, Option<Message>) {
        let Some(cursor_position) = cursor.position_in(bounds) else {
            return (event::Status::Ignored, None);
        };
        match event {
            Event::Mouse(event) => match event {
                mouse::Event::ButtonPressed(mouse::Button::Left) => {
                    match *state {
                        GraphState::None => {
                            *state = GraphState::Moving {
                                start: cursor_position,
                            };
                        }
                        _ => {}
                    }
                    (event::Status::Captured, None)
                }
                mouse::Event::ButtonReleased(mouse::Button::Left) => {
                    *state = GraphState::None;
                    (event::Status::Captured, None)
                }
                mouse::Event::CursorMoved { .. } => match *state {
                    GraphState::Moving { start } => {
                        let mut diff = (start - cursor_position) * (1.0 / self.scale);
                        diff.y *= -1.0;
                        *state = GraphState::Moving {
                            start: cursor_position,
                        };
                        (event::Status::Captured, Some(Message::Moved(diff)))
                    }
                    GraphState::None => (event::Status::Ignored, None),
                },
                mouse::Event::WheelScrolled { delta } => match delta {
                    mouse::ScrollDelta::Lines { y, .. } | mouse::ScrollDelta::Pixels { y, .. } => {
                        let scaling = self.scale * (1.0 + y / 30.0);
                        let mid = if let Some(cursor_to_center) =
                            cursor.position_from(bounds.center())
                        {
                            let factor = scaling - self.scale;

                            Some(
                                self.mid
                                    - Vector::new(
                                        -cursor_to_center.x * factor / (self.scale * self.scale),
                                        cursor_to_center.y * factor / (self.scale * self.scale),
                                    ),
                            )
                        } else {
                            None
                        };
                        (event::Status::Captured, Some(Message::Scaled(scaling, mid)))
                    }
                },
                _ => (event::Status::Ignored, None),
            },
            _ => (event::Status::Ignored, None),
        }
    }
}
