use std::collections::HashMap;


use desmos_compiler::expressions::ExpressionId;
use iced::{
    event::Status,
    mouse::{self, Cursor},
    widget::canvas::{event, Cache, Event, Frame, Geometry, Path, Program, Stroke},
    Color, Point, Size, Theme, Vector,
};

use crate::Message;

pub struct GraphRenderer<'a> {
    scale: f32,
    mid: Vector,

    points: &'a HashMap<ExpressionId, Vec<Option<Vector>>>,
    graph_caches: &'a HashMap<ExpressionId, Cache>,
}

impl<'a> GraphRenderer<'a> {
    pub fn new(
        points: &'a HashMap<ExpressionId, Vec<Option<Vector>>>,
        graph_caches: &'a HashMap<ExpressionId, Cache>,
        scale: f32,
        mid: Vector,
    ) -> Self {
        Self {
            points,
            graph_caches,
            scale,
            mid,
        }
    }
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

impl Program<Message> for GraphRenderer<'_> {
    type State = GraphState;
    fn draw(
        &self,
        _: &Self::State,
        renderer: &iced::Renderer,
        _: &Theme,
        bounds: iced::Rectangle,
        _: Cursor,
    ) -> Vec<Geometry> {
        let graphs = self.points.iter().map(|(id, points)| {
            self.graph_caches[id].draw(renderer, bounds.size(), |frame| {
                for i in 0..points.len().saturating_sub(1) {
                    if let (Some(point), Some(next_point)) = (points[i], points[i + 1]) {
                        let point = translate_point(point, self.mid, self.scale, bounds.size());
                        let next_point =
                            translate_point(next_point, self.mid, self.scale, bounds.size());

                        if point.y.is_finite() && next_point.y.is_finite() {
                            frame.stroke(
                                &Path::line(point, next_point),
                                Stroke::default()
                                    .with_width(3.0)
                                    .with_color(Color::from_rgb8(45, 112, 179)),
                            );
                        }
                    }
                }
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
                    if let GraphState::None = *state {
                        *state = GraphState::Moving {
                            start: cursor_position,
                        };
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
