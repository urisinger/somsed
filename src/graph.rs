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
    range: f32,
    mid: Vector,

    points: &'a HashMap<ExpressionId, Vec<Vector>>,
    graph_caches: &'a HashMap<ExpressionId, Cache>,
}

impl<'a> GraphRenderer<'a> {
    pub fn new(
        points: &'a HashMap<ExpressionId, Vec<Vector>>,
        graph_caches: &'a HashMap<ExpressionId, Cache>,
        range: f32,
        mid: Vector,
    ) -> Self {
        Self {
            points,
            graph_caches,
            range,
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

pub fn translate_point(point: Vector, mid: Vector, range: f32, size: Size) -> Point {
    Point {
        x: translate_coord(point.x, mid.x, range, size.width),
        y: translate_coord(point.y, mid.y, -range, size.height),
    }
}

pub fn translate_coord(point: f32, mid: f32, range: f32, size: f32) -> f32 {
    (point - mid) * size / range + size / 2.0
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
                let path = Path::new(|builder| {
                    for point in points {
                        let point = translate_point(*point, self.mid, self.range, bounds.size());

                        if !point.x.is_finite() || !point.y.is_finite() {
                            continue;
                        }
                        builder.line_to(point);
                    }
                });
                frame.stroke(
                    &path,
                    Stroke::default()
                        .with_width(5.0)
                        .with_color(Color::from_rgb8(45, 112, 179)),
                );
            })
        });

        let mut graphs: Vec<_> = graphs.collect();

        // Create the grid and axis lines
        let mut grid = Frame::new(renderer, bounds.size());

        // Draw x-axis
        let x_axis_y = translate_coord(0.0, self.mid.y, -self.range, bounds.size().height);
        grid.stroke(
            &Path::line(
                Point::new(0.0, x_axis_y),
                Point::new(bounds.size().width, x_axis_y),
            ),
            Stroke::default().with_width(3.0),
        );

        // Draw y-axis
        let y_axis_x = translate_coord(0.0, self.mid.x, self.range, bounds.size().width);
        grid.stroke(
            &Path::line(
                Point::new(y_axis_x, 0.0),
                Point::new(y_axis_x, bounds.size().height),
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
        match event {
            Event::Mouse(event) => match event {
                mouse::Event::ButtonPressed(mouse::Button::Left) => {
                    let Some(cursor_position) = cursor.position_in(bounds) else {
                        return (event::Status::Ignored, None);
                    };
                    if let GraphState::None = *state {
                        *state = GraphState::Moving {
                            start: cursor_position,
                        };
                    }
                    (event::Status::Captured, None)
                }
                mouse::Event::ButtonReleased(mouse::Button::Left)
                | mouse::Event::CursorLeft
                | mouse::Event::CursorEntered => {
                    *state = GraphState::None;
                    (event::Status::Captured, None)
                }
                mouse::Event::CursorMoved { .. } => match *state {
                    GraphState::Moving { start } => {
                        let Some(cursor_position) = cursor.position_in(bounds) else {
                            return (event::Status::Ignored, None);
                        };
                        let mut diff = (start - cursor_position) * self.range;
                        diff.x /= bounds.width;
                        diff.y /= bounds.height;
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
                        let new_range = self.range / (1.0 + y / 30.0); // Adjust range inversely to scaling

                        let mid =
                            if let Some(cursor_to_center) = cursor.position_from(bounds.center()) {
                                let factor = new_range - self.range;

                                Some(
                                    self.mid
                                        - Vector::new(
                                            cursor_to_center.x * factor / bounds.width,
                                            -cursor_to_center.y * factor / bounds.height,
                                        ),
                                )
                            } else {
                                None
                            };

                        (
                            event::Status::Captured,
                            Some(Message::Scaled(new_range, mid)),
                        )
                    }
                },
                _ => (event::Status::Ignored, None),
            },
            _ => (event::Status::Ignored, None),
        }
    }
}
