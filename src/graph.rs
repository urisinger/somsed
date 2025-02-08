use std::{collections::HashMap, ops::Div};

use desmos_compiler::{
    expressions::ExpressionId,
    lang::backends::compiled::backend::jit::{JitListValue, JitValue, PointValue},
};
use iced::{
    event::Status,
    mouse::{self, Cursor},
    widget::canvas::{event, Cache, Event, Frame, Geometry, Path, Program, Stroke, Text},
    Color, Point, Size, Theme, Vector,
};

use crate::{server::ComputationResult, Message};

pub struct GraphRenderer<'a> {
    range: f32,
    mid: Vector,

    points: &'a HashMap<ExpressionId, ComputationResult>,
    graph_caches: &'a HashMap<ExpressionId, Cache>,
}

impl<'a> GraphRenderer<'a> {
    pub fn new(
        points: &'a HashMap<ExpressionId, ComputationResult>,
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

pub fn translate_point(point: Vector, mid: Vector, range: Vector, size: Size) -> Point {
    Point {
        x: translate_coord(point.x, mid.x, range.x, size.width),
        y: translate_coord(point.y, mid.y, -range.y, size.height),
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
        let range = Vector::new(self.range, self.range * bounds.height / bounds.width);
        // 1) Draw your graphs as before
        let graphs = self.points.iter().map(|(id, points)| {
            self.graph_caches[id].draw(renderer, bounds.size(), |frame| match points {
                ComputationResult::Explicit(points) => {
                    let path = Path::new(|builder| {
                        for point in points {
                            let point = translate_point(*point, self.mid, range, bounds.size());

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
                }
                ComputationResult::Constant(c) => match c {
                    JitValue::Number(_) => {}
                    JitValue::Point(PointValue { x, y }) => {
                        frame.fill(
                            &Path::circle(
                                translate_point(
                                    Vector::new(*x as f32, *y as f32),
                                    self.mid,
                                    range,
                                    bounds.size(),
                                ),
                                5.0,
                            ),
                            Color::from_rgb8(45, 112, 179),
                        );
                    }
                    JitValue::List(JitListValue::PointList(l)) => {
                        println!("{l:?}");
                        for PointValue { x, y } in l {
                            frame.fill(
                                &Path::circle(
                                    translate_point(
                                        Vector::new(*x as f32, *y as f32),
                                        self.mid,
                                        range,
                                        bounds.size(),
                                    ),
                                    5.0,
                                ),
                                Color::from_rgb8(45, 112, 179),
                            );
                        }
                    }
                    JitValue::List(_) => {}
                },
                ComputationResult::Implicit(_) => eprintln!("implicit not yet implemented"),
            })
        });

        // 2) Create a `Frame` to draw the grid and axes
        let mut grid = Frame::new(renderer, bounds.size());

        let pow_10 = 10.0f32.powf(range.y.div(2.0).log10().floor());
        let (step_size, substep_size) = if 5.0 * pow_10 <= range.y / 2.0 {
            (5.0 * pow_10, pow_10)
        } else if 2.0 * pow_10 <= range.y / 2.0 {
            (2.0 * pow_10, pow_10 / 2.0)
        } else {
            (pow_10, pow_10 / 5.0)
        };

        draw_grid(
            &mut grid,
            self.mid,
            step_size,
            range,
            bounds.size(),
            Stroke::default()
                .with_width(1.5)
                .with_color(Color::from_rgba8(100, 100, 100, 0.9)),
        );

        draw_grid(
            &mut grid,
            self.mid,
            substep_size,
            range,
            bounds.size(),
            Stroke::default()
                .with_width(1.0)
                .with_color(Color::from_rgba8(200, 200, 200, 0.7)),
        );

        draw_axis_with_labels(
            &mut grid,
            self.mid,
            range,
            bounds.size(),
            Color::BLACK,
            step_size,
        );

        let y_axis_x = translate_coord(0.0, self.mid.x, self.range, bounds.size().width);
        grid.stroke(
            &Path::line(
                Point::new(y_axis_x, 0.0),
                Point::new(y_axis_x, bounds.size().height),
            ),
            Stroke::default().with_width(3.0),
        );

        let geometries: Vec<_> = std::iter::once(grid.into_geometry())
            .chain(graphs)
            .collect();

        geometries
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

fn draw_grid(
    grid: &mut Frame,
    mid: Vector,
    step_size: f32,
    range: Vector,
    size: Size,
    stroke: Stroke,
) {
    let x_min = mid.x - range.x / 2.0;
    let x_max = mid.x + range.x / 2.0;

    let start_i = (x_min / step_size).floor() as i32;
    let end_i = (x_max / step_size).ceil() as i32;

    for i in start_i..=end_i {
        let x_world = i as f32 * step_size;
        let x_screen = translate_coord(x_world, mid.x, range.x, size.width);

        grid.stroke(
            &Path::line(Point::new(x_screen, 0.0), Point::new(x_screen, size.height)),
            stroke,
        );
    }

    let y_min = mid.y - range.y / 2.0;
    let y_max = mid.y + range.y / 2.0;

    let start_j = (y_min / step_size).floor() as i32;
    let end_j = (y_max / step_size).ceil() as i32;

    for j in start_j..=end_j {
        let y_world = j as f32 * step_size;
        let y_screen = translate_coord(y_world, mid.y, -range.y, size.height);

        grid.stroke(
            &Path::line(Point::new(0.0, y_screen), Point::new(size.width, y_screen)),
            stroke,
        );
    }
}

pub fn draw_axis_with_labels(
    frame: &mut Frame,
    mid: Vector,
    range: Vector,
    size: Size,
    color: Color,
    step_size: f32,
) {
    // Translate world y=0 to a screen coordinate for the horizontal axis
    let axis_screen_pos = translate_coord(0.0, mid.y, -range.y, size.height);

    // Draw the axis line
    frame.stroke(
        &Path::line(
            Point::new(0.0, axis_screen_pos),
            Point::new(size.width, axis_screen_pos),
        ),
        Stroke::default().with_width(3.0).with_color(color),
    );

    // Determine the visible world-space range in the x-direction
    let min = mid.x - (range.x / 2.0);
    let max = mid.x + (range.x / 2.0);

    // Compute the tick range in terms of multiples of `step_size`
    let start_i = (min / step_size).floor() as i32;
    let end_i = (max / step_size).ceil() as i32;

    // For each tick, draw the tick mark and label
    for i in start_i..=end_i {
        let world_coord = i as f32 * step_size;

        // Convert the world-space coordinate to screen-space
        let screen_coord = translate_coord(world_coord, mid.x, range.x, size.width);

        // Draw the tick mark
        let tick_height = 5.0;
        frame.stroke(
            &Path::line(
                Point::new(screen_coord, axis_screen_pos - tick_height),
                Point::new(screen_coord, axis_screen_pos + tick_height),
            ),
            Stroke::default().with_width(2.0).with_color(color),
        );

        // Measure text dimensions to calculate the background box size
        let text_size = 14.0;
        let padding = 4.0; // Extra padding around the text
        let text_height = text_size + padding;

        // Draw the label
        let mut label = Text::from(format!("{}", world_coord));
        label.color = color;
        label.size = text_size.into();
        label.position = Point::new(screen_coord, axis_screen_pos + 10.0 + text_height / 2.0);

        frame.fill_text(label);
    }

    // Translate world x=0 to a screen coordinate for the vertical axis
    let axis_screen_pos = translate_coord(0.0, mid.x, range.x, size.width);

    // Draw the axis line
    frame.stroke(
        &Path::line(
            Point::new(axis_screen_pos, 0.0),
            Point::new(axis_screen_pos, size.height),
        ),
        Stroke::default().with_width(3.0).with_color(color),
    );

    // Determine the visible world-space range in the y-direction
    let min = mid.y - (range.y / 2.0);
    let max = mid.y + (range.y / 2.0);

    // Compute the tick range in terms of multiples of `step_size`
    let start_i = (min / step_size).floor() as i32;
    let end_i = (max / step_size).ceil() as i32;

    // For each tick, draw the tick mark and label
    for i in start_i..=end_i {
        let world_coord = i as f32 * step_size;

        // Convert the world-space coordinate to screen-space
        let screen_coord = translate_coord(world_coord, mid.y, -range.y, size.height);

        // Draw the tick mark
        let tick_width = 5.0;
        frame.stroke(
            &Path::line(
                Point::new(axis_screen_pos - tick_width, screen_coord),
                Point::new(axis_screen_pos + tick_width, screen_coord),
            ),
            Stroke::default().with_width(2.0).with_color(color),
        );

        // Measure text dimensions to calculate the background box size
        let text_size = 14.0;
        let text_width = (format!("{}", world_coord).len() as f32) * (text_size * 0.6); // Approximate width

        // Draw the label
        let mut label = Text::from(format!("{}", world_coord));
        label.color = color;
        label.size = text_size.into();
        label.position = Point::new(axis_screen_pos + 10.0 + text_width / 2.0, screen_coord);

        frame.fill_text(label);
    }
}
