use std::collections::HashMap;

use desmos_compiler::expressions::ExpressionId;
use iced::{
    alignment, mouse,
    widget::{
        column, container, mouse_area, row, scrollable, text, text::LineHeight, text_input,
        text_input::Id, tooltip, TextInput,
    },
    Color, Element, Length, Padding,
};

use super::icons;
use crate::Message;

pub fn view<'element>(
    equations: &'element HashMap<ExpressionId, String>,
    errors: &'element HashMap<ExpressionId, String>,
    shown_error: &Option<ExpressionId>,
) -> Element<'element, crate::Message> {
    let mut elements = equations
        .iter()
        .map(|(i, equation)| {
            let input = TextInput::new("", equation)
                .on_input(move |s| Message::EquationChanged(*i, s))
                .size(20)
                .padding(Padding {
                    top: 10.0,
                    bottom: 10.0,
                    right: 0.0,
                    left: 0.0,
                })
                .line_height(LineHeight::Absolute(30.0.into()))
                .id(Id::new(format!("equation_{}", i.0)))
                .width(Length::Fill);

            let show_err = if errors.get(i).is_some() {
                mouse_area(
                    container(icons::error().size(20))
                        .align_x(alignment::Horizontal::Center)
                        .align_y(alignment::Vertical::Center)
                        .width(Length::Fixed(35.0))
                        .height(Length::Fixed(50.0)),
                )
                .on_enter(Message::ShowError(Some(*i)))
                .interaction(mouse::Interaction::Grab)
                .on_exit(Message::ShowError(None))
            } else {
                mouse_area(
                    container("")
                        .width(Length::Fixed(35.0))
                        .height(Length::Fixed(50.0)),
                )
                .on_enter(Message::ShowError(Some(*i)))
                .on_exit(Message::ShowError(None))
            };

            let left: Element<crate::Message> = if let Some(i) = *shown_error {
                if let Some(err) = errors.get(&i) {
                    tooltip(
                        show_err,
                        container(text(err).style(|_| text::Style {
                            color: Some(Color::WHITE),
                        }))
                        .padding(5)
                        .width(Length::Shrink)
                        .height(Length::Shrink)
                        .style(styles::floating_box),
                        tooltip::Position::Bottom,
                    )
                    .into()
                } else {
                    show_err.into()
                }
            } else {
                show_err.into()
            };
            row![left, input].into()
        })
        .collect::<Vec<Element<crate::Message>>>();

    elements.push(
        mouse_area(
            container("")
                .style(styles::add_eq)
                .width(Length::Fill)
                .padding(0)
                .height(Length::Fixed(50.0)),
        )
        .on_press(Message::EquationAdded("".to_string()))
        .interaction(mouse::Interaction::Grab)
        .into(),
    );

    let sidebar = scrollable(column(elements))
        .width(Length::Fill)
        .height(Length::Fill);

    let view = container(sidebar)
        .width(Length::Fill)
        .height(Length::Fill)
        .style(styles::sidebar);

    view.into()
}

mod styles {
    use iced::{widget::container, Border, Color, Shadow, Theme, Vector};

    pub fn add_eq(_: &Theme) -> container::Style {
        container::Style {
            border: Border {
                width: 1.0,
                color: Color::from_rgb8(240, 240, 240),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    pub fn floating_box(_: &Theme) -> container::Style {
        container::Style {
            background: Some(iced::Background::Color(Color::from_rgb8(102, 102, 102))),
            border: Border {
                radius: 8.0.into(),
                width: 0.3,
                color: Color::from_rgb8(102, 102, 102),
            },
            ..Default::default()
        }
    }

    pub fn sidebar(_: &Theme) -> container::Style {
        container::Style {
            border: Border {
                width: 1.0,
                radius: 0.5.into(),

                color: Color::from_rgb8(204, 204, 204),
            },
            shadow: Shadow {
                blur_radius: 5.0,
                color: Color::from_rgb8(204, 204, 204),
                offset: Vector::new(2.0, 0.0),
            },
            ..Default::default()
        }
    }
}
