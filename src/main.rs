use std::collections::HashMap;

use components::sidebar;
use desmos_compiler::{
    expressions::{ExpressionId, Expressions},
    lang::backends::llvm::{codegen::compile_all_exprs, CompiledExprs},
};
use graph::GraphRenderer;
use iced::{
    alignment::Horizontal,
    overlay,
    widget::{
        self,
        canvas::Cache,
        container, mouse_area, opaque,
        pane_grid::{self, Axis, Content, Pane, ResizeEvent},
        row,
        text_input::{self, focus, Id},
        Canvas, Stack, TextInput,
    },
    Application, Color, Length, Padding, Settings, Task, Vector,
};

mod components;
mod graph;

static DCG_FONT: &[u8; 45324] = include_bytes!("./dcg-icons-2024-08-02.ttf");

fn main() -> iced::Result {
    {
        iced::application("Somsed", Somsed::update, Somsed::view)
            .font(DCG_FONT)
            .antialiasing(true)
            .run_with(move || (Somsed::new(), Task::none()))?;
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub enum Message {
    Moved(Vector),
    Scaled(f32, Option<Vector>),
    EquationChanged(ExpressionId, String),
    EquationAdded(String),
    ShowError(Option<ExpressionId>),
    FocusExpr(usize),
    Resized(pane_grid::ResizeEvent),
}

enum PaneType {
    Graph,
    Sidebar,
}

struct Somsed {
    context: inkwell::context::Context,
    panes: pane_grid::State<PaneType>,
    graph_caches: HashMap<ExpressionId, Cache>,
    parsed_expressions: Expressions,
    expressions: HashMap<ExpressionId, String>,

    errors: HashMap<ExpressionId, String>,

    compiled_eqs: CompiledExprs<'static>,

    shown_error: Option<ExpressionId>,

    scale: f32,
    mid: Vector,
    resolution: u32,
}

#[derive(Debug)]
struct Options<'a> {
    context: &'a inkwell::context::Context,
}

impl<'a> Options<'a> {
    pub fn new(context: &'a inkwell::context::Context) -> Self {
        Self { context }
    }
}

impl Somsed {
    fn new() -> Self {
        let graph_caches = HashMap::new();
        let expressions = Expressions::new();

        let (mut panes, pane) = pane_grid::State::new(PaneType::Sidebar);

        panes.split(Axis::Vertical, pane, PaneType::Graph);

        Self {
            panes,
            compiled_eqs: CompiledExprs::new(),
            scale: 100.0,
            mid: Vector { x: 0.0, y: 0.0 },
            resolution: 1000,
            context: inkwell::context::Context::create(),
            errors: HashMap::new(),

            parsed_expressions: expressions,
            expressions: HashMap::new(),
            graph_caches,

            shown_error: None,
        }
    }

    fn view(&self) -> pane_grid::PaneGrid<'_, Message> {
        pane_grid::PaneGrid::new(&self.panes, move |_, id, _| match id {
            PaneType::Graph => Content::new(
                Canvas::new(GraphRenderer::new(
                    &self.compiled_eqs,
                    &self.graph_caches,
                    self.scale,
                    self.mid,
                    self.resolution,
                ))
                .width(Length::Fill)
                .height(Length::Fill),
            ),
            PaneType::Sidebar => pane_grid::Content::new(sidebar::view(
                &self.expressions,
                &self.errors,
                &self.shown_error,
            )),
        })
        .on_resize(10, Message::Resized)
        .width(Length::Fill)
        .height(Length::Fill)
    }

    pub fn clear_caches(&mut self) {
        for (_, v) in &mut self.graph_caches {
            v.clear();
        }
    }

    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Moved(p) => {
                self.mid = self.mid + p;
                self.clear_caches();
            }
            Message::EquationChanged(i, s) => {
                self.parsed_expressions.set_expr(i, &s);

                self.compiled_eqs = compile_all_exprs(&self.context, &self.parsed_expressions);

                self.graph_caches[&i].clear();
            }
            Message::EquationAdded(s) => {
                self.parsed_expressions.add_expr(&s);

                self.graph_caches.insert(
                    ExpressionId(self.parsed_expressions.max_id - 1),
                    Cache::new(),
                );
                self.compiled_eqs = compile_all_exprs(&self.context, &self.parsed_expressions);
                return focus(Id::new(format!(
                    "equation_{}",
                    self.parsed_expressions.max_id - 1
                )));
            }
            Message::Scaled(scale, mid) => {
                self.scale = scale;
                if let Some(mid) = mid {
                    self.mid = mid;
                }
                self.clear_caches();
            }
            Message::ShowError(i) => {
                self.shown_error = i;
            }
            Message::FocusExpr(i) => return focus(Id::new(format!("equation_{}", i))),
            Message::Resized(ResizeEvent { split, ratio }) => {
                self.panes.resize(split, ratio);
            }
        };
        Task::none()
    }
}
