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

struct UnsafeContext(inkwell::context::Context);

unsafe impl Send for UnsafeContext {}
unsafe impl Sync for UnsafeContext {}

use inkwell::context::Context;
use once_cell::sync::Lazy;

static GLOBAL_CONTEXT: Lazy<UnsafeContext> = Lazy::new(|| UnsafeContext(Context::create()));
fn main() -> iced::Result {
    {
        iced::application("Somsed", Somsed::update, Somsed::view)
            .font(DCG_FONT)
            .antialiasing(true)
            .run_with(move || (Somsed::new(&GLOBAL_CONTEXT.0), Task::none()))?;
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

struct Somsed<'a> {
    context: &'a inkwell::context::Context,
    panes: pane_grid::State<PaneType>,
    graph_caches: HashMap<ExpressionId, Cache>,
    parsed_expressions: Expressions,
    expressions: HashMap<ExpressionId, String>,

    errors: HashMap<ExpressionId, String>,

    compiled_eqs: CompiledExprs<'a>,

    shown_error: Option<ExpressionId>,

    pub max_id: u32,

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

impl<'a> Somsed<'a> {
    fn new(options: &'a inkwell::context::Context) -> Self {
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
            context: options,
            errors: HashMap::new(),
            max_id: 0,

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
            Message::EquationChanged(id, s) => {
                match self.parsed_expressions.insert_expr(id, &s) {
                    Ok(()) => {
                        self.compiled_eqs =
                            compile_all_exprs(self.context, &self.parsed_expressions);

                        self.graph_caches
                            .entry(id)
                            .or_insert_with(|| Cache::new())
                            .clear();
                    }
                    Err(e) => {
                        eprintln!("failed to parse eq: {e}");
                    }
                }
                self.expressions.insert(id, s);
            }
            Message::EquationAdded(s) => {
                let id = ExpressionId(self.max_id);
                match self.parsed_expressions.insert_expr(id, &s) {
                    Ok(()) => {
                        self.graph_caches.insert(id, Cache::new());
                        self.compiled_eqs =
                            compile_all_exprs(self.context, &self.parsed_expressions);
                        return focus(Id::new(format!("equation_{}", self.max_id)));
                    }
                    Err(e) => {
                        eprintln!("failed to parse eq: {e}");
                    }
                };

                self.expressions.insert(id, s);

                self.max_id += 1;
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
