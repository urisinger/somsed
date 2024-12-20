use std::{collections::HashMap, thread};

use flume::{unbounded, Receiver, Sender};

use components::sidebar;
use desmos_compiler::{
    expressions::{ExpressionId, Expressions},
    lang::backends::llvm::{codegen::compile_all_exprs, CompiledExprs},
};
use graph::GraphRenderer;
use iced::{
    widget::{
        canvas::Cache,
        pane_grid::{self, Axis, Content, ResizeEvent},
        text_input::{focus, Id},
        Canvas,
    },
    Length, Subscription, Task, Vector,
};
use server::{Event, PointsServerRequest};

mod components;
mod graph;
mod server;

static DCG_FONT: &[u8; 45324] = include_bytes!("./dcg-icons-2024-08-02.ttf");

fn main() -> iced::Result {
    iced::application("Somsed", Somsed::update, Somsed::view)
        .font(DCG_FONT)
        .antialiasing(true)
        .subscription(Somsed::subscription)
        .run_with(Somsed::new)?;
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
    ServerResponse(Event),
}

enum PaneType {
    Graph,
    Sidebar,
}

struct Somsed {
    panes: pane_grid::State<PaneType>,
    graph_caches: HashMap<ExpressionId, Cache>,
    expressions: HashMap<ExpressionId, String>,

    points: HashMap<ExpressionId, Vec<Option<Vector>>>,

    errors: HashMap<ExpressionId, String>,

    equation_tx: Option<Sender<PointsServerRequest>>,

    shown_error: Option<ExpressionId>,

    pub max_id: u32,

    scale: f32,
    mid: Vector,
    resolution: u32,
}

impl Somsed {
    fn new() -> (Self, Task<Message>) {
        let graph_caches = HashMap::new();

        let (mut panes, pane) = pane_grid::State::new(PaneType::Sidebar);

        panes.split(Axis::Vertical, pane, PaneType::Graph);

        (
            Self {
                panes,
                scale: 100.0,
                mid: Vector { x: 0.0, y: 0.0 },
                resolution: 1000,
                errors: HashMap::new(),
                max_id: 0,
                equation_tx: None,

                points: HashMap::new(),

                expressions: HashMap::new(),
                graph_caches,

                shown_error: None,
            },
            Task::none(),
        )
    }

    fn view(&self) -> pane_grid::PaneGrid<'_, Message> {
        pane_grid::PaneGrid::new(&self.panes, move |_, id, _| match id {
            PaneType::Graph => Content::new(
                Canvas::new(GraphRenderer::new(
                    &self.points,
                    &self.graph_caches,
                    self.scale,
                    self.mid,
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

    fn subscription(&self) -> Subscription<Message> {
        Subscription::run(server::points_server).map(Message::ServerResponse)
    }

    fn update(&mut self, message: Message) -> Task<Message> {
        match message {
            Message::Moved(p) => {
                self.mid = self.mid + p;
                self.clear_caches();
            }
            Message::EquationChanged(id, s) => {
                self.expressions.insert(id, s.clone());

                if s.is_empty() {
                    return Task::none();
                }
                let _ = self
                    .equation_tx
                    .as_ref()
                    .map(|sender| sender.send(PointsServerRequest::Compile { id, expr_source: s }));

                let _ = self.equation_tx.as_ref().map(|sender| {
                    sender.send(PointsServerRequest::Compute {
                        id,
                        range: self.scale,
                        mid: self.mid,
                        resolution: self.resolution,
                    })
                });
            }
            Message::EquationAdded(s) => {
                let id = ExpressionId(self.max_id);
                self.max_id += 1;

                self.expressions.insert(id, s.clone());
                self.graph_caches.insert(id, Cache::new());
                if s.is_empty() {
                    return focus(Id::new(format!("equation_{}", id.0)));
                }
                let _ = self
                    .equation_tx
                    .as_ref()
                    .expect("sender not recived")
                    .send(PointsServerRequest::Compile { id, expr_source: s });

                let _ = self.equation_tx.as_ref().expect("sender not recived").send(
                    PointsServerRequest::Compute {
                        id,
                        range: self.scale,
                        mid: self.mid,
                        resolution: self.resolution,
                    },
                );
                return focus(Id::new(format!("equation_{}", id.0)));
            }
            Message::Scaled(scale, mid) => {
                self.scale = scale;
                if let Some(mid) = mid {
                    self.mid = mid;
                }
                self.clear_caches();

                for id in self.expressions.keys() {
                    let _ = self
                        .equation_tx
                        .as_ref()
                        .expect("sender not received")
                        .send(PointsServerRequest::Compute {
                            id: *id,
                            range: self.scale,
                            mid: self.mid,
                            resolution: self.resolution,
                        });
                }
            }
            Message::ShowError(i) => {
                self.shown_error = i;
            }
            Message::FocusExpr(i) => return focus(Id::new(format!("equation_{}", i))),
            Message::Resized(ResizeEvent { split, ratio }) => {
                self.panes.resize(split, ratio);
            }
            Message::ServerResponse(response) => {
                match response {
                    Event::Computed { id, points } => {
                        self.points.insert(id, points);
                        self.graph_caches.get(&id).map(Cache::clear);
                        self.errors.remove(&id);
                    }
                    Event::Error { id, message } => {
                        self.errors.insert(id, message);
                    }
                    Event::Sender(sender) => {
                        self.equation_tx = Some(sender);
                    }
                };
                return Task::none();
            }
        };

        Task::none()
    }
}
