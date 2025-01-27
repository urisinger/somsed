use std::collections::{BTreeSet, BinaryHeap, HashMap};

use flume::Sender;

use components::sidebar;
use desmos_compiler::expressions::{ExpressionId, Expressions};
use graph::GraphRenderer;
use iced::{
    widget::{
        canvas::Cache,
        pane_grid::{self, Content, ResizeEvent},
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
    EquationAdded(String),
    EquationChanged(ExpressionId, String),
    EquationRemoved(ExpressionId),
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
    // Big data structures used to store expressions
    sorted_expressions: BTreeSet<ExpressionId>,

    graph_caches: HashMap<ExpressionId, Cache>,
    expressions: HashMap<ExpressionId, String>,
    points: HashMap<ExpressionId, Vec<Vector>>,
    errors: HashMap<ExpressionId, String>,

    // Fields used for rendering state
    panes: pane_grid::State<PaneType>,

    shown_error: Option<ExpressionId>,

    max_id: u32,
    range: f32,
    mid: Vector,

    // Sender to send Requests to the point server
    equation_tx: Option<Sender<PointsServerRequest>>,
}

impl Somsed {
    const DEFAULT_MID: Vector = Vector { x: 0.0, y: 0.0 };
    const DEFAULT_RANGE: f32 = 10.0;
    fn new() -> (Self, Task<Message>) {
        let graph_caches = HashMap::new();

        let panes = pane_grid::State::with_configuration(pane_grid::Configuration::Split {
            axis: pane_grid::Axis::Vertical,
            ratio: 0.2,
            a: Box::new(pane_grid::Configuration::Pane(PaneType::Sidebar)),
            b: Box::new(pane_grid::Configuration::Pane(PaneType::Graph)),
        });

        (
            Self {
                panes,
                range: Self::DEFAULT_RANGE,
                mid: Self::DEFAULT_MID,
                sorted_expressions: BTreeSet::new(),
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
                    self.range,
                    self.mid,
                ))
                .width(Length::FillPortion(3))
                .height(Length::Fill),
            ),
            PaneType::Sidebar => pane_grid::Content::new(sidebar::view(
                &self.sorted_expressions,
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
        for v in self.graph_caches.values_mut() {
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
                let _ = self
                    .equation_tx
                    .as_ref()
                    .expect("sender not received")
                    .send(PointsServerRequest::Resize {
                        range: self.range,
                        mid: self.mid,
                    });
            }
            Message::EquationChanged(id, s) => {
                self.expressions.insert(id, s.clone());

                if s.is_empty() {
                    self.points.remove(&id);
                    let _ = self
                        .equation_tx
                        .as_ref()
                        .map(|sender| sender.send(PointsServerRequest::Remove { id }));
                }

                let _ = self
                    .equation_tx
                    .as_ref()
                    .map(|sender| sender.send(PointsServerRequest::Compile { id, expr_source: s }));
            }
            Message::EquationAdded(s) => {
                let id = self.add_expression(s);
                return focus(Id::new(format!("equation_{}", id.0)));
            }
            Message::EquationRemoved(id) => {
                self.points.remove(&id);
                self.expressions.remove(&id);
                self.errors.remove(&id);

                self.sorted_expressions.remove(&id);

                let _ = self
                    .equation_tx
                    .as_ref()
                    .map(|sender| sender.send(PointsServerRequest::Remove { id }));
            }
            Message::Scaled(new_range, mid) => {
                self.range = new_range;
                if let Some(mid) = mid {
                    self.mid = mid;
                }
                self.clear_caches();

                let _ = self
                    .equation_tx
                    .as_ref()
                    .expect("sender not received")
                    .send(PointsServerRequest::Resize {
                        range: self.range,
                        mid: self.mid,
                    });
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
                        self.points.remove(&id);
                    }
                    Event::Sender(sender) => {
                        self.equation_tx = Some(sender);
                    }
                    Event::Success { id } => {
                        self.errors.remove(&id);
                    }
                };
                return Task::none();
            }
        };

        Task::none()
    }
}

impl Somsed {
    fn add_expression(&mut self, s: String) -> ExpressionId {
        let id = ExpressionId(self.max_id);
        self.sorted_expressions.insert(id);
        self.max_id += 1;

        self.expressions.insert(id, s.clone());
        self.graph_caches.insert(id, Cache::new());
        if s.is_empty() {
            return id;
        }
        let _ = self
            .equation_tx
            .as_ref()
            .expect("sender not recived")
            .send(PointsServerRequest::Compile { id, expr_source: s });
        id
    }
}
