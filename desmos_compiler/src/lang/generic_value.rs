use std::fmt::{self, Debug, Formatter};

pub type ValueType = GenericValue<(), (), (), ()>;

pub type ListType = GenericList<(), ()>;

pub enum GenericValue<NumberT, PointT, NumberListT, PointListT> {
    Number(NumberT),
    Point(PointT),
    List(GenericList<NumberListT, PointListT>),
}

pub enum GenericList<NumberListT, PointListT> {
    NumberList(NumberListT),
    PointList(PointListT),
}

impl<NumberT, PointT, NumberListT, PointListT>
    GenericValue<NumberT, PointT, NumberListT, PointListT>
{
    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Number(_) => ValueType::Number(()),
            Self::Point(_) => ValueType::Point(()),
            Self::List(list) => ValueType::List(match list {
                GenericList::NumberList(_) => ListType::NumberList(()),
                GenericList::PointList(_) => ListType::PointList(()),
            }),
        }
    }
}

impl<NumberT: Clone, PointT: Clone, NumberListT: Clone, PointListT: Clone> Clone
    for GenericValue<NumberT, PointT, NumberListT, PointListT>
{
    fn clone(&self) -> Self {
        match self {
            GenericValue::Number(n) => GenericValue::Number(n.clone()),
            GenericValue::Point(p) => GenericValue::Point(p.clone()),
            GenericValue::List(l) => GenericValue::List(l.clone()),
        }
    }
}

impl<NumberListT: Clone, PointListT: Clone> Clone for GenericList<NumberListT, PointListT> {
    fn clone(&self) -> Self {
        match self {
            GenericList::NumberList(nl) => GenericList::NumberList(nl.clone()),
            GenericList::PointList(pl) => GenericList::PointList(pl.clone()),
        }
    }
}

impl<NumberT: PartialEq, PointT: PartialEq, NumberListT: PartialEq, PointListT: PartialEq> PartialEq
    for GenericValue<NumberT, PointT, NumberListT, PointListT>
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (GenericValue::Number(n1), GenericValue::Number(n2)) => n1 == n2,
            (GenericValue::Point(p1), GenericValue::Point(p2)) => p1 == p2,
            (GenericValue::List(l1), GenericValue::List(l2)) => l1 == l2,
            _ => false,
        }
    }
}

impl<NumberT: Eq, PointT: Eq, NumberListT: Eq, PointListT: Eq> Eq
    for GenericValue<NumberT, PointT, NumberListT, PointListT>
{
}

impl<NumberListT: PartialEq, PointListT: PartialEq> PartialEq
    for GenericList<NumberListT, PointListT>
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (GenericList::NumberList(nl1), GenericList::NumberList(nl2)) => nl1 == nl2,
            (GenericList::PointList(pl1), GenericList::PointList(pl2)) => pl1 == pl2,
            _ => false,
        }
    }
}

impl<NumberListT: Eq, PointListT: Eq> Eq for GenericList<NumberListT, PointListT> {}

impl<NumberT: Debug, PointT: Debug, NumberListT: Debug, PointListT: Debug> Debug
    for GenericValue<NumberT, PointT, NumberListT, PointListT>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericValue::Number(n) => write!(f, "Number({:?})", n),
            GenericValue::Point(p) => write!(f, "Point({:?})", p),
            GenericValue::List(l) => write!(f, "List({:?})", l),
        }
    }
}

impl<NumberListT: Debug, PointListT: Debug> Debug for GenericList<NumberListT, PointListT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenericList::NumberList(nl) => write!(f, "NumberList({:?})", nl),
            GenericList::PointList(pl) => write!(f, "PointList({:?})", pl),
        }
    }
}

impl ValueType {
    pub fn name(&self) -> &'static str {
        match self {
            GenericValue::Number(_) => "Number",
            GenericValue::Point(_) => "Point",
            GenericValue::List(_) => "List",
        }
    }
}

impl ListType {
    pub fn name(&self) -> &'static str {
        match self {
            GenericList::NumberList(_) => "NumberList",
            GenericList::PointList(_) => "PointList",
        }
    }
}
