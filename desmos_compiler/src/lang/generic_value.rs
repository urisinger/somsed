use std::fmt::{self, Debug, Formatter};

pub type ValueType = GenericValue<(), (), (), ()>;

pub type ListType = GenericList<(), ()>;

pub enum GenericValue<NumberT, PointT, NumberListT, PointListT> {
    Number(NumberT),
    Point(PointT),
    List(GenericList<NumberListT, PointListT>),
}

//Even tho is is name GenericList, it can also represent a generic scaler
pub enum GenericList<NumberListT, PointListT> {
    Number(NumberListT),
    Point(PointListT),
}

pub type GenericScalerValue<NumberT, PointT> = GenericList<NumberT, PointT>;

pub type ScalerType = GenericList<(), ()>;

impl<NumberT, PointT, NumberListT, PointListT>
    GenericValue<NumberT, PointT, NumberListT, PointListT>
{
    pub fn get_type(&self) -> ValueType {
        match self {
            Self::Number(_) => ValueType::Number(()),
            Self::Point(_) => ValueType::Point(()),
            Self::List(list) => ValueType::List(match list {
                GenericList::Number(_) => ListType::Number(()),
                GenericList::Point(_) => ListType::Point(()),
            }),
        }
    }

    pub fn lift_scaler(self) -> Option<GenericScalerValue<NumberT, PointT>> {
        match self {
            Self::Number(n) => Some(GenericScalerValue::Number(n)),
            Self::Point(p) => Some(GenericScalerValue::Point(p)),
            Self::List(_) => None,
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
            GenericList::Number(nl) => GenericList::Number(nl.clone()),
            GenericList::Point(pl) => GenericList::Point(pl.clone()),
        }
    }
}

impl<NumberT: Copy, PointT: Copy, NumberListT: Copy, PointListT: Copy> Copy
    for GenericValue<NumberT, PointT, NumberListT, PointListT>
{
}

impl<NumberListT: Copy, PointListT: Copy> Copy for GenericList<NumberListT, PointListT> {}

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
            (GenericList::Number(nl1), GenericList::Number(nl2)) => nl1 == nl2,
            (GenericList::Point(pl1), GenericList::Point(pl2)) => pl1 == pl2,
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
            GenericList::Number(nl) => write!(f, "NumberList({:?})", nl),
            GenericList::Point(pl) => write!(f, "PointList({:?})", pl),
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
            GenericList::Number(_) => "NumberList",
            GenericList::Point(_) => "PointList",
        }
    }
}
