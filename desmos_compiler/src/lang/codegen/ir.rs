use std::{
    collections::HashMap,
    fmt::{self, Debug, Display, Formatter},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IRType {
    Scaler(IRScalerType),
    List(IRScalerType),
}

impl Display for IRType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IRType::Scaler(inner) => write!(f, "{inner}"),
            IRType::List(inner) => write!(f, "Vec<{inner}>"),
        }
    }
}

impl IRType {
    pub const NUMBER: IRType = IRType::Scaler(IRScalerType::Number);
    pub const POINT: IRType = IRType::Scaler(IRScalerType::Point);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IRScalerType {
    Number,
    Point,
}

impl Display for IRScalerType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IRScalerType::Number => write!(f, "f64"),
            IRScalerType::Point => write!(f, "Point"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockID(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct InstID {
    block: BlockID,
    inst: usize,
    t: IRType,
}

impl InstID {
    pub fn new(block: BlockID, inst: usize, t: IRType) -> Self {
        Self { block, inst, t }
    }

    pub fn block(&self) -> BlockID {
        self.block
    }

    pub fn inst(&self) -> usize {
        self.inst
    }

    pub fn ty(&self) -> IRType {
        self.t
    }

    pub fn with_type(&self, t: IRType) -> Self {
        InstID {
            block: self.block,
            inst: self.inst,
            t,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // f64 -> Number
    Number(f64),

    // Number, Number -> Point
    Point(InstID, InstID),

    // Vec<Number> -> NumberList
    NumberList(Vec<InstID>),

    // Point
    PointList(Vec<InstID>),

    Extract(InstID, usize),
    Index(InstID, InstID),

    Map {
        lists: Vec<InstID>,
        args: Vec<InstID>,
        block_id: BlockID,
    },

    Add(InstID, InstID),
    Sub(InstID, InstID),
    Mul(InstID, InstID),
    Div(InstID, InstID),
    Pow(InstID, InstID),

    Neg(InstID),
    Sqrt(InstID),
    Sin(InstID),
    Cos(InstID),
    Tan(InstID),

    Call {
        func: String,
        args: Vec<InstID>,
    },

    FnArg {
        index: usize,
    },

    BlockArg {
        index: usize,
    },
}

#[derive(Debug)]
pub struct IRBlock {
    instructions: Vec<Instruction>,
    ret: IRType,
}

impl Default for IRBlock {
    fn default() -> Self {
        Self::new()
    }
}

impl IRBlock {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            ret: IRType::NUMBER,
        }
    }

    pub fn push(&mut self, instr: Instruction, t: IRType) -> usize {
        let idx = self.instructions.len();
        self.instructions.push(instr);

        self.ret = t;
        idx
    }

    pub fn get(&self, index: usize) -> Option<&Instruction> {
        self.instructions.get(index)
    }

    pub fn ret(&self) -> IRType {
        self.ret
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &Instruction)> {
        self.instructions.iter().enumerate()
    }

    pub fn insts(&self) -> &[Instruction] {
        &self.instructions
    }
}

#[derive(Debug)]
pub struct IRSegment {
    blocks: Vec<IRBlock>,

    pub entry_block: Option<BlockID>,
    ret: Option<InstID>,
}

impl Default for IRSegment {
    fn default() -> Self {
        Self::new()
    }
}

impl IRSegment {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            entry_block: None,
            ret: None,
        }
    }

    pub fn entry_block(&self) -> Option<&IRBlock> {
        self.entry_block.and_then(|id| self.blocks.get(id.0))
    }

    pub fn create_block(&mut self) -> BlockID {
        let id = BlockID(self.blocks.len());
        self.entry_block.get_or_insert(id);
        self.blocks.push(IRBlock::new());
        id
    }

    pub fn ret(&self) -> Option<InstID> {
        self.ret
    }

    pub fn push(&mut self, block: BlockID, instr: Instruction, t: IRType) -> InstID {
        let inst = self.blocks[block.0].push(instr, t);
        let id = InstID::new(block, inst, t);

        if Some(block) == self.entry_block {
            self.ret = Some(id);
        }
        id
    }

    pub fn get(&self, id: InstID) -> Option<&Instruction> {
        self.blocks.get(id.block().0)?.get(id.inst())
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
        self.ret = None;
        self.entry_block = None;
    }

    pub fn blocks(&self) -> &Vec<IRBlock> {
        &self.blocks
    }

    pub fn len(&self) -> usize {
        self.blocks.iter().map(|b| b.instructions.len()).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.blocks.iter().all(|b| b.instructions.is_empty())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SegmentKey {
    pub name: String,
    pub args: Vec<IRType>,
}

impl Display for SegmentKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // print the segment name
        write!(f, "{}(", self.name)?;

        // print the argument list, comma‑separated
        for (i, arg) in self.args.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?; // add comma between args
            }
            write!(f, "{}", arg)?; // `arg` must itself implement Display
        }

        write!(f, ")")
    }
}

impl SegmentKey {
    pub fn new(name: impl Into<String>, args: Vec<IRType>) -> Self {
        Self {
            name: name.into(),
            args,
        }
    }

    pub fn as_ref(&self) -> (&str, &[IRType]) {
        (&self.name, &self.args)
    }
}

#[derive(Debug)]
pub struct IRModule {
    segments: HashMap<SegmentKey, IRSegment>,
}

impl Default for IRModule {
    fn default() -> Self {
        Self::new()
    }
}

impl IRModule {
    pub fn new() -> Self {
        Self {
            segments: HashMap::new(),
        }
    }

    /// Returns a reference to a segment by name and argument types
    pub fn get_segment(&self, key: &SegmentKey) -> Option<&IRSegment> {
        self.segments.get(key)
    }

    /// Returns a mutable reference to a segment by name and argument types
    pub fn get_segment_mut(&mut self, key: &SegmentKey) -> Option<&mut IRSegment> {
        self.segments.get_mut(key)
    }

    pub fn insert_segment(&mut self, key: SegmentKey, segment: IRSegment) {
        self.segments.insert(key, segment);
    }

    pub fn remove_segment(&mut self, key: &SegmentKey) {
        self.segments.remove(key);
    }

    /// Returns an iterator over all segments
    pub fn iter_segments(&self) -> impl Iterator<Item = (&SegmentKey, &IRSegment)> {
        self.segments.iter()
    }

    /// Returns a mutable iterator over all segments
    pub fn iter_segments_mut(&mut self) -> impl Iterator<Item = (&SegmentKey, &mut IRSegment)> {
        self.segments.iter_mut()
    }
}
