pub mod backend;
mod bin_op;
pub mod ir;
pub mod ir_type;
mod unary_op;

use std::collections::HashMap;

use anyhow::{anyhow, bail, Context, Result};
use ir::{BlockID, IRModule, IRScalerType, IRSegment, IRType, InstID, Instruction, SegmentKey};

use crate::{
    expressions::{ExpressionId, Expressions},
    lang::expr::{BinaryOp, Expr, Literal, Node},
};

pub struct IRGen<'a> {
    exprs: &'a Expressions,
    module: &'a mut IRModule,
}

impl<'a> IRGen<'a> {
    pub fn new(exprs: &'a Expressions, module: &'a mut IRModule) -> Self {
        Self { exprs, module }
    }

    pub fn exprs(&self) -> &Expressions {
        &self.exprs
    }

    pub fn codegen_node(
        &mut self,
        segment_key: &SegmentKey,
        current_block: BlockID,
        expr: &Node,
    ) -> Result<InstID> {
        Ok(match expr {
            Node::Lit(Literal::Float(value)) => self
                .module
                .get_segment_mut(segment_key)
                .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?
                .push(
                    current_block,
                    Instruction::Const(*value),
                    IRType::Scaler(IRScalerType::Number),
                ),
            Node::Lit(Literal::List(elements)) => {
                let insts = elements
                    .iter()
                    .map(|elem| self.codegen_node(segment_key, current_block, elem))
                    .collect::<Result<Vec<_>>>()?;

                let first_ty = insts
                    .first()
                    .map(|id| id.ty())
                    .unwrap_or(IRType::Scaler(IRScalerType::Number)); // default for empty list

                match first_ty {
                    IRType::Scaler(scaler_type) if insts.iter().all(|id| id.ty() == first_ty) => {
                        let instr = match scaler_type {
                            IRScalerType::Number => Instruction::NumberList(insts),
                            IRScalerType::Point => Instruction::PointList(insts),
                        };
                        let segment = self
                            .module
                            .get_segment_mut(segment_key)
                            .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;

                        segment.push(current_block, instr, IRType::List(scaler_type))
                    }

                    IRType::Scaler(_) => {
                        bail!("Inconsistent scalar types in list elements");
                    }

                    IRType::List(_) => {
                        bail!("Nested lists are not supported");
                    }
                }
            }
            Node::Extract { val, index } => {
                let val = self.codegen_node(segment_key, current_block, val)?;

                let segment = self
                    .module
                    .get_segment_mut(segment_key)
                    .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;

                segment.push(
                    current_block,
                    Instruction::Extract(val, *index),
                    IRType::Scaler(IRScalerType::Number),
                )
            }
            Node::Lit(Literal::Point(x, y)) => {
                let x = self.codegen_node(segment_key, current_block, x)?;
                let y = self.codegen_node(segment_key, current_block, y)?;

                let number_type = IRType::Scaler(IRScalerType::Number);

                if x.ty() != number_type || y.ty() != number_type {
                    bail!("Point must have number types only")
                }
                let segment = self
                    .module
                    .get_segment_mut(segment_key)
                    .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;

                segment.push(
                    current_block,
                    Instruction::Point(x, y),
                    IRType::Scaler(IRScalerType::Point),
                )
            }
            Node::Ident(ident) => self.get_var(segment_key, current_block, ident)?,
            Node::BinOp { lhs, op, rhs } => {
                let lhs = self.codegen_node(segment_key, current_block, lhs)?;
                let rhs = self.codegen_node(segment_key, current_block, rhs)?;
                let segment = self
                    .module
                    .get_segment_mut(segment_key)
                    .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;
                Self::codegen_binary_op(segment, current_block, lhs, *op, rhs)?
            }
            Node::UnaryOp { val, op } => {
                let lhs = self.codegen_node(segment_key, current_block, val)?;
                let segment = self
                    .module
                    .get_segment_mut(segment_key)
                    .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;
                Self::codegen_unary_op(segment, current_block, lhs, *op)?
            }
            Node::FnCall { ident, args } => match self.exprs.get_expr(ident) {
                Some(Expr::FnDef { rhs, .. }) => {
                    let args = args
                        .iter()
                        .map(|arg| self.codegen_node(segment_key, current_block, arg))
                        .collect::<Result<Vec<_>>>()?;

                    let types: Vec<_> = args.iter().map(InstID::ty).collect();

                    let new_key = SegmentKey::new(ident.to_string(), types);

                    let ret = if let Some(segment) = self.module.get_segment(&new_key) {
                        segment
                            .ret()
                            .expect("module is empty, this indicates a bug")
                            .ty()
                    } else {
                        self.compile_fn(rhs, new_key)?
                    };
                    let segment = self
                        .module
                        .get_segment_mut(segment_key)
                        .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;

                    segment.push(
                        current_block,
                        Instruction::Call {
                            func: ident.clone(),
                            args,
                        },
                        ret,
                    )
                }
                Some(Expr::VarDef { .. }) => {
                    if args.len() == 1 {
                        let lhs = self.get_var(segment_key, current_block, ident)?;
                        let rhs = self.codegen_node(segment_key, current_block, &args[0])?;
                        let segment = self
                            .module
                            .get_segment_mut(segment_key)
                            .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?;
                        Self::codegen_binary_op(segment, current_block, lhs, BinaryOp::Paran, rhs)?
                    } else {
                        bail!("{ident} is not a function")
                    }
                }
                None => bail!("unknown ident {ident}"),
                _ => unreachable!("idents should be VarDef or FnDef only"),
            },
            Node::FnArg { index } => self
                .module
                .get_segment_mut(segment_key)
                .ok_or_else(|| anyhow!("segment not found, this indicates a bug."))?
                .push(
                    current_block,
                    Instruction::FnArg { index: *index },
                    *segment_key
                        .args
                        .get(*index)
                        .ok_or_else(|| anyhow!("function does not have arg {index}"))?,
                ),
        })
    }

    pub fn get_var(
        &mut self,
        segment_key: &SegmentKey,
        current_block: BlockID,
        ident: &str,
    ) -> Result<InstID> {
        match self
            .exprs
            .get_expr(ident)
            .context(anyhow!("Cannot find expr {ident}"))?
        {
            Expr::VarDef { rhs, .. } => self.codegen_node(segment_key, current_block, rhs),
            _ => bail!("Expr is not of type VarDef"),
        }
    }

    pub fn compile_fn(&mut self, node: &Node, segment_key: SegmentKey) -> Result<IRType> {
        let mut segment = IRSegment::new();
        let entry_block = segment.create_block();

        self.module.insert_segment(segment_key.clone(), segment);
        let value = self.codegen_node(&segment_key, entry_block, node)?;

        Ok(value.ty())
    }

    pub fn generate_ir(expressions: &Expressions) -> (IRModule, HashMap<ExpressionId, String>) {
        let mut errors = HashMap::new();
        let mut module = IRModule::new();
        let mut codegen = IRGen::new(expressions, &mut module);

        for (id, expr) in &expressions.exprs {
            match &expr.expr {
                Expr::Implicit { lhs, rhs, .. } => {
                    let args = vec![IRType::NUMBER, IRType::NUMBER];

                    let lhs_name = format!("implicit_{}_lhs", id.0);
                    if let Err(e) = codegen.compile_fn(lhs, SegmentKey::new(lhs_name, args.clone()))
                    {
                        errors.insert(*id, e.to_string());
                    }

                    let rhs_name = format!("implicit_{}_rhs", id.0);
                    if let Err(e) = codegen.compile_fn(rhs, SegmentKey::new(rhs_name, args)) {
                        errors.insert(*id, e.to_string());
                    }
                }

                Expr::Explicit { lhs } => {
                    let name = format!("explicit_{}", id.0);
                    let args = if expr.used_idents.contains("x") {
                        vec![IRType::NUMBER]
                    } else {
                        vec![]
                    };

                    if let Err(e) = codegen.compile_fn(lhs, SegmentKey::new(name, args)) {
                        errors.insert(*id, e.to_string());
                    }
                }

                Expr::VarDef { rhs, ident } => {
                    if !expr.used_idents.is_empty() {
                        continue;
                    }

                    let name = ident.to_string();
                    let args = vec![];

                    if let Err(e) = codegen.compile_fn(rhs, SegmentKey::new(name, args)) {
                        errors.insert(*id, e.to_string());
                    }
                }

                _ => continue,
            }
        }
        (module, errors)
    }
}
