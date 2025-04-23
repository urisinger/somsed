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
}

impl<'a> IRGen<'a> {
    pub fn new(exprs: &'a Expressions) -> Self {
        Self { exprs }
    }

    pub fn exprs(&self) -> &Expressions {
        self.exprs
    }

    pub fn codegen_node(
        &mut self,
        segment: &mut IRSegment,
        arg_types: &[IRType],
        current_block: BlockID,
        expr: &Node,
    ) -> Result<InstID> {
        Ok(match expr {
            Node::Lit(Literal::Float(value)) => segment.push(
                current_block,
                Instruction::Number(*value),
                IRType::Scaler(IRScalerType::Number),
            ),
            Node::Lit(Literal::List(elements)) => {
                let insts = elements
                    .iter()
                    .map(|elem| self.codegen_node(segment, arg_types, current_block, elem))
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
                let val = self.codegen_node(segment, arg_types, current_block, val)?;

                segment.push(
                    current_block,
                    Instruction::Extract(val, *index),
                    IRType::Scaler(IRScalerType::Number),
                )
            }
            Node::Index { list, index } => {
                let list = self.codegen_node(segment, arg_types, current_block, list)?;
                let index = self.codegen_node(segment, arg_types, current_block, index)?;

                let ty = match list.ty() {
                    IRType::List(scaler) => IRType::Scaler(scaler),
                    IRType::Scaler(_) => bail!("cannot index scaler"),
                };

                segment.push(current_block, Instruction::Index(list, index), ty)
            }
            Node::Lit(Literal::Point(x, y)) => {
                let x = self.codegen_node(segment, arg_types, current_block, x)?;
                let y = self.codegen_node(segment, arg_types, current_block, y)?;

                let number_type = IRType::Scaler(IRScalerType::Number);

                if x.ty() != number_type || y.ty() != number_type {
                    bail!("Point must have number types only")
                }

                segment.push(
                    current_block,
                    Instruction::Point(x, y),
                    IRType::Scaler(IRScalerType::Point),
                )
            }
            Node::Ident(ident) => self.get_var(segment, arg_types, current_block, ident)?,
            Node::BinOp { lhs, op, rhs } => {
                let lhs = self.codegen_node(segment, arg_types, current_block, lhs)?;
                let rhs = self.codegen_node(segment, arg_types, current_block, rhs)?;

                Self::codegen_binary_op(segment, current_block, lhs, *op, rhs)?
            }
            Node::UnaryOp { val, op } => {
                let lhs = self.codegen_node(segment, arg_types, current_block, val)?;

                Self::codegen_unary_op(segment, arg_types, current_block, lhs, *op)?
            }
            Node::FnCall { ident, args } => match self.exprs.get_expr(ident) {
                Some(Expr::FnDef { rhs, .. }) => {
                    let args = args
                        .iter()
                        .map(|arg| self.codegen_node(segment, arg_types, current_block, arg))
                        .collect::<Result<Vec<_>>>()?;

                    let types: Vec<_> = args.iter().map(InstID::ty).collect();

                    let ret = rhs.ty(self.exprs, &types)?;
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
                        let lhs = self.get_var(segment, arg_types, current_block, ident)?;
                        let rhs = self.codegen_node(segment, arg_types, current_block, &args[0])?;
                        Self::codegen_binary_op(segment, current_block, lhs, BinaryOp::Paran, rhs)?
                    } else {
                        bail!("{ident} is not a function")
                    }
                }
                None => bail!("unknown ident {ident}"),
                _ => unreachable!("idents should be VarDef or FnDef only"),
            },
            Node::FnArg { index } => segment.push(
                current_block,
                Instruction::FnArg { index: *index },
                *arg_types
                    .get(*index)
                    .ok_or_else(|| anyhow!("function does not have arg {index}"))?,
            ),
        })
    }

    pub fn get_var(
        &mut self,
        segment: &mut IRSegment,
        arg_types: &[IRType],
        current_block: BlockID,
        ident: &str,
    ) -> Result<InstID> {
        match self
            .exprs
            .get_expr(ident)
            .context(anyhow!("Cannot find expr {ident}"))?
        {
            Expr::VarDef { rhs, .. } => self.codegen_node(segment, arg_types, current_block, rhs),
            _ => bail!("Expr is not of type VarDef"),
        }
    }

    pub fn compile_fn(&mut self, node: &Node, arg_types: &[IRType]) -> Result<IRSegment> {
        let mut segment = IRSegment::new();
        let entry_block = segment.create_block();

        _ = self.codegen_node(&mut segment, arg_types, entry_block, node)?;

        Ok(segment)
    }

    fn enqueue_with_dependencies<'b>(
        expressions: &'b Expressions,
        errors: &mut HashMap<ExpressionId, String>,
        pending: &mut HashMap<SegmentKey, (ExpressionId, &'b Node)>,
        expr_id: ExpressionId,
        node: &'b Node,
        key: SegmentKey,
        args: &[IRType],
    ) {
        if pending.contains_key(&key) {
            return;
        }

        pending.insert(key.clone(), (expr_id, node));

        match node.used_functions(expressions, args) {
            Ok(used_fns) => {
                for (fn_name, fn_args) in used_fns {
                    if let Some(Expr::FnDef { rhs: fn_body, .. }) = expressions.get_expr(&fn_name) {
                        let fn_key = SegmentKey::new(fn_name.clone(), fn_args.clone());
                        Self::enqueue_with_dependencies(
                            expressions,
                            errors,
                            pending,
                            expr_id,
                            fn_body,
                            fn_key,
                            &fn_args,
                        );
                    }
                }
            }
            Err(e) => {
                errors.insert(expr_id, e.to_string());
            }
        }
    }

    pub fn generate_ir(expressions: &Expressions) -> (IRModule, HashMap<ExpressionId, String>) {
        let mut errors = HashMap::new();
        let mut pending = HashMap::<SegmentKey, (ExpressionId, &Node)>::new();

        for (id, expr) in &expressions.exprs {
            match &expr.expr {
                Expr::Implicit { lhs, rhs, .. } => {
                    let args = vec![IRType::NUMBER, IRType::NUMBER];

                    for (side, node) in [("lhs", lhs), ("rhs", rhs)] {
                        let key =
                            SegmentKey::new(format!("implicit_{}_{}", id.0, side), args.clone());
                        Self::enqueue_with_dependencies(
                            expressions,
                            &mut errors,
                            &mut pending,
                            *id,
                            node,
                            key,
                            &args,
                        );
                    }
                }

                Expr::Explicit { lhs } => {
                    let args = if expr.used_idents.contains("x") {
                        vec![IRType::NUMBER]
                    } else {
                        vec![]
                    };
                    let key = SegmentKey::new(format!("explicit_{}", id.0), args.clone());
                    Self::enqueue_with_dependencies(
                        expressions,
                        &mut errors,
                        &mut pending,
                        *id,
                        lhs,
                        key,
                        &args,
                    );
                }

                Expr::VarDef { rhs, ident } if expr.used_idents.is_empty() => {
                    let args = vec![];
                    let key = SegmentKey::new(ident.to_string(), args.clone());
                    Self::enqueue_with_dependencies(
                        expressions,
                        &mut errors,
                        &mut pending,
                        *id,
                        rhs,
                        key,
                        &args,
                    );
                }

                _ => continue,
            }
        }

        let mut module = IRModule::new();
        let mut codegen = IRGen::new(expressions);

        for (key, (expr_id, node)) in pending {
            match codegen.compile_fn(node, &key.args) {
                Ok(segment) => module.insert_segment(key, segment),
                Err(e) => {
                    errors.insert(expr_id, e.to_string());
                }
            }
        }

        (module, errors)
    }
}
