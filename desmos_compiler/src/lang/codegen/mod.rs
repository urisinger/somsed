pub mod backend;
mod bin_op;
pub mod ir;
pub mod ir_type;
mod unary_op;

use std::collections::{HashMap, HashSet};

use anyhow::{anyhow, bail, Context, Result};
use ir::{BlockID, IRModule, IRScalerType, IRSegment, IRType, InstID, Instruction, SegmentKey};

use crate::{
    expressions::{ExpressionId, Expressions},
    lang::expr::{BinaryOp, Expr, Literal, Node},
};

pub struct IRGen<'a> {
    exprs: &'a Expressions,
}

#[derive(Debug)]
struct PendingFn<'a> {
    expr_id: ExpressionId,
    node: &'a Node,
    key: SegmentKey,
}

impl<'a> IRGen<'a> {
    pub fn new(exprs: &'a Expressions) -> Self {
        Self { exprs }
    }

    pub fn exprs(&self) -> &Expressions {
        &self.exprs
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
                Instruction::Const(*value),
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

                Self::codegen_binary_op(segment, arg_types, current_block, lhs, *op, rhs)?
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
                        Self::codegen_binary_op(
                            segment,
                            arg_types,
                            current_block,
                            lhs,
                            BinaryOp::Paran,
                            rhs,
                        )?
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

    pub fn generate_ir(expressions: &Expressions) -> (IRModule, HashMap<ExpressionId, String>) {
        let mut errors = HashMap::new();

        let mut pending = Vec::new();
        let mut seen = HashSet::new();

        for (id, expr) in &expressions.exprs {
            match &expr.expr {
                Expr::Implicit { lhs, rhs, .. } => {
                    let args = vec![IRType::NUMBER, IRType::NUMBER];

                    for (side, node) in [("lhs", lhs), ("rhs", rhs)] {
                        let key =
                            SegmentKey::new(format!("implicit_{}_{}", id.0, side), args.clone());
                        if seen.insert(key.clone()) {
                            pending.push(PendingFn {
                                expr_id: *id,
                                node,
                                key,
                            });
                        }

                        match node.used_functions(expressions) {
                            Ok(used_fns) => {
                                for fn_name in used_fns {
                                    if let Some(Expr::FnDef {
                                        rhs: fn_body,
                                        args: fn_args,
                                        ..
                                    }) = expressions.get_expr(&fn_name)
                                    {
                                        let arg_types = vec![IRType::NUMBER; fn_args.len()];
                                        let fn_key = SegmentKey::new(fn_name.clone(), arg_types);
                                        if seen.insert(fn_key.clone()) {
                                            pending.push(PendingFn {
                                                expr_id: *id,
                                                node: fn_body,
                                                key: fn_key,
                                            });
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                errors.insert(*id, e.to_string());
                            }
                        }
                    }
                }

                Expr::Explicit { lhs } => {
                    let args = if expr.used_idents.contains("x") {
                        vec![IRType::NUMBER]
                    } else {
                        vec![]
                    };
                    let key = SegmentKey::new(format!("explicit_{}", id.0), args.clone());
                    if seen.insert(key.clone()) {
                        pending.push(PendingFn {
                            expr_id: *id,
                            node: lhs,
                            key,
                        });
                    }

                    match lhs.used_functions(expressions) {
                        Ok(used_fns) => {
                            for fn_name in used_fns {
                                if let Some(Expr::FnDef {
                                    rhs: fn_body,
                                    args: fn_args,
                                    ..
                                }) = expressions.get_expr(&fn_name)
                                {
                                    let arg_types = vec![IRType::NUMBER; fn_args.len()];
                                    let fn_key = SegmentKey::new(fn_name.clone(), arg_types);
                                    if seen.insert(fn_key.clone()) {
                                        pending.push(PendingFn {
                                            expr_id: *id,
                                            node: fn_body,
                                            key: fn_key,
                                        });
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            errors.insert(*id, e.to_string());
                        }
                    }
                }

                Expr::VarDef { rhs, ident } if expr.used_idents.is_empty() => {
                    let key = SegmentKey::new(ident.to_string(), vec![]);
                    if seen.insert(key.clone()) {
                        pending.push(PendingFn {
                            expr_id: *id,
                            node: rhs,
                            key,
                        });
                    }

                    match rhs.used_functions(expressions) {
                        Ok(used_fns) => {
                            for fn_name in used_fns {
                                if let Some(Expr::FnDef {
                                    rhs: fn_body,
                                    args: fn_args,
                                    ..
                                }) = expressions.get_expr(&fn_name)
                                {
                                    let arg_types = vec![IRType::NUMBER; fn_args.len()];
                                    let fn_key = SegmentKey::new(fn_name.clone(), arg_types);
                                    if seen.insert(fn_key.clone()) {
                                        pending.push(PendingFn {
                                            expr_id: *id,
                                            node: fn_body,
                                            key: fn_key,
                                        });
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            errors.insert(*id, e.to_string());
                        }
                    }
                }

                _ => continue,
            }
        }

        dbg!(&pending);

        let mut module = IRModule::new();
        let mut codegen = IRGen::new(&expressions);

        for PendingFn { expr_id, node, key } in pending {
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
