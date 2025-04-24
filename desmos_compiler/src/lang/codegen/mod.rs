mod bin_op;
pub mod ir;
pub mod ir_type;
pub mod jit;
mod unary_op;

use std::collections::HashMap;

use anyhow::{anyhow, bail, Context, Result};
use ir::{BlockID, IRModule, IRScalerType, IRSegment, IRType, InstID, Instruction, SegmentKey};

use crate::{
    expressions::{ExpressionId, Expressions},
    lang::parser::ast::{BinaryOperator, Expression, ExpressionListEntry},
};

use super::parser::ast::ChainedComparison;

pub struct IRGen<'a> {
    exprs: &'a Expressions,
}

#[derive(Default)]
struct Scope {
    types: HashMap<String, IRType>,
    args: HashMap<String, Instruction>,
}

impl Scope {
    pub fn insert(&mut self, name: impl ToString, inst: Instruction, ty: IRType) {
        let name = name.to_string();
        self.types.insert(name.clone(), ty);
        self.args.insert(name, inst);
    }

    pub fn get_ty(&self, name: &str) -> Option<IRType> {
        self.types.get(name).copied()
    }
    pub fn get_arg(&self, name: &str) -> Option<&Instruction> {
        self.args.get(name)
    }

    pub fn get(&self, name: &str) -> Option<(&Instruction, IRType)> {
        Some((self.get_arg(name)?, self.get_ty(name)?))
    }
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
        scope: &Scope,
        current_block: BlockID,
        expr: &Expression,
    ) -> Result<InstID> {
        use Expression::*;
        Ok(match expr {
            Number(value) => segment.push(
                current_block,
                Instruction::Number(*value),
                IRType::Scaler(IRScalerType::Number),
            ),
            Point(x, y) => {
                let x = self.codegen_node(segment, scope, current_block, x)?;
                let y = self.codegen_node(segment, scope, current_block, y)?;
                segment.push(current_block, Instruction::Point(x, y), IRType::POINT)
            }
            List(elements) => {
                let insts = elements
                    .iter()
                    .map(|elem| self.codegen_node(segment, scope, current_block, elem))
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
            /*Extract { val, index } => {
                let val = self.codegen_node(segment, arg_types, current_block, val)?;

                segment.push(
                    current_block,
                    Instruction::Extract(val, *index),
                    IRType::Scaler(IRScalerType::Number),
                )
            }*/
            /*Index { list, index } => {
                let list = self.codegen_node(segment, arg_types, current_block, list)?;
                let index = self.codegen_node(segment, arg_types, current_block, index)?;

                let ty = match list.ty() {
                    IRType::List(scaler) => IRType::Scaler(scaler),
                    IRType::Scaler(_) => bail!("cannot index scaler"),
                };

                segment.push(current_block, Instruction::Index(list, index), ty)
            }*/
            Identifier(ident) => self.get_var(segment, scope, current_block, ident)?,
            /*Node::Lit(Literal::Point(x, y)) => {
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
            }*/
            UnaryOperation { arg, operation } => {
                let arg = self.codegen_node(segment, scope, current_block, arg)?;

                Self::codegen_unary_op(segment, current_block, arg, *operation)?
            }
            BinaryOperation {
                left,
                operation,
                right,
            } => {
                let lhs = self.codegen_node(segment, scope, current_block, left)?;
                let rhs = self.codegen_node(segment, scope, current_block, right)?;

                Self::codegen_binary_op(segment, current_block, lhs, *operation, rhs)?
            }

            CallOrMultiply { callee, args } => match scope.get(callee) {
                Some((inst, ty)) => segment.push(current_block, inst.clone(), ty),
                None => match self.exprs.get_expr(callee) {
                    Some(ExpressionListEntry::FunctionDeclaration {
                        body, parameters, ..
                    }) => {
                        let args = args
                            .iter()
                            .map(|arg| self.codegen_node(segment, scope, current_block, arg))
                            .collect::<Result<Vec<_>>>()?;

                        let types: HashMap<_, _> = parameters
                            .iter()
                            .cloned()
                            .zip(args.iter().map(InstID::ty))
                            .collect();

                        let ret = body.ty(self.exprs, &types)?;
                        segment.push(
                            current_block,
                            Instruction::Call {
                                func: callee.to_string(),
                                args,
                            },
                            ret,
                        )
                    }
                    Some(ExpressionListEntry::Assignment { .. }) => {
                        if args.len() == 1 {
                            let lhs = self.get_var(segment, scope, current_block, &callee)?;
                            let rhs = self.codegen_node(segment, scope, current_block, &args[0])?;
                            Self::codegen_binary_op(
                                segment,
                                current_block,
                                lhs,
                                BinaryOperator::Mul,
                                rhs,
                            )?
                        } else {
                            bail!("{callee} is not a function")
                        }
                    }
                    None => {
                        bail!("No expression found for {callee}")
                    }
                    _ => unreachable!("idents should be VarDef or FnDef only"),
                },
            },
            ListRange {
                before_ellipsis,
                after_ellipsis,
            } => todo!(),
            Call { callee, args } => todo!(),
            ChainedComparison(chained_comparison) => todo!(),
            Piecewise {
                test,
                consequent,
                alternate,
            } => todo!(),
            SumProd {
                kind,
                variable,
                lower_bound,
                upper_bound,
                body,
            } => todo!(),
            With {
                body,
                substitutions,
            } => todo!(),
            For { body, lists } => todo!(),
        })
    }

    fn get_var(
        &mut self,
        segment: &mut IRSegment,
        scope: &Scope,
        current_block: BlockID,
        name: &str,
    ) -> Result<InstID> {
        if let Some((inst, ty)) = scope.get(name) {
            return Ok(segment.push(current_block, inst.clone(), ty));
        }
        match self
            .exprs
            .get_expr(name)
            .context(anyhow!("Cannot find expr {name}"))?
        {
            ExpressionListEntry::Assignment { value, .. } => {
                self.codegen_node(segment, scope, current_block, value)
            }
            _ => bail!("Expr is not of type VarDef"),
        }
    }

    fn compile_fn(
        &mut self,
        node: &Expression,
        types: &[IRType],
        parameters: &[String],
    ) -> Result<IRSegment> {
        let mut segment = IRSegment::new();
        let entry_block = segment.create_block();

        let (types, args) = parameters
            .iter()
            .zip(types.iter())
            .enumerate()
            .map(|(i, (k, ty))| {
                (
                    (k.to_string(), *ty),
                    (k.to_string(), Instruction::FnArg { index: i }),
                )
            })
            .collect();
        let scope = Scope { types, args };

        _ = self.codegen_node(&mut segment, &scope, entry_block, node)?;

        Ok(segment)
    }

    #[allow(clippy::too_many_arguments)]
    fn enqueue_with_dependencies<'b>(
        expressions: &'b Expressions,
        errors: &mut HashMap<ExpressionId, String>,
        pending: &mut HashMap<SegmentKey, (ExpressionId, &'b Expression, &'b [String])>,
        expr_id: ExpressionId,
        node: &'b Expression,
        key: SegmentKey,
        parameters: &'b [String],
        args: &HashMap<String, IRType>,
    ) {
        if pending.contains_key(&key) {
            return;
        }

        pending.insert(key.clone(), (expr_id, node, parameters));

        match node.used_functions(expressions, args) {
            Ok(used_fns) => {
                for (fn_name, fn_args) in used_fns {
                    if let Some(ExpressionListEntry::FunctionDeclaration {
                        body, parameters, ..
                    }) = expressions.get_expr(&fn_name)
                    {
                        let types = parameters
                            .iter()
                            .cloned()
                            .zip(fn_args.iter().copied())
                            .collect();
                        let fn_key = SegmentKey::new(fn_name.clone(), fn_args.clone());
                        Self::enqueue_with_dependencies(
                            expressions,
                            errors,
                            pending,
                            expr_id,
                            body,
                            fn_key,
                            parameters,
                            &types,
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
        let mut pending = HashMap::new();

        let implicit_args = ["x".to_string(), "y".to_string()];

        let explicit_args = ["x".to_string()];
        let constant_args = [];

        for (id, expr) in &expressions.exprs {
            match expr {
                ExpressionListEntry::Relation(ChainedComparison {
                    operands,
                    operators,
                }) => {
                    if operands.len() != 2 || operators.len() != 1 {
                        continue;
                    }

                    let mut types = HashMap::default();

                    types.insert("x".to_string(), IRType::NUMBER);
                    types.insert("y".to_string(), IRType::NUMBER);

                    for (side, node) in [("lhs", &operands[0]), ("rhs", &operands[1])] {
                        let key = SegmentKey::new(
                            format!("implicit_{}_{}", id.0, side),
                            vec![IRType::NUMBER, IRType::NUMBER],
                        );
                        Self::enqueue_with_dependencies(
                            expressions,
                            &mut errors,
                            &mut pending,
                            *id,
                            node,
                            key,
                            &implicit_args,
                            &types,
                        );
                    }
                }

                ExpressionListEntry::Expression(lhs) => {
                    let mut types = HashMap::default();

                    types.insert("x".to_string(), IRType::NUMBER);
                    let key = SegmentKey::new(format!("explicit_{}", id.0), vec![IRType::NUMBER]);
                    Self::enqueue_with_dependencies(
                        expressions,
                        &mut errors,
                        &mut pending,
                        *id,
                        lhs,
                        key,
                        &explicit_args,
                        &types,
                    );
                }

                ExpressionListEntry::Assignment { name, value } => {
                    let key = SegmentKey::new(name.to_string(), vec![]);
                    Self::enqueue_with_dependencies(
                        expressions,
                        &mut errors,
                        &mut pending,
                        *id,
                        value,
                        key,
                        &constant_args,
                        &HashMap::new(),
                    );
                }

                _ => continue,
            }
        }

        let mut module = IRModule::new();
        let mut codegen = IRGen::new(expressions);

        for (key, (expr_id, node, parameters)) in pending {
            match codegen.compile_fn(node, &key.args, parameters) {
                Ok(segment) => module.insert_segment(key, segment),
                Err(e) => {
                    errors.insert(expr_id, e.to_string());
                }
            }
        }

        (module, errors)
    }
}
