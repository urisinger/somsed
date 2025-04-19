use anyhow::Result;
use builder::CraneliftBuilder;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncOrDataId, Linkage, Module};
use functions::{import_symbols, ImportedFunctions};

use desmos_compiler::lang::codegen::ir::{IRModule, IRScalerType, IRType, SegmentKey};

pub mod builder;
pub mod jit;
pub mod value;

mod functions;

pub struct CraneliftBackend {
    module: JITModule,
    functions: ImportedFunctions,
}

impl CraneliftBackend {
    pub fn new() -> Result<Self> {
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;

        import_symbols(&mut builder);

        let mut module = JITModule::new(builder);

        let functions = ImportedFunctions::new(&mut module)?;

        Ok(Self { module, functions })
    }

    pub fn make_sig(&self, key: &SegmentKey, ret_type: IRType) -> Signature {
        let mut signature = self.module.make_signature();

        use IRScalerType::*;
        use IRType::*;
        for ty in &key.args {
            match ty {
                Scaler(Number) => {
                    signature.params.push(AbiParam::new(types::F64));
                }
                Scaler(Point) => {
                    signature.params.push(AbiParam::new(types::F64));
                    signature.params.push(AbiParam::new(types::F64));
                }
                List(_) => {
                    signature.params.push(AbiParam::new(types::I64));
                    signature.params.push(AbiParam::new(types::I64));
                }
            };
        }

        match ret_type {
            Scaler(Number) => {
                signature.returns.push(AbiParam::new(types::F64));
            }
            Scaler(Point) => {
                signature.returns.push(AbiParam::new(types::F64));
                signature.returns.push(AbiParam::new(types::F64));
            }
            List(_) => {
                signature.returns.push(AbiParam::new(types::I64));
                signature.returns.push(AbiParam::new(types::I64));
            }
        }
        signature
    }

    pub fn compile_module(&mut self, ir_module: &IRModule) -> Result<()> {
        // we must do everything in 2 passes
        for (key, segment) in ir_module.iter_segments() {
            let signature =
                self.make_sig(key, segment.ret().expect("segment cannot be empty").ty());
            let name = key.to_string();
            _ = self
                .module
                .declare_function(&name, Linkage::Export, &signature)
                .expect("failed to declare function, this indicates a bug");
        }

        let mut ctx = codegen::Context::new();
        let mut builder_ctx = FunctionBuilderContext::new();

        for (key, segment) in ir_module.iter_segments() {
            let name = key.to_string();
            let func_id = if let FuncOrDataId::Func(id) = self
                .module
                .get_name(&name)
                .expect("Function should have been generated")
            {
                id
            } else {
                panic!("Entry should not be data")
            };

            ctx.func.signature =
                self.make_sig(key, segment.ret().expect("segment cannot be empty").ty());

            let builder = CraneliftBuilder::new(
                self,
                ir_module,
                FunctionBuilder::new(&mut ctx.func, &mut builder_ctx),
                &key.args,
            );

            builder.build_fn(segment)?;

            println!("{}", &ctx.func.display());
            self.module.define_function(func_id, &mut ctx)?;
            self.module.clear_context(&mut ctx);
        }

        self.module
            .finalize_definitions()
            .expect("failed to finilize module");

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use cranelift_module::Module;
    use desmos_compiler::{
        expressions::{ExpressionId, Expressions},
        lang::{
            codegen::{
                backend::{
                    jit::{ExplicitFn, ExplicitJitFn, JitValue, PointValue},
                    ExecutionEngine,
                },
                ir::{IRType, SegmentKey},
            },
            expr::Expr,
        },
    };

    use anyhow::Result;
    use desmos_compiler::lang::codegen::IRGen;

    use crate::CraneliftBackend;

    macro_rules! generate_explicit_tests {
    (
        $(
            $name:ident: $input:expr => $expected:expr $(, inputs = [$($input_vals:expr),*])?
        );* $(;)?
    ) => {
        $(
            #[test]
            fn $name() -> Result<()> {
                let inputs = vec![$($($input_vals),*)?];
                run_explicit_test(stringify!($name), $input, $expected, inputs)
            }
        )*
    };
}

    fn run_explicit_test(
        test_name: &str,
        input: &str,
        expected: f64,
        inputs: Vec<f64>,
    ) -> Result<()> {
        let mut expressions = Expressions::new();
        let id = ExpressionId(0);
        expressions.insert_expr(id, input)?;

        let (ir, errors) = IRGen::generate_ir(&expressions);
        let mut backend = CraneliftBackend::new().unwrap();
        backend.compile_module(&ir)?;

        // Handle compilation errors
        if !errors.is_empty() {
            panic!("Compilation errors in '{}': {:?}", test_name, errors);
        }

        let args = vec![IRType::NUMBER; inputs.len()];

        let key = SegmentKey::new("explicit_0".to_string(), args);
        let ty = ir
            .get_segment(&key)
            .and_then(|segment| segment.ret())
            .unwrap()
            .ty();

        let x = inputs.last().copied().unwrap_or(0.0);
        let result = match backend.get_explicit_fn(&key.to_string(), &ty) {
            Some(ExplicitJitFn::Number(lhs)) => lhs.call(x),
            Some(ExplicitJitFn::Point(_)) => {
                panic!(
                    "Point results are not supported in this test for '{}'",
                    test_name
                );
            }

            Some(_) => {
                panic!(
                    "List results are not supported in this test for '{}'",
                    test_name
                );
            }
            None => match backend
                .eval(&key.name, &ty)
                .expect("expected either Constant or explicit value")
            {
                JitValue::Number(val) => val,
                JitValue::Point(_) => {
                    panic!(
                        "Point results are not supported in this test for '{}'",
                        test_name
                    );
                }

                _ => {
                    panic!(
                        "List results are not supported in this test for '{}'",
                        test_name
                    );
                }
            },
        };
        assert_eq!(
            result, expected,
            "Test '{}' failed: expected {}, got {}",
            test_name, expected, result
        );

        Ok(())
    }

    macro_rules! generate_implicit_test_groups {
        (
            $(
                $group_name:ident: {
                    $(
                        $input:expr, inputs = [$($input_vals:expr),*];
                    )*
                }
            )*
        ) => {
            $(
                #[test]
                fn $group_name() -> Result<()> {
                    // Create the group-wide input vector (if any)
                    let test_cases = vec![$(($input, vec![$($input_vals),*])),*];
                    run_implicit_tests(test_cases)
                }
            )*
        };
    }

    fn run_implicit_tests(test_cases: Vec<(&str, Vec<f64>)>) -> Result<()> {
        let mut expressions = Expressions::new();

        for (i, (input, _)) in test_cases.iter().enumerate() {
            expressions.insert_expr(ExpressionId(i as u32), input)?;
        }

        let (ir, errors) = IRGen::generate_ir(&expressions);
        if !errors.is_empty() {
            panic!("Compilation errors: {:?}", errors);
        }

        let mut backend = CraneliftBackend::new()?;
        backend.compile_module(&ir)?;

        for (i, (input, inputs)) in test_cases.iter().enumerate() {
            let id = ExpressionId(i as u32);
            let expr = &expressions.exprs[&id].expr;

            let (x, y) = {
                let mut it = inputs.iter();
                (*it.next().unwrap_or(&0.0), *it.next().unwrap_or(&0.0))
            };

            match expr {
                Expr::Implicit { .. } => {
                    let args = vec![IRType::NUMBER, IRType::NUMBER];
                    let lhs_key = SegmentKey::new(format!("implicit_{}_lhs", id.0), args.clone());
                    let rhs_key = SegmentKey::new(format!("implicit_{}_rhs", id.0), args);

                    let lhs_ty = ir.get_segment(&lhs_key).and_then(|s| s.ret()).unwrap().ty();
                    let rhs_ty = ir.get_segment(&rhs_key).and_then(|s| s.ret()).unwrap().ty();

                    let lhs_fn = backend
                        .get_implicit_fn(&lhs_key.to_string(), &lhs_ty)
                        .expect("lhs eval failed");
                    let rhs_fn = backend
                        .get_implicit_fn(&rhs_key.to_string(), &rhs_ty)
                        .expect("rhs eval failed");

                    match (lhs_fn.call_implicit(x, y), rhs_fn.call_implicit(x, y)) {
                        (JitValue::Number(lhs), JitValue::Number(rhs)) => {
                            assert_eq!(
                                lhs, rhs,
                                "Test '{}' failed: lhs_result ({}) != rhs_result ({})",
                                input, lhs, rhs
                            );
                        }
                        (JitValue::NumberList(lhs), JitValue::NumberList(rhs)) => {
                            compare_lists::<f64>(&lhs, &rhs, input)
                        }
                        (JitValue::PointList(lhs), JitValue::PointList(rhs)) => {
                            compare_lists::<PointValue>(&lhs, &rhs, input)
                        }
                        (JitValue::Number(number), JitValue::NumberList(list))
                        | (JitValue::NumberList(list), JitValue::Number(number)) => {
                            compare_number_with_list(number, &list, input)
                        }
                        (JitValue::Point(lhs), JitValue::Point(rhs)) => {
                            assert_eq!(
                                lhs, rhs,
                                "Test '{}' failed: lhs_result ({:?}) != rhs_result ({:?})",
                                input, lhs, rhs
                            );
                        }
                        (lhs, rhs) => panic!(
                            "Incompatible result types in test '{}': {:?} vs {:?}",
                            input, lhs, rhs
                        ),
                    }
                }

                Expr::Explicit { .. } => {
                    panic!(
                        "Test '{}' failed: Expected an implicit expression, got explicit",
                        input
                    );
                }

                _ => continue,
            }
        }

        Ok(())
    }

    fn compare_lists<T: PartialEq + Debug>(lhs_list: &[T], rhs_list: &[T], name: &str) {
        // Ensure list sizes match
        if lhs_list.len() != rhs_list.len() {
            panic!(
                "Test '{}' failed: List sizes differ (lhs_size: {}, rhs_size: {})",
                name,
                lhs_list.len(),
                rhs_list.len()
            );
        }

        for i in 0..lhs_list.len() {
            let lhs_elem = &lhs_list[i];
            let rhs_elem = &rhs_list[i];
            if lhs_elem != rhs_elem {
                panic!(
                    "Test '{}' failed at index {}: lhs_elem ({:?}) != rhs_elem ({:?})",
                    name, i, lhs_elem, rhs_elem
                );
            }
        }
    }

    fn compare_number_with_list<T: PartialEq + Debug>(number: T, list: &[T], name: &str) {
        let mut list_vec = Vec::new();
        for elem in list {
            list_vec.push(elem);
            if number == *elem {
                return;
            }
        }

        panic!(
            "Test '{}' failed becuase no number is equal to it: number ({:?}) != list_elem ({:?})",
            name, number, list_vec
        );
    }

    generate_implicit_test_groups! {
        test_list_equality:{ "[x, y] = [1, 2]", inputs = [1.0, 2.0];}

        test_number_vs_list_rhs:{ "x = [1, 1]", inputs = [1.0];}

        test_list_vs_number_lhs:{ "[x, x] = 2", inputs = [2.0];}

        test_function_list_equality:{
            "f(n) = [n, n+1]", inputs = [];
            "g(n) = [n, n+1]", inputs = [];
            "f(3) = [3, 4]", inputs = [1.0];
            "f(x+1) = g(x+1)", inputs = [1.0];
        }

        test_number_vs_list_mismatch:{ "x = [1, 2]", inputs = [1.0];}

        test_list_element_mismatch:{ "[x, y] = [1, 3]", inputs = [1.0, 3.0];}

        test_big_list:{ "[x, y, x,x,x,x,x,x,x,x,x,x,x,x,x,x,x] = [1, 3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]", inputs = [1.0, 3.0];}

        // Basic Relationships
        test_implicit_addition:{ "x + y = 15", inputs = [10.0, 5.0];}
        test_implicit_multiplication:{ "x * y = 100", inputs = [10.0, 10.0];}
        test_implicit_power:{ "x^{2} = y", inputs = [3.0, 9.0];}
        test_implicit_division:{ "y / x = 5", inputs = [5.0, 25.0];}

        // Pythagoras
        test_implicit_pythagoras:{ "x^{2} + y^{2} = 25", inputs = [3.0, 4.0];}
        test_implicit_scaled_pythagoras:{ "3x^{2} + 4y^{2} = 171", inputs = [3.0, 6.0];}

        // Fractions
        test_implicit_fraction:{ "\\frac{x}{y} = 2", inputs = [4.0, 2.0];}

        test_implicit_list:{ "[1,2,3]=[x+1,y,x+y+1]", inputs = [0.0,2.0];}

        test_square_function:{
            "s(z) = z^{2}", inputs = [];
            "s(x) + s(y) = 25", inputs = [3.0, 4.0];
        }

        // Function Usage
        test_fraction_function:{
            "f(x, y) = \\frac{x + y}{2}", inputs = [];
            "f(x, y) + 0 = 5", inputs = [6.0, 4.0];
        }
    }

    generate_explicit_tests! {
        // Basic Arithmetic
        test_simple_addition: "2 + 2" => 4.0;
        test_subtraction: "7 - 4" => 3.0;
        test_multiplication: "6 * 7" => 42.0;
        test_division: "15 / 3" => 5.0;
        test_power: "2^{4}" => 16.0;
        test_combined_operations: "3 + 5 * 2 - 4 / 2" => 11.0;

        // Variables
        test_variable: "x * 3" => 30.0, inputs = [10.0];
        test_variable_addition: "x + 5" => 12.0, inputs = [7.0];
        test_variable_power: "x^{2}" => 49.0, inputs = [7.0];
        test_variable_division: "10 / x" => 2.0, inputs = [5.0];
    }
}
