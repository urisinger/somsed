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

    use desmos_compiler::{
        expressions::{ExpressionId, Expressions},
        lang::{
            codegen::{
                ir::{IRType, SegmentKey},
                jit::{
                    function::{ExplicitFn, ExplicitJitFn, JitValue, PointValue},
                    ExecutionEngine,
                },
            },
            parser::ast::{ChainedComparison, ExpressionListEntry},
        },
    };

    use anyhow::Result;
    use desmos_compiler::lang::codegen::IRGen;

    use crate::CraneliftBackend;

    macro_rules! generate_explicit_tests {
        (
            $(
                $name:ident: $input:expr => $expected:tt $(, inputs = [$($input_vals:expr),*])?
            );* $(;)?
        ) => {
            $(
                #[test]
                fn $name() -> Result<()> {
                    let inputs = vec![$($($input_vals),*)?];
                    let expected = parse_expected!($expected);
                    run_explicit_test(stringify!($name), $input, expected, inputs)
                }
            )*
        };
    }

    macro_rules! parse_expected {
    // Match point lists: [(x, y), (x, y), ...]
    ([ $( ( $x:expr, $y:expr ) ),* ]) => {
        ExpectedValue::PointList(vec![ $( ($x, $y) ),* ])
    };
    // Match number lists: [x, y, ...]
    ([ $($val:expr),* ]) => {
        ExpectedValue::NumberList(vec![ $($val),* ])
    };
    // Match single number
    ($val:literal) => {
        ExpectedValue::Number($val)
    };
}

    #[derive(Debug)]
    enum ExpectedValue {
        Number(f64),
        NumberList(Vec<f64>),
        PointList(Vec<(f64, f64)>),
    }

    fn run_explicit_test(
        test_name: &str,
        input: &str,
        expected: ExpectedValue,
        inputs: Vec<f64>,
    ) -> Result<()> {
        let mut expressions = Expressions::new();
        let id = ExpressionId(0);
        expressions.insert_expr(id, input)?;

        let (ir, errors) = IRGen::generate_ir(&expressions);
        let mut backend = CraneliftBackend::new().unwrap();
        backend.compile_module(&ir)?;

        if !errors.is_empty() {
            panic!("Compilation errors in '{}': {:?}", test_name, errors);
        }

        let args = vec![IRType::NUMBER];
        let key = SegmentKey::new("explicit_0".to_string(), args);

        let ty = ir
            .get_segment(&key)
            .and_then(|segment| segment.ret())
            .unwrap()
            .ty();

        let x = inputs.last().copied().unwrap_or(0.0);
        let result = match backend.get_explicit_fn(&key.to_string(), &ty) {
            Some(ExplicitJitFn::Number(lhs)) => JitValue::Number(lhs.call(x)),
            Some(ExplicitJitFn::NumberList(lhs)) => JitValue::NumberList(lhs.call(x)),
            Some(ExplicitJitFn::PointList(lhs)) => JitValue::PointList(lhs.call(x)),
            Some(_) => panic!("Unsupported result type in test '{}'", test_name),
            None => backend
                .eval(&key.name, &ty)
                .expect("expected either Constant or explicit value"),
        };

        match (expected, result) {
            (ExpectedValue::Number(expected_val), JitValue::Number(actual_val)) => {
                assert_eq!(
                    actual_val, expected_val,
                    "Test '{}' failed: expected {}, got {}",
                    test_name, expected_val, actual_val
                );
            }
            (ExpectedValue::NumberList(expected_list), JitValue::NumberList(actual_list)) => {
                compare_lists(&expected_list, &actual_list, test_name);
            }

            (ExpectedValue::PointList(expected_list), JitValue::PointList(actual_list)) => {
                assert_eq!(
                    expected_list.len(),
                    actual_list.len(),
                    "Test '{}' failed: Point list length mismatch",
                    test_name
                );
                for (i, (expected, actual)) in
                    expected_list.iter().zip(actual_list.iter()).enumerate()
                {
                    assert!(
                        (expected.0 - actual.x).abs() < 1e-6
                            && (expected.1 - actual.y).abs() < 1e-6,
                        "Test '{}' failed at index {}: expected {:?}, got {:?}",
                        test_name,
                        i,
                        expected,
                        actual
                    );
                }
            }
            (exp, got) => {
                panic!(
                    "Test '{}' failed: mismatched types. expected {:?}, got {:?}",
                    test_name, exp, got
                );
            }
        }

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
            let expr = &expressions.exprs[&id];

            let (x, y) = {
                let mut it = inputs.iter();
                (*it.next().unwrap_or(&0.0), *it.next().unwrap_or(&0.0))
            };

            match expr {
                ExpressionListEntry::Relation(ChainedComparison { .. }) => {
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

                ExpressionListEntry::Expression { .. } => {
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

        test_list_scalar_add_rhs:{ "[x, y] + 3 = [4, 7]",  inputs = [1.0, 4.0];}
        test_scalar_list_add_lhs:{ "5 + [x, y] = [8, 9]",  inputs = [3.0, 4.0];}

        test_list_scalar_sub_rhs:{ "[x, y] - 2 = [-1, 1]", inputs = [1.0, 3.0];}
        test_scalar_list_sub_lhs:{ "10 - [x, y] = [7, 5]", inputs = [3.0, 5.0];}

        test_list_scalar_mul_rhs:{ "[x, y] * 2 = [2, 8]",  inputs = [1.0, 4.0];}
        test_scalar_list_mul_lhs:{ "2 * [x, y] = [6, 8]",  inputs = [3.0, 4.0];}

        test_list_scalar_div_rhs:{ "[x, y] / 2 = [1, 2]",  inputs = [2.0, 4.0];}
        test_scalar_list_div_lhs:{ "20 / [x, y] = [4, 5]", inputs = [5.0, 4.0];}

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

        // Constant lists
        test_constant_list: "[1, 2, 3]" => [1.0, 2.0, 3.0];

        // List with arithmetic
        test_list_addition: "[1+1, 2+2]" => [2.0, 4.0];
        test_list_multiplication: "[2*3, 4*5]" => [6.0, 20.0];

        // Using input in list expressions
        test_list_with_input: "[x, x+1, x+2]" => [5.0, 6.0, 7.0], inputs = [5.0];


        // Nested expressions in lists
        test_list_nested_ops: "[(x+2)^{2}, (x-1)^{2}]" => [16.0, 1.0], inputs = [2.0];

        // Longer list
        test_longer_list: "[1,2,3,4,5,6,7,8,9,10]" => [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0];


        // Repeated input
        test_list_expression_repeat: "[x, x, x]" => [7.0, 7.0, 7.0], inputs = [7.0];

        test_list_add_list: "[1, 2, 3] + [4, 5, 6]" => [5.0, 7.0, 9.0];
        test_list_sub_list: "[10, 20, 30] - [1, 2, 3]" => [9.0, 18.0, 27.0];
        test_list_mul_list: "[2, 3, 4] * [5, 6, 7]" => [10.0, 18.0, 28.0];
        test_list_div_list: "[8, 9, 10] / [2, 3, 5]" => [4.0, 3.0, 2.0];
        test_list_pow_list: "[2, 3, 4]^{3}" => [8.0, 27.0, 64.0];

        // --- Uneven length list operations (should truncate to smaller length) ---
        test_list_add_shorter: "[1, 2, 3, 4] + [10, 20]" => [11.0, 22.0];
        test_list_sub_shorter: "[10, 20] - [1, 2, 3]" => [9.0, 18.0];

        // --- List ⊕ Scalar ---
        test_list_add_scalar_rhs: "[1, 2, 3] + 5" => [6.0, 7.0, 8.0];
        test_list_sub_scalar_rhs: "[10, 20, 30] - 10" => [0.0, 10.0, 20.0];
        test_list_mul_scalar_rhs: "[1, 2, 3] * 4" => [4.0, 8.0, 12.0];
        test_list_div_scalar_rhs: "[8, 16, 32] / 2" => [4.0, 8.0, 16.0];
        test_list_pow_scalar_rhs: "[2, 3, 4]^{2}" => [4.0, 9.0, 16.0];

        // --- Scalar ⊕ List ---
        test_scalar_add_list_lhs: "10 + [1, 2, 3]" => [11.0, 12.0, 13.0];
        test_scalar_sub_list_lhs: "10 - [1, 2, 3]" => [9.0, 8.0, 7.0];
        test_scalar_mul_list_lhs: "2 * [3, 4, 5]" => [6.0, 8.0, 10.0];
        test_scalar_div_list_lhs: "20 / [2, 4, 5]" => [10.0, 5.0, 4.0];
        test_scalar_pow_list_lhs: "2^{[3, 4, 5]}" => [8.0, 16.0, 32.0];

    test_for_loop_simple_cartesian:
        "(a, b) \\operatorname{for} a = [1, 2], b = [10, 20]" => [(1.0, 10.0), (1.0, 20.0), (2.0, 10.0), (2.0, 20.0)], inputs = [];

    test_for_loop_with_expression:
        "a + b \\operatorname{for} a = [1, 2], b = [3, 4]" => [4.0, 5.0, 5.0, 6.0], inputs = [];


    test_for_loop_triple_nested:
        "(a, b)c \\operatorname{for} a = [1], b = [2], c = [3, 4]" => [(3.0, 6.0), (4.0, 8.0)], inputs = [];


    test_for_loop_expression_triple:
        "a + b + c \\operatorname{for} a = [1], b = [2], c = [3, 4]" => [6.0, 7.0], inputs = [];


    test_for_loop_mixed_scalar_and_list:
        "a + 1 \\operatorname{for} a = [5, 6]" => [6.0, 7.0], inputs = [];


    test_for_loop_expression_output_list:
        "(a, b) \\operatorname{for} a = [1], b = [2]" => [(1.0, 2.0)], inputs = [];
    }
}
