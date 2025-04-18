#![allow(type_alias_bounds, clippy::type_complexity)]
pub mod expressions;
pub mod lang;

#[cfg(test)]
mod tests {

    use std::fmt::Debug;

    use crate::{
        expressions::ExpressionId,
        lang::{
            codegen::backend::{
                compile_expressions, compiled_exprs::CompiledExpr, jit::ExplicitFn,
                jit::ExplicitJitFn, jit::JitValue, jit::PointValue, llvm::LLVMBackend,
            },
            expr::Expr,
            generic_value::{GenericList, GenericValue},
        },
    };

    use super::expressions::Expressions;
    use anyhow::Result;
    use inkwell::context::Context;

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
        // Set up the context and expressions
        let context = Context::create();
        let mut expressions = Expressions::new();
        let id = ExpressionId(0);
        expressions.insert_expr(id, input)?;

        // Compile the expressions
        let backend = LLVMBackend::new(&context);
        let compiled = compile_expressions(&backend, &expressions);

        // Handle compilation errors
        if !compiled.errors.is_empty() {
            panic!(
                "Compilation errors in '{}': {:?}",
                test_name, compiled.errors
            );
        }

        // Get the compiled expression
        let expr = &compiled.compiled[&id];
        match expr {
            CompiledExpr::Explicit { lhs } => {
                let x = inputs.last().copied().unwrap_or(0.0);

                let result = match lhs {
                    ExplicitJitFn::Number(lhs) => lhs.call(x),
                    ExplicitJitFn::List(_) => {
                        panic!(
                            "List results are not supported in this test for '{}'",
                            test_name
                        );
                    }
                    ExplicitJitFn::Point(_) => {
                        panic!(
                            "Point results are not supported in this test for '{}'",
                            test_name
                        );
                    }
                };

                assert_eq!(
                    result, expected,
                    "Test '{}' failed: expected {}, got {}",
                    test_name, expected, result
                );
            }
            CompiledExpr::Constant { value } => {
                let result = match value {
                    JitValue::Number(val) => *val,
                    JitValue::List(_) => {
                        panic!(
                            "List results are not supported in this test for '{}'",
                            test_name
                        );
                    }
                    JitValue::Point(_) => {
                        panic!(
                            "Point results are not supported in this test for '{}'",
                            test_name
                        );
                    }
                };

                assert_eq!(
                    result, expected,
                    "Test '{}' failed: expected {}, got {}",
                    test_name, expected, result
                );
            }
            _ => {
                panic!(
                    "Test '{}' failed: Expected an explicit expression but found an implicit one",
                    test_name
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
        let context = Context::create();
        let mut expressions = Expressions::new();

        for (i, (input, _)) in test_cases.iter().enumerate() {
            expressions.insert_expr(ExpressionId(i as u32), input)?;
        }

        let backend = LLVMBackend::new(&context);

        let compiled = compile_expressions(&backend, &expressions);

        if !compiled.errors.is_empty() {
            panic!("Compilation errors: {:?}", compiled.errors);
        }

        for (i, (input, inputs)) in test_cases.iter().enumerate() {
            let id = ExpressionId(i as u32);
            let expr = &expressions
                .exprs
                .get(&id)
                .expect("Expression not found")
                .expr;

            if let Expr::FnDef { .. } = expr {
                continue;
            }

            let compiled_expr = &compiled.compiled[&id];
            match compiled_expr {
                CompiledExpr::Implicit { lhs, rhs, .. } => {
                    let (x, y) = {
                        let mut inputs_iter = inputs.iter();
                        let x = *inputs_iter.next().unwrap_or(&0.0);
                        let y = *inputs_iter.next().unwrap_or(&0.0);
                        (x, y)
                    };

                    match (lhs.call_implicit(x, y), rhs.call_implicit(x, y)) {
                        (GenericValue::Number(lhs), GenericValue::Number(rhs)) => {
                            assert_eq!(
                                lhs, rhs,
                                "Test '{}' failed: lhs_result ({}) != rhs_result ({})",
                                input, lhs, rhs
                            );
                        }
                        (GenericValue::List(lhs), GenericValue::List(rhs)) => {
                            match (lhs, rhs) {
                                (GenericList::Number(lhs_fn), GenericList::Number(rhs_fn)) => {
                                    compare_lists::<f64>(&lhs_fn, &rhs_fn, input)?
                                }
                                (GenericList::Point(lhs_fn), GenericList::Point(rhs_fn)) => {
                                    compare_lists::<PointValue>(&lhs_fn, &rhs_fn, input)?
                                }
                                _ => panic!("Incompatible function types"),
                            };
                        }
                        (GenericValue::Number(lhs), GenericValue::List(rhs)) => {
                            match rhs {
                                GenericList::Number(rhs_list) => {
                                    compare_number_with_list(lhs, &rhs_list, input)
                                }
                                GenericList::Point(_) => {
                                    panic!("Cant compare number with point list")
                                }
                            };
                        }
                        (GenericValue::List(lhs), GenericValue::Number(rhs)) => {
                            match lhs {
                                GenericList::Number(lhs_list) => {
                                    compare_number_with_list(rhs, &lhs_list, input)
                                }
                                GenericList::Point(_) => {
                                    panic!("Cant compare number with point list")
                                }
                            };
                        }
                        (GenericValue::Point(lhs), GenericValue::Point(rhs)) => {
                            assert_eq!(
                                lhs, rhs,
                                "Test '{}' failed: lhs_result ({:?}) != rhs_result ({:?})",
                                input, lhs, rhs
                            );
                        }
                        _ => panic!("Point cannot be compared with non-points"),
                    }
                }
                CompiledExpr::Explicit { .. } => {
                    panic!("Test '{}' failed: Expected an implicit expression or a function/variable depth, found Explicit expression", input);
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn compare_lists<T: PartialEq + Debug>(
        lhs_list: &[T],
        rhs_list: &[T],
        name: &str,
    ) -> Result<()> {
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

        Ok(())
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
