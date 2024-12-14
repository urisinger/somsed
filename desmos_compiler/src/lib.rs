pub mod expressions;
pub mod lang;

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use crate::lang::{backends::llvm::jit::ListLayout, parser::Expr};

    use super::expressions::Expressions;
    use super::lang::backends::llvm::CompiledExpr;
    use anyhow::Result;
    use inkwell::context::Context;

    use super::lang::backends::llvm::{
        codegen::compile_all_exprs,
        jit::{ExplicitJitFn, ImplicitJitFn, ImplicitJitListFn},
    };

    macro_rules! generate_explicit_tests {
        (
            $(
                $name:ident: $input:expr => $expected:expr $(, inputs = [$($input_vals:expr),*])?
            );* $(;)?
        ) => {
            $(
                #[test]
                fn $name() -> Result<()> {

                    // Set up the context and expressions
                    let context = Context::create();
                    let mut expressions = Expressions::new();
                    let id = expressions.add_expr($input)?;
                    // Compile the expressions
                    let compiled = compile_all_exprs(&context, &expressions);

                    // Handle compilation errors
                    if !compiled.errors.is_empty() {
                        panic!("Compilation errors in '{}': {:?}", stringify!($name), compiled.errors);
                    }



                    // Get the compiled expression
                    let expr = &compiled.compiled[&id];
                    match expr {
                        CompiledExpr::Explicit { lhs } => {
                            // Use provided inputs or default to x = 0.0
                            let x = {
                                let mut inputs = vec![$($($input_vals),*)?];
                                inputs.pop().unwrap_or(0.0)
                            };

                            let result = match lhs {
                                ExplicitJitFn::Number(lhs) => unsafe { lhs.call(x)},
                                ExplicitJitFn::List(_) => {
                                    panic!("List results are not supported in this test for '{}'", stringify!($name));
                                }
                            };

                            assert_eq!(result, $expected, "Test '{}' failed: expected {}, got {}", stringify!($name), $expected, result);
                        }
                        _ => {
                            panic!("Test '{}' failed: Expected an explicit expression", stringify!($name));
                        }
                    }

                    Ok(())
                }
            )*
        };
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

    macro_rules! generate_implicit_tests {
        (
            $test_name:ident: {$(
                $name:ident: $input:expr, inputs = [$($input_vals:expr),*];
            )*}
        ) => {
            #[test]
            fn $test_name() -> Result<()> {
                // Collect test cases
                let mut test_cases = Vec::new();
                $(
                    test_cases.push((stringify!($name), $input, vec![$($input_vals),*]));
                )*

                // Initialize the context and expressions
                let context = Context::create();
                let mut expressions = Expressions::new();
                let mut expr_ids = HashMap::new();


                for (name, input, _) in &test_cases {
                    let id = expressions.add_expr(input)?;
                    expr_ids.insert(*name, id);
                }

                let compiled = compile_all_exprs(&context, &expressions);

                if !compiled.errors.is_empty() {
                    panic!("Compilation errors: {:?}", compiled.errors);
                }

                for (name, _, inputs) in &test_cases {
                    let id = expr_ids.get(name).expect("Expression ID not found");
                    let expr = expressions.exprs.get(id).expect("Expression not found");

                    if let Expr::FnDef { .. } = expr {
                        continue;
                    }


                    let compiled_expr = &compiled.compiled[id];
                    match compiled_expr {
                        CompiledExpr::Implicit { lhs, rhs, .. } => {
                            // Use provided inputs
                            let (x, y) = {
                                let mut inputs_iter = inputs.iter();
                                let x = *inputs_iter.next().unwrap_or(&0.0);
                                let y = *inputs_iter.next().unwrap_or(&0.0);
                                (x, y)
                            };

                            // Match over (lhs, rhs) functions
                            match (lhs, rhs) {
                                // Both lhs and rhs return Number
                                (ImplicitJitFn::Number(lhs_fn), ImplicitJitFn::Number(rhs_fn)) => {
                                    let lhs_result = unsafe { lhs_fn.call(x, y) };
                                    let rhs_result = unsafe { rhs_fn.call(x, y) };
                                    assert_eq!(
                                        lhs_result, rhs_result,
                                        "Test '{}' failed: lhs_result ({}) != rhs_result ({})",
                                        name, lhs_result, rhs_result
                                    );
                                }
                                // Both lhs and rhs return List
                                (ImplicitJitFn::List(lhs_fn), ImplicitJitFn::List(rhs_fn)) => {
                                    // Call the functions
                                    let lhs_list = match lhs_fn {
                                        ImplicitJitListFn::Number(lhs_fn) => unsafe { lhs_fn.call(x, y) },
                                    };
                                    let rhs_list = match rhs_fn {
                                        ImplicitJitListFn::Number(rhs_fn) => unsafe { rhs_fn.call(x, y) },
                                    };

                                    // Compare the lists
                                    compare_lists(&lhs_list, &rhs_list, name)?;
                                }
                                // lhs returns Number, rhs returns List
                                (ImplicitJitFn::Number(lhs_fn), ImplicitJitFn::List(rhs_fn)) => {
                                    let lhs_result = unsafe { lhs_fn.call(x, y) };
                                    let rhs_list = match rhs_fn {
                                        ImplicitJitListFn::Number(rhs_fn) => unsafe { rhs_fn.call(x, y) },
                                    };
                                    // Compare lhs_result with each element of rhs_list
                                    compare_number_with_list(lhs_result, &rhs_list, name);
                                }
                                // lhs returns List, rhs returns Number
                                (ImplicitJitFn::List(lhs_fn), ImplicitJitFn::Number(rhs_fn)) => {
                                    let lhs_list = match lhs_fn {
                                        ImplicitJitListFn::Number(lhs_fn) => unsafe { lhs_fn.call(x, y) },
                                    };
                                    let rhs_result = unsafe { rhs_fn.call(x, y) };
                                    compare_number_with_list(rhs_result, &lhs_list, name);
                                }
                            }
                        }
                        _ => {
                            panic!("Test '{}' failed: Expected an implicit expression", name);
                        }
                    }
                }

                Ok(())
            }
        };
    }

    fn compare_lists(lhs_list: &ListLayout, rhs_list: &ListLayout, name: &str) -> Result<()> {
        // Ensure list sizes match
        if lhs_list.size != rhs_list.size {
            panic!(
                "Test '{}' failed: List sizes differ (lhs_size: {}, rhs_size: {})",
                name, lhs_list.size, rhs_list.size
            );
        }

        // Compare list elements
        let lhs_ptr = lhs_list.ptr as *const f64;
        let rhs_ptr = rhs_list.ptr as *const f64;
        for i in 0..lhs_list.size as usize {
            let lhs_elem = unsafe { *lhs_ptr.add(i) };
            let rhs_elem = unsafe { *rhs_ptr.add(i) };
            if lhs_elem != rhs_elem {
                panic!(
                    "Test '{}' failed at index {}: lhs_elem ({}) != rhs_elem ({})",
                    name, i, lhs_elem, rhs_elem
                );
            }
        }

        Ok(())
    }

    fn compare_number_with_list(number: f64, list: &ListLayout, name: &str) {
        let mut list_vec = Vec::new();
        let list_ptr = list.ptr as *const f64;
        for i in 0..list.size as usize {
            let elem = unsafe { *list_ptr.add(i) };

            list_vec.push(elem);
            if number == elem {
                return;
            }
        }

        panic!(
            "Test '{}' failed becuase no number is equal to it: number ({}) != list_elem ({:?})",
            name, number, list_vec
        );
    }

    generate_implicit_tests! {
        simple_tests: {
            // Basic Relationships
            test_implicit_addition: "x + y = 15", inputs = [10.0, 5.0];
            test_implicit_multiplication: "x * y = 100", inputs = [10.0, 10.0];
            test_implicit_power: "x^{2} = y", inputs = [3.0, 9.0];
            test_implicit_division: "y / x = 5", inputs = [5.0, 25.0];

            // Pythagoras
            test_implicit_pythagoras: "x^{2} + y^{2} = 25", inputs = [3.0, 4.0];
            test_implicit_scaled_pythagoras: "3x^{2} + 4y^{2} = 171", inputs = [3.0, 6.0];

            // Fractions
            test_implicit_fraction: "\\frac{x}{y} = 2", inputs = [4.0, 2.0];

            test_implicit_list: "[1,2,3]=[x+1,y,x+y+1]", inputs = [0.0,2.0];

            test_function_definition_square: "s(z) = z^{2}", inputs = [];
            test_function_definition_fraction: "f(x, y) = \\frac{x + y}{2}", inputs = [];

            // Function Usage
            test_square_function: "s(x) + s(y) = 25", inputs = [3.0, 4.0];
            test_pythagoras_function: "p(x, y) = 5", inputs = [3.0, 4.0];
            test_fraction_function: "f(x, y) = 5", inputs = [6.0, 4.0];
        }
    }

    generate_implicit_tests! {
        list_tests: {
            test_list_equality: "[x, y] = [1, 2]", inputs = [1.0, 2.0];

            test_number_vs_list_rhs: "x = [1, 1]", inputs = [1.0];

            test_list_vs_number_lhs: "[x, x] = 2", inputs = [2.0];

            test_function_definition_list: "f(n) = [n, n+1]", inputs = [];
            test_function_definition_list_g: "g(n) = [n, n+1]", inputs = [];

            test_function_list_usage: "f(3) = [3, 4]", inputs = [1.0];
            test_function_list_equality: "f(x+1) = g(x+1)", inputs = [1.0];

            test_number_vs_list_mismatch: "x = [1, 2]", inputs = [1.0];

            test_list_element_mismatch: "[x, y] = [1, 3]", inputs = [1.0, 3.0];
        }
    }
}
