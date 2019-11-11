use std::{
    collections::HashMap,
    rc::Rc,
};

mod whitespace;
use whitespace::*;

#[derive(Debug)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

#[derive(Debug)]
enum OperationType {
    Number(f64),
    BasicOp(Operator),
    Function(Span),
}

#[derive(Debug)]
struct Operation {
    span: Span,
    op_type: OperationType,
}

enum ErrorKind {
    InsufficientStack,
    UnknownFunction,
    NonEmptyStack,
    InvalidVariableOrFunction,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ErrorKind::*;
        match self {
            InsufficientStack => write!(f, "Attempted to evaluate an operator with insufficient values on stack.")?,
            UnknownFunction => write!(f, "Unknown function or variable.",)?,
            NonEmptyStack => write!(f, "Mismatched numbers and operations.")?,
            InvalidVariableOrFunction => write!(f, "Invalid variable or function definition.")?,
        }

        Ok(())
    }
}

#[derive(Debug)]
struct CustomFunction {
    span: Span,
    body: Vec<Operation>,
    variable_names: Vec<Span>,
}

fn parse_operations(input: &[Span]) -> Vec<Operation> {
    let mut ops = Vec::new();

    for part in input {
        let operation = if let Ok(num) = part.value().parse() {
            OperationType::Number(num)
        } else {
            match part.value() {
                "+" => OperationType::BasicOp(Operator::Add),
                "-" => OperationType::BasicOp(Operator::Sub),
                "*" => OperationType::BasicOp(Operator::Mul),
                "/" => OperationType::BasicOp(Operator::Div),
                "^" => OperationType::BasicOp(Operator::Pow),
                "%" => OperationType::BasicOp(Operator::Mod),
                _ => OperationType::Function(part.clone()),
            }
        };

        ops.push(Operation {
            span: part.clone(),
            op_type: operation,
        });
    }

    ops
}

fn apply_mono_func(stack: &mut Vec<f64>, f: impl Fn(f64) -> f64) -> Result<(), ErrorKind> {
    let a = stack.pop().ok_or(ErrorKind::InsufficientStack)?;
    stack.push(f(a));

    Ok(())
}

fn apply_bi_func(stack: &mut Vec<f64>, f: impl Fn(f64, f64) -> f64) -> Result<(), ErrorKind> {
    let (b, a) = stack.pop()
        .and_then(|a| stack.pop().map(|b| (a, b)))
        .ok_or(ErrorKind::InsufficientStack)?;

    stack.push(f(a, b));

    Ok(())
}

fn evaluate_operations(ops: &[Operation], variables: &HashMap<&str, f64>, functions: &HashMap<Span, CustomFunction>, total_span: Span) -> Result<f64, (Span, ErrorKind)> {
    let mut stack = Vec::new();

    for op in ops {
        let ret = match &op.op_type {
            OperationType::Number(num) => {
                stack.push(*num);
                Ok(())
            },
            OperationType::BasicOp(Operator::Add) => apply_bi_func(&mut stack, |a, b| a + b),
            OperationType::BasicOp(Operator::Sub) => apply_bi_func(&mut stack, |a, b| a - b),
            OperationType::BasicOp(Operator::Mul) => apply_bi_func(&mut stack, |a, b| a * b),
            OperationType::BasicOp(Operator::Div) => apply_bi_func(&mut stack, |a, b| a / b),
            OperationType::BasicOp(Operator::Pow) => apply_bi_func(&mut stack, &f64::powf),
            OperationType::BasicOp(Operator::Mod) => apply_bi_func(&mut stack, |a, b| a % b),
            OperationType::Function(name) => {
                match name.value() {
                    "abs"      => apply_mono_func(&mut stack, &f64::abs),
                    "ceil"     => apply_mono_func(&mut stack, &f64::ceil),
                    "floor"    => apply_mono_func(&mut stack, &f64::floor),
                    "exp"      => apply_mono_func(&mut stack, &f64::exp),
                    "ln"       => apply_mono_func(&mut stack, &f64::ln),
                    "log10"    => apply_mono_func(&mut stack, &f64::log10),
                    "sqrt"     => apply_mono_func(&mut stack, &f64::sqrt),
                    "d2rad"    => apply_mono_func(&mut stack, &f64::to_radians),
                    "r2deg"    => apply_mono_func(&mut stack, &f64::to_degrees),
                    "rnd"      => apply_mono_func(&mut stack, &f64::round),

                    "log"      => apply_bi_func(&mut stack, &f64::log),
                    "pow"      => apply_bi_func(&mut stack, &f64::powf),

                    "cos"      => apply_mono_func(&mut stack, &f64::cos),
                    "cosh"     => apply_mono_func(&mut stack, &f64::cosh),
                    "acos"     => apply_mono_func(&mut stack, &f64::acos),
                    "acosh"    => apply_mono_func(&mut stack, &f64::acosh),

                    "sin"      => apply_mono_func(&mut stack, &f64::sin),
                    "sinh"     => apply_mono_func(&mut stack, &f64::sinh),
                    "asin"     => apply_mono_func(&mut stack, &f64::asin),
                    "asinh"    => apply_mono_func(&mut stack, &f64::asinh),

                    "tan"      => apply_mono_func(&mut stack, &f64::tan),
                    "tanh"     => apply_mono_func(&mut stack, &f64::tanh),
                    "atan"     => apply_mono_func(&mut stack, &f64::atan),
                    "atanh"    => apply_mono_func(&mut stack, &f64::atanh),
                    "atan2"    => apply_bi_func(&mut stack, &f64::atan2),
                    "pi"       => {
                        stack.push(std::f64::consts::PI);
                        Ok(())
                    },
                    "e"        => {
                        stack.push(std::f64::consts::E);
                        Ok(())
                    },

                    var => {
                        match (variables.get(var), functions.get(var)) {
                            (Some(var), _) => {
                                stack.push(*var);
                                Ok(())
                            },
                            (None, Some(fun)) => {
                                let mut fun_vars = variables.clone();

                                for variable_name in fun.variable_names.iter().rev() {
                                    let val = stack.pop().ok_or((op.span.clone(), ErrorKind::InsufficientStack))?;
                                    fun_vars.insert(variable_name.value(), val);
                                }

                                let result = evaluate_operations(&fun.body, &fun_vars, &functions, fun.span.clone())?;
                                stack.push(result);
                                Ok(())
                            },
                            (None, None) => Err(ErrorKind::UnknownFunction),
                        }
                    }
                }
            }
        };

        ret.map_err(|e|(op.span.clone(), e))?;
    }

    if stack.len() != 1 {
        Err((total_span, ErrorKind::NonEmptyStack))
    } else {
        Ok(stack.pop().unwrap())
    }
}

fn print_error(span: Span, err: ErrorKind) {
    eprintln!("Error: {}", err);
    eprintln!("   {}", span.input());
    eprintln!("   {1:0$}{3:^^2$}", span.start(), "", span.len(), "");
}

fn starts_with_digit(input: &str) -> bool {
    input.chars().next().filter(char::is_ascii_digit).is_some()
}

fn process_input(input: String, variables: &mut HashMap<&str, f64>, functions: &mut HashMap<Span, CustomFunction>) {
    let input: Rc<str> = input.into_boxed_str().into();

    let total_span = Span::new(Rc::clone(&input));
    let parts = get_parts(Rc::clone(&input));
    let split_idx = parts.iter().position(|span| span.value() == "=");

    match split_idx {
        Some(idx) => {
            let (var, expr) = parts.split_at(idx);

            if var.is_empty() || var.iter().any(|v| starts_with_digit(v.value())) {
                print_error(total_span, ErrorKind::InvalidVariableOrFunction);
                return;
            }

            let expr = &expr[1..];
            let name = var.last().unwrap();

            println!("Defining function: {}", input);
            let variable_names: Vec<Span> = var.iter().map(|v| v.clone()).take(var.len()-1).collect();
            let body = parse_operations(expr);

            // Quick test of the function to make sure it works.
            // We don't need the result, just to evaluate to see if it fails.
            let mut fun_vars = variables.clone();
            for variable_name in variable_names.iter().rev() {
                fun_vars.insert(variable_name.value(), 1.0);
            }

            if let Err((span, e)) = evaluate_operations(&body, &fun_vars, &functions, total_span.clone()) {
                print_error(span, e);
            } else {
                functions.insert(name.clone(), CustomFunction { span: total_span.clone(), body, variable_names });
            }
        },
        None => {
            println!("Evaluating Expression: {}", input);
            let ops = parse_operations(&parts);
            let result = evaluate_operations(&ops, &variables, &functions, total_span);

            match result {
                Ok(result) => {
                    variables.insert("ans", result);

                    println!("Result: {}", result);
                    println!();
                },
                Err((span, e)) => print_error(span, e),
            }
        },
    };
}

fn main() {
    let inputs: Vec<_> = std::env::args().skip(1).collect();
    if inputs.is_empty() {
        eprintln!("Must provide a post-fix expression.");
        std::process::exit(1);
    }

    let mut variables = HashMap::new();
    let mut functions = HashMap::new();

    for input in inputs {
        process_input(input, &mut variables, &mut functions);
    }
}
