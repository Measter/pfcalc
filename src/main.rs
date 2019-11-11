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

    Abs,
    Ceil,
    Floor,
    Exp,
    Ln,
    Log10,
    Sqrt,
    D2Rad,
    R2Deg,
    Rnd,

    Log,

    Cos,
    Cosh,
    ACos,
    ACosh,

    Sin,
    Sinh,
    ASin,
    ASinh,

    Tan,
    Tanh,
    Atan,
    Atanh,
    Atan2,

    Pi,
    E,
}

#[derive(Debug)]
enum OperationType {
    Number(f64),
    Builtin(Operator),
    CustomFunction(Span),
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
                "+"         => OperationType::Builtin(Operator::Add),
                "-"         => OperationType::Builtin(Operator::Sub),
                "*"         => OperationType::Builtin(Operator::Mul),
                "/"         => OperationType::Builtin(Operator::Div),
                "^"         => OperationType::Builtin(Operator::Pow),
                "%"         => OperationType::Builtin(Operator::Mod),

                "abs"       => OperationType::Builtin(Operator::Abs),
                "ceil"      => OperationType::Builtin(Operator::Ceil),
                "floor"     => OperationType::Builtin(Operator::Floor),
                "exp"       => OperationType::Builtin(Operator::Exp),
                "ln"        => OperationType::Builtin(Operator::Ln),
                "log10"     => OperationType::Builtin(Operator::Log10),
                "sqrt"      => OperationType::Builtin(Operator::Sqrt),
                "d2rad"     => OperationType::Builtin(Operator::D2Rad),
                "r2deg"     => OperationType::Builtin(Operator::R2Deg),
                "rnd"       => OperationType::Builtin(Operator::Rnd),

                "log"       => OperationType::Builtin(Operator::Log),

                "cos"       => OperationType::Builtin(Operator::Cos),
                "cosh"      => OperationType::Builtin(Operator::Cosh),
                "acos"      => OperationType::Builtin(Operator::ACos),
                "acosh"     => OperationType::Builtin(Operator::ACosh),

                "sin"       => OperationType::Builtin(Operator::Sin),
                "sinh"      => OperationType::Builtin(Operator::Sinh),
                "asin"      => OperationType::Builtin(Operator::ASin),
                "asinh"     => OperationType::Builtin(Operator::ASinh),

                "tan"       => OperationType::Builtin(Operator::Tan),
                "tanh"      => OperationType::Builtin(Operator::Tanh),
                "atan"      => OperationType::Builtin(Operator::Atan),
                "atanh"     => OperationType::Builtin(Operator::Atanh),
                "atan2"     => OperationType::Builtin(Operator::Atan2),
                "pi"        => OperationType::Builtin(Operator::Pi),
                "e"         => OperationType::Builtin(Operator::E),
                _ => OperationType::CustomFunction(part.clone()),
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

fn evaluate_operations(
    ops: &[Operation],
    variables: &HashMap<&str, f64>,
    functions: &HashMap<Span, CustomFunction>,
    total_span: Span
) -> Result<f64, (Span, ErrorKind)>
{
    let mut stack = Vec::new();

    for op in ops {
        let ret = match &op.op_type {
            OperationType::Number(num) => {
                stack.push(*num);
                Ok(())
            },
            OperationType::Builtin(Operator::Add)   => apply_bi_func(&mut stack, |a, b| a + b),
            OperationType::Builtin(Operator::Sub)   => apply_bi_func(&mut stack, |a, b| a - b),
            OperationType::Builtin(Operator::Mul)   => apply_bi_func(&mut stack, |a, b| a * b),
            OperationType::Builtin(Operator::Div)   => apply_bi_func(&mut stack, |a, b| a / b),
            OperationType::Builtin(Operator::Pow)   => apply_bi_func(&mut stack, &f64::powf),
            OperationType::Builtin(Operator::Mod)   => apply_bi_func(&mut stack, |a, b| a % b),

            OperationType::Builtin(Operator::Abs)   => apply_mono_func(&mut stack, &f64::abs),
            OperationType::Builtin(Operator::Ceil)  => apply_mono_func(&mut stack, &f64::ceil),
            OperationType::Builtin(Operator::Floor) => apply_mono_func(&mut stack, &f64::floor),
            OperationType::Builtin(Operator::Exp)   => apply_mono_func(&mut stack, &f64::exp),
            OperationType::Builtin(Operator::Ln)    => apply_mono_func(&mut stack, &f64::ln),
            OperationType::Builtin(Operator::Log10) => apply_mono_func(&mut stack, &f64::log10),
            OperationType::Builtin(Operator::Sqrt)  => apply_mono_func(&mut stack, &f64::sqrt),
            OperationType::Builtin(Operator::D2Rad) => apply_mono_func(&mut stack, &f64::to_radians),
            OperationType::Builtin(Operator::R2Deg) => apply_mono_func(&mut stack, &f64::to_degrees),
            OperationType::Builtin(Operator::Rnd)   => apply_mono_func(&mut stack, &f64::round),

            OperationType::Builtin(Operator::Log)   => apply_bi_func(&mut stack, &f64::log),

            OperationType::Builtin(Operator::Cos)   => apply_mono_func(&mut stack, &f64::cos),
            OperationType::Builtin(Operator::Cosh)  => apply_mono_func(&mut stack, &f64::cosh),
            OperationType::Builtin(Operator::ACos)  => apply_mono_func(&mut stack, &f64::acos),
            OperationType::Builtin(Operator::ACosh) => apply_mono_func(&mut stack, &f64::acosh),

            OperationType::Builtin(Operator::Sin)   => apply_mono_func(&mut stack, &f64::sin),
            OperationType::Builtin(Operator::Sinh)  => apply_mono_func(&mut stack, &f64::sinh),
            OperationType::Builtin(Operator::ASin)  => apply_mono_func(&mut stack, &f64::asin),
            OperationType::Builtin(Operator::ASinh) => apply_mono_func(&mut stack, &f64::asinh),

            OperationType::Builtin(Operator::Tan)   => apply_mono_func(&mut stack, &f64::tan),
            OperationType::Builtin(Operator::Tanh)  => apply_mono_func(&mut stack, &f64::tanh),
            OperationType::Builtin(Operator::Atan)  => apply_mono_func(&mut stack, &f64::atan),
            OperationType::Builtin(Operator::Atanh) => apply_mono_func(&mut stack, &f64::atanh),
            OperationType::Builtin(Operator::Atan2) => apply_bi_func(&mut stack, &f64::atan2),
            OperationType::Builtin(Operator::Pi)    => {
                stack.push(std::f64::consts::PI);
                Ok(())
            },
            OperationType::Builtin(Operator::E)     => {
                stack.push(std::f64::consts::E);
                Ok(())
            },
            OperationType::CustomFunction(name) => {
                let var = name.value();
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
            let variable_names: Vec<Span> = var.iter().cloned().take(var.len()-1).collect();
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
