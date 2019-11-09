use std::collections::HashMap;

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
enum OperationType<'a> {
    Number(f64),
    BasicOp(Operator),
    Function(&'a str),
}

#[derive(Debug)]
struct Operation<'a> {
    span: Span<'a>,
    op_type: OperationType<'a>,
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

#[derive(Debug, Copy, Clone)]
struct Span<'a> {
    value: &'a str,
    start: usize,
    len: usize
}

#[derive(Debug)]
struct CustomFunction<'a> {
    span: Span<'a>,
    body: Vec<Operation<'a>>,
    variable_names: Vec<&'a str>,
}

fn get_parts(input: &str) -> Vec<Span> {
    let chars = &mut input.char_indices().peekable();
    let mut parts = Vec::new();

    while let Some((idx, c)) = chars.next() {
        if c.is_whitespace() {
            continue;
        }

        let end_idx;

        loop {
            match chars.peek() {
                Some((_, c)) if !c.is_whitespace() => {
                    chars.next();
                    continue
                },
                Some((idx, _)) => end_idx = *idx,
                // At end of string, just grab the rest.
                None => end_idx = input.len(),
            }

            break;
        }

        let part = &input[idx..end_idx];
        parts.push(Span{value: part, start: idx, len: end_idx-idx});
    }

    parts
}

fn parse_operations<'a>(input: &[Span<'a>]) -> Vec<Operation<'a>> {
    let mut ops = Vec::new();

    for part in input {
        let operation = if let Ok(num) = part.value.parse() {
            OperationType::Number(num)
        } else {
            match part.value {
                "+" => OperationType::BasicOp(Operator::Add),
                "-" => OperationType::BasicOp(Operator::Sub),
                "*" => OperationType::BasicOp(Operator::Mul),
                "/" => OperationType::BasicOp(Operator::Div),
                "^" => OperationType::BasicOp(Operator::Pow),
                "%" => OperationType::BasicOp(Operator::Mod),
                _ => OperationType::Function(part.value),
            }
        };

        ops.push(Operation {
            span: *part,
            op_type: operation,
        });
    }

    ops
}

fn apply_mono_func<'a>(stack: &mut Vec<f64>, t_span: Span<'a>, span: Span<'a>, f: impl Fn(f64) -> f64) -> Result<(), (Span<'a>, Span<'a>, ErrorKind)> {
    let a = stack.pop().ok_or((t_span, span, ErrorKind::InsufficientStack))?;
    stack.push(f(a));

    Ok(())
}

fn apply_bi_func<'a>(stack: &mut Vec<f64>, t_span: Span<'a>, span: Span<'a>, f: impl Fn(f64, f64) -> f64) -> Result<(), (Span<'a>, Span<'a>, ErrorKind)> {
    let (b, a) = stack.pop()
        .and_then(|a| stack.pop().map(|b| (a, b)))
        .ok_or((t_span, span, ErrorKind::InsufficientStack))?;

    stack.push(f(a, b));

    Ok(())
}

fn evaluate_operations<'a>(ops: &'a [Operation], variables: &HashMap<&str, f64>, functions: &'a HashMap<&'a str, CustomFunction<'a>>, total_span: Span<'a>) -> Result<f64, (Span<'a>, Span<'a>, ErrorKind)> {
    let mut stack = Vec::new();

    for op in ops {
        match &op.op_type {
            OperationType::Number(num) => stack.push(*num),
            OperationType::BasicOp(Operator::Add) => apply_bi_func(&mut stack, total_span, op.span, |a, b| a + b)?,
            OperationType::BasicOp(Operator::Sub) => apply_bi_func(&mut stack, total_span, op.span, |a, b| a - b)?,
            OperationType::BasicOp(Operator::Mul) => apply_bi_func(&mut stack, total_span, op.span, |a, b| a * b)?,
            OperationType::BasicOp(Operator::Div) => apply_bi_func(&mut stack, total_span, op.span, |a, b| a / b)?,
            OperationType::BasicOp(Operator::Pow) => apply_bi_func(&mut stack, total_span, op.span, &f64::powf)?,
            OperationType::BasicOp(Operator::Mod) => apply_bi_func(&mut stack, total_span, op.span, |a, b| a % b)?,
            OperationType::Function(name) => {
                match name {
                    &"abs"      => apply_mono_func(&mut stack, total_span, op.span, &f64::abs)?,
                    &"ceil"     => apply_mono_func(&mut stack, total_span, op.span, &f64::ceil)?,
                    &"floor"    => apply_mono_func(&mut stack, total_span, op.span, &f64::floor)?,
                    &"exp"      => apply_mono_func(&mut stack, total_span, op.span, &f64::exp)?,
                    &"ln"       => apply_mono_func(&mut stack, total_span, op.span, &f64::ln)?,
                    &"log10"    => apply_mono_func(&mut stack, total_span, op.span, &f64::log10)?,
                    &"sqrt"     => apply_mono_func(&mut stack, total_span, op.span, &f64::sqrt)?,
                    &"d2rad"    => apply_mono_func(&mut stack, total_span, op.span, &f64::to_radians)?,
                    &"r2deg"    => apply_mono_func(&mut stack, total_span, op.span, &f64::to_degrees)?,
                    &"rnd"      => apply_mono_func(&mut stack, total_span, op.span, &f64::round)?,

                    &"log"      => apply_bi_func(&mut stack, total_span, op.span, &f64::log)?,
                    &"pow"      => apply_bi_func(&mut stack, total_span, op.span, &f64::powf)?,

                    &"cos"      => apply_mono_func(&mut stack, total_span, op.span, &f64::cos)?,
                    &"cosh"     => apply_mono_func(&mut stack, total_span, op.span, &f64::cosh)?,
                    &"acos"     => apply_mono_func(&mut stack, total_span, op.span, &f64::acos)?,
                    &"acosh"    => apply_mono_func(&mut stack, total_span, op.span, &f64::acosh)?,

                    &"sin"      => apply_mono_func(&mut stack, total_span, op.span, &f64::sin)?,
                    &"sinh"     => apply_mono_func(&mut stack, total_span, op.span, &f64::sinh)?,
                    &"asin"     => apply_mono_func(&mut stack, total_span, op.span, &f64::asin)?,
                    &"asinh"    => apply_mono_func(&mut stack, total_span, op.span, &f64::asinh)?,

                    &"tan"      => apply_mono_func(&mut stack, total_span, op.span, &f64::tan)?,
                    &"tanh"     => apply_mono_func(&mut stack, total_span, op.span, &f64::tanh)?,
                    &"atan"     => apply_mono_func(&mut stack, total_span, op.span, &f64::atan)?,
                    &"atanh"    => apply_mono_func(&mut stack, total_span, op.span, &f64::atanh)?,
                    &"atan2"    => apply_bi_func(&mut stack, total_span, op.span, &f64::atan2)?,
                    &"pi"       => stack.push(std::f64::consts::PI),
                    &"e"        => stack.push(std::f64::consts::E),

                    var => {
                        match (variables.get(var), functions.get(var)) {
                            (Some(var), _) => stack.push(*var),
                            (None, Some(fun)) => {
                                let mut fun_vars = variables.clone();

                                for variable_name in fun.variable_names.iter().rev() {
                                    let val = stack.pop().ok_or((total_span, op.span, ErrorKind::InsufficientStack))?;
                                    fun_vars.insert(variable_name, val);
                                }

                                let result = evaluate_operations(&fun.body, &fun_vars, &functions, fun.span)?;
                                stack.push(result);
                            },
                            (None, None) => return Err((total_span, op.span, ErrorKind::UnknownFunction)),
                        }
                    }
                }
            }
        }
    }

    if stack.len() != 1 {
        Err((total_span, total_span, ErrorKind::NonEmptyStack))
    } else {
        Ok(stack.pop().unwrap())
    }
}

fn print_error(orig: &str, span: Span, err: ErrorKind) {
    eprintln!("Error: {}", err);
    eprintln!("   {}", orig);
    eprintln!("   {1:0$}{3:^^2$}", span.start, "", span.len, "");
}

fn starts_with_digit(input: &str) -> bool {
    input.chars().next().filter(char::is_ascii_digit).is_some()
}

fn main() {
    let inputs: Vec<_> = std::env::args().skip(1).collect();
    if inputs.is_empty() {
        eprintln!("Must provide a post-fix expression.");
        std::process::exit(1);
    }

    let mut variables = HashMap::new();
    let mut functions = HashMap::new();

    for input in &inputs {
        let total_span = Span { value: input, start: 0, len: input.len() };
        let parts = get_parts(input);
        let split_idx = parts.iter().position(|span| span.value == "=");

        match split_idx {
            Some(idx) => {
                let (var, expr) = parts.split_at(idx);

                if var.is_empty() || var.iter().any(|v| starts_with_digit(v.value)) {
                    print_error(input, total_span, ErrorKind::InvalidVariableOrFunction);
                    continue;
                }

                let expr = &expr[1..];
                let name = var.last().unwrap();

                println!("Defining function: {}", input);
                let variable_names: Vec<&str> = var.iter().map(|v| v.value).take(var.len()-1).collect();
                let body = parse_operations(expr);

                // Quick test of the function to make sure it works.
                // We don't need the result, just to evaluate to see if it fails.
                let mut fun_vars = variables.clone();
                for &variable_name in variable_names.iter().rev() {
                    fun_vars.insert(variable_name, 1.0);
                }

                if let Err((t_span, span, e)) = evaluate_operations(&body, &fun_vars, &functions, total_span) {
                    print_error(t_span.value, span, e);
                } else {
                    functions.insert(name.value, CustomFunction { span: total_span, body, variable_names });
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
                    Err((t_span, span, e)) => print_error(t_span.value, span, e),
                }
            },
        };
    }
}
