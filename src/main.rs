use rustyline::{
    error::ReadlineError,
    completion::Completer,
    hint::Hinter,
    highlight::Highlighter,
    validate::Validator,
    Helper, Editor, Context,
};

use tabwriter::TabWriter;
use joinery::*;

use std::{
    collections::{HashSet, HashMap},
    rc::Rc,
    io::Write,
    borrow::Cow,
};

mod whitespace;
use whitespace::*;

struct AutoCompleter {
    hints: HashSet<String>,
}

impl AutoCompleter {
    fn new() -> Self {
        let mut hints = HashSet::new();
        hints.insert("functions".into());
        hints.insert("variables".into());
        hints.insert("clear functions".into());
        hints.insert("clear variables".into());
        hints.insert("remove".into());
        hints.insert("abs".into());
        hints.insert("ceil".into());
        hints.insert("floor".into());
        hints.insert("exp".into());
        hints.insert("ln".into());
        hints.insert("log10".into());
        hints.insert("sqrt".into());
        hints.insert("d2rad".into());
        hints.insert("r2deg".into());
        hints.insert("round".into());
        hints.insert("log".into());
        hints.insert("cos".into());
        hints.insert("cosh".into());
        hints.insert("acos".into());
        hints.insert("acosh".into());
        hints.insert("sin".into());
        hints.insert("asin".into());
        hints.insert("sinh".into());
        hints.insert("asinh".into());
        hints.insert("tan".into());
        hints.insert("tanh".into());
        hints.insert("atan".into());
        hints.insert("atanh".into());
        hints.insert("atan2".into());
        hints.insert("pi".into());
        hints.insert("e".into());
        hints.insert("sum".into());
        hints.insert("prod".into());
        
        Self {
            hints
        }
    }
}

impl Completer for AutoCompleter {
    type Candidate = String;
}
impl Hinter for AutoCompleter {
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        if pos < line.len() || line.is_empty() || line.ends_with(" ") {
            return None;
        }

        // Find the last entered token after a whitespace
        let (pos, line) = line.rmatch_indices(" ").next()
            .map(|(idx, _)| ( pos - (idx+1), &line[idx+1..]))
            .unwrap_or((pos, line));

        self.hints.iter()
            .filter_map(|hint| {
                if hint.starts_with(&line[..pos]) {
                    Some(hint[pos..].into())
                } else {
                    None
                }
            })
            .next()
    }
}
impl Highlighter for AutoCompleter {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[90m".to_owned() + hint + "\x1b[0m")
    }
}
impl Validator for AutoCompleter {}
impl Helper for AutoCompleter {}


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

    Sum,
    Product,

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
                "round"     => OperationType::Builtin(Operator::Rnd),

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

                "sum"       => OperationType::Builtin(Operator::Sum),
                "prod"      => OperationType::Builtin(Operator::Product),
                _           => OperationType::CustomFunction(part.clone()),
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

            OperationType::Builtin(Operator::Sum)   => {
                if stack.is_empty() {
                    Err(ErrorKind::InsufficientStack)
                } else {
                    let mut sum = 0.0;
                    while let Some(r) = stack.pop() {
                        sum += r;
                    }
                    stack.push(sum);
                    Ok(())
                }
            },
            OperationType::Builtin(Operator::Product)   => {
                if stack.is_empty() {
                    Err(ErrorKind::InsufficientStack)
                } else {
                    let mut sum = 1.0;
                    while let Some(r) = stack.pop() {
                        sum *= r;
                    }
                    stack.push(sum);
                    Ok(())
                }
            },

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

fn process_input(input: String, variables: &mut HashMap<&str, f64>, functions: &mut HashMap<Span, CustomFunction>, mut insert_hint: impl FnMut(String)) {
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

            let variable_names: Vec<Span> = var.iter().cloned().take(var.len()-1).collect();
            let body = parse_operations(expr);

            // Quick test of the function to make sure it works.
            // We don't need the result, just to evaluate to see if it fails.
            let mut fun_vars = variables.clone();
            for variable_name in variable_names.iter().rev() {
                fun_vars.insert(variable_name.value(), 1.0);
            }

            match evaluate_operations(&body, &fun_vars, &functions, total_span.clone()) {
                Err((span, e)) => { print_error(span, e); },
                Ok(res) if variable_names.is_empty() => {
                    println!("Variable defined: {}", input);
                    println!();

                    let body_start = expr[0].start();
                    let mut body_span = total_span.clone();
                    body_span.set_start(body_start);

                    insert_hint(name.value().into());
                    functions.insert(name.clone(), CustomFunction {
                        span: total_span.clone(),
                        body: vec![Operation {
                            op_type: OperationType::Number(res),
                            span: body_span }],
                        variable_names});
                },
                Ok(_) => {
                    println!("Function defined: {}", input);
                    println!();
                    insert_hint(name.value().into());
                    functions.insert(name.clone(), CustomFunction { span: total_span.clone(), body, variable_names });
                }
            };
        },
        None => {
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

fn print_help() {
    println!("-- Commands --");

    let stdout = std::io::stdout();
    let mut tw = TabWriter::new(stdout).padding(1);
    let _ = writeln!(&mut tw, "{}\t{}", "functions",        "List all custom defined functions");
    let _ = writeln!(&mut tw, "{}\t{}", "variables",        "List all defined variables");
    let _ = writeln!(&mut tw, "{}\t{}", "clear functions",  "Remove all custom defined functions");
    let _ = writeln!(&mut tw, "{}\t{}", "clear variables",  "Remove all defined variables");
    let _ = writeln!(&mut tw, "{}\t{}", "remove <name>",    "Remove a variable or custom function by name");

    let _ = tw.flush();

    println!();
}

fn print_functions(functions: &HashMap<Span, CustomFunction>) {
    if !functions.iter().any(|(_, f)| !f.variable_names.is_empty()) {
        println!("No custom functions defined");
    } else {
        println!("-- Custom Functions --");
        let stdout = std::io::stdout();
        let mut tw = TabWriter::new(stdout).padding(1);
        let _ = writeln!(&mut tw, "Name\t|\tArgs\t|\tBody");

        for (name, fun) in functions.iter().filter(|(_, f)| !f.variable_names.is_empty()) {
            let _ = write!(&mut tw, "{}", name.value());
            let _ = write!(&mut tw, "\t|\t{}", fun.variable_names.iter().map(|s| s.value()).join_with(", "));

            let body = fun.body[0].span.start();

            let _ = writeln!(&mut tw, "\t|\t{}", &fun.body[0].span.input()[body..]);
        }

        let _ = tw.flush();
    }

    println!()
}

fn print_variables(variables: &HashMap<&str, f64>, functions: &HashMap<Span, CustomFunction>) {
    if variables.is_empty() && !functions.iter().any(|(_, f)| f.variable_names.is_empty()) {
        println!("No variables defined");
    } else {
        println!("-- Variables --");
        let stdout = std::io::stdout();
        let mut tw = TabWriter::new(stdout).padding(1);
        let _ = writeln!(&mut tw, "Name\t|\tValue");

        for (name, value) in variables {
            let _ = writeln!(&mut tw, "{}\t|\t{}", name, value);
        }

        for (name, fun) in functions.iter().filter(|(_, f)| f.variable_names.is_empty()) {
            let _ = write!(&mut tw, "{}", name.value());

            let body = fun.body[0].span.start();
            let _ = writeln!(&mut tw, "\t|\t{}", &fun.body[0].span.input()[body..]);
        }

        let _ = tw.flush();
    }

    println!()
}

fn clear_hinter<'a> (hinter: &mut AutoCompleter, names: impl Iterator<Item = &'a str>) {
    for name in names {
        hinter.hints.remove(name);
    }
}

fn main() {
    let inputs: Vec<_> = std::env::args().skip(1).collect();

    let mut variables = HashMap::new();
    let mut functions = HashMap::new();

    if inputs.is_empty() {
        println!("Postfix Calculator");
        println!("type \"help\" for more information.");

        let completer = AutoCompleter::new();
        let mut rl = Editor::new();
        rl.set_helper(Some(completer));
        loop {
            let line = rl.readline(">>> ");
            match line {
                Ok(input) => {
                    rl.add_history_entry(&input);
                    match input.trim() {
                        "help" => print_help(),
                        "functions" => print_functions(&functions),
                        "variables" => print_variables(&variables, &functions),
                        "clear variables" => {
                            if let Some(helper) = rl.helper_mut() {
                                clear_hinter(helper, functions.iter().filter(|(_, f)| f.variable_names.is_empty()).map(|(name, _)| name.value()));
                            }
                            functions.retain(|_, f| !f.variable_names.is_empty());
                            variables.clear();
                            println!("Variables cleared");
                            println!();
                        },
                        "clear functions" => {
                            if let Some(helper) = rl.helper_mut() {
                                clear_hinter(helper, functions.iter().filter(|(_, f)| !f.variable_names.is_empty()).map(|(name, _)| name.value()));
                            }
                            functions.retain(|_, f| f.variable_names.is_empty());
                            println!("Custom functions cleared");
                            println!();
                        },
                        _ if input.starts_with("remove ") => {
                            let name = input.trim_start_matches("remove ");
                            if name == "ans" {
                                variables.clear();
                                println!("Removed variable \"{}\"", name);
                                println!();
                            } else if let Some(f) = functions.remove(name) {
                                if let Some(helper) = rl.helper_mut() {
                                    clear_hinter(helper, std::iter::once(name));
                                }
                                if f.variable_names.is_empty() {
                                    println!("Removed variable \"{}\"", name);
                                } else {
                                    println!("Removed function \"{}\"", name);
                                }
                                println!();
                            } else {
                                println!("Unknown variable or function \"{}\"", name);
                                println!();
                            }
                        },
                        _ => {
                            let helper = |hint| if let Some(helper) = rl.helper_mut() { helper.hints.insert(hint); };
                            process_input(input, &mut variables, &mut functions, helper);
                        }
                    }
                },
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
                Err(e) => {
                    eprintln!("{:?}", e);
                    std::process::exit(1);
                }
            }
        }
    } else {
        for input in inputs {
            println!("Evaluating Expression: {}", input);
            process_input(input, &mut variables, &mut functions, |_| {});
        }
    }
}
