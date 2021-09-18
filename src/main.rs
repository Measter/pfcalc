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
    collections::{BTreeSet, HashMap},
    rc::Rc,
    io::Write,
    borrow::Cow,
};

mod whitespace;
use whitespace::*;

struct AutoCompleter {
    builtins: BTreeSet<String>,
    hints: BTreeSet<String>,
}

impl AutoCompleter {
    fn new() -> Self {
        let keywords = [
            "functions", "variables", "clear", "remove",
            "add", "sub", "mul", "div", "pow", "rem", "sqrt",
            
            "abs", "ceil", "floor","round",
            
            "exp", "ln", "log10", "log",
            
            "d2rad", "r2deg",
            "cos", "cosh", "acos", "acosh",
            "sin", "asin", "sinh", "asinh",
            "tan", "tanh", "atan", "atanh", "atan2",
            
            "pi", "e",
            "sum", "prod"
        ];

        Self {
            builtins: keywords.iter().map(|&k| k.to_owned()).collect(),
            hints: BTreeSet::new(),
        }
    }
}

impl Completer for AutoCompleter {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize, _: &Context<'_>) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        if pos < line.len() || line.is_empty() || line.ends_with(' ') {
            return Ok((pos, Vec::new()));
        }

        // Find the last entered token after a whitespace
        let (start_pos, line) = line.rmatch_indices(' ').next()
            .map(|(idx, _)| (idx+1, &line[idx+1..]))
            .unwrap_or((0, line));
        
        let mut matches: Vec<_> = self.builtins.iter()
            .filter(|f| f.starts_with(line))
            .chain(self.hints.iter()
                .filter(|f| f.starts_with(line))
            )
            .cloned()
            .collect();
        matches.sort();

        Ok((start_pos, matches))
    }
}
impl Hinter for AutoCompleter {
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        if pos < line.len() || line.is_empty() || line.ends_with(' ') {
            return None;
        }

        // Find the last entered token after a whitespace
        let (pos, line) = line.rmatch_indices(' ').next()
            .map(|(idx, _)| ( pos - (idx+1), &line[idx+1..]))
            .unwrap_or((pos, line));

        let finder = |map: &BTreeSet<String>| {
            map.iter()
                .filter_map(|hint| {
                    if hint.starts_with(&line[..pos]) {
                        Some(hint[pos..].into())
                    } else {
                        None
                    }
                })
                .next()
        };

        finder(&self.builtins).or_else(|| finder(&self.hints))
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

fn parse_operations(input: impl Iterator<Item=Span>) -> Vec<Operation> {
    let mut ops = Vec::new();

    for part in input {
        let operation = if let Ok(num) = part.parse() {
            OperationType::Number(num)
        } else {
            match &*part {
                "+" | "add" => OperationType::Builtin(Operator::Add),
                "-" | "sub" => OperationType::Builtin(Operator::Sub),
                "*" | "mul" => OperationType::Builtin(Operator::Mul),
                "/" | "div" => OperationType::Builtin(Operator::Div),
                "^" | "pow" => OperationType::Builtin(Operator::Pow),
                "%" | "rem" => OperationType::Builtin(Operator::Mod),

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
    functions: &HashMap<String, CustomFunction>,
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

            OperationType::Builtin(Operator::Sum) => {
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
            OperationType::Builtin(Operator::Product) => {
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

            OperationType::Builtin(Operator::Pi) => {
                stack.push(std::f64::consts::PI);
                Ok(())
            },
            OperationType::Builtin(Operator::E) => {
                stack.push(std::f64::consts::E);
                Ok(())
            },
            OperationType::CustomFunction(name) => {
                let var = &**name;
                match (variables.get(var), functions.get(var)) {
                    (Some(var), _) => {
                        stack.push(*var);
                        Ok(())
                    },
                    (None, Some(fun)) => {
                        let mut fun_vars = variables.clone();

                        for variable_name in fun.variable_names.iter().rev() {
                            let val = stack.pop().ok_or((op.span.clone(), ErrorKind::InsufficientStack))?;
                            fun_vars.insert(&*variable_name, val);
                        }

                        let result = evaluate_operations(&fun.body, &fun_vars, &functions, fun.span.clone())?;
                        stack.push(result);
                        Ok(())
                    },
                    (None, None) => Err(ErrorKind::UnknownFunction),
                }
            }
        };

        ret.map_err(|e| (op.span.clone(), e))?;
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

fn process_input(input: String, variables: &mut HashMap<&str, f64>, functions: &mut HashMap<String, CustomFunction>, mut insert_hint: impl FnMut(String)) {
    let input: Rc<str> = input.into_boxed_str().into();

    let total_span = Span::new(Rc::clone(&input));
    let mut parts = get_spans(&input);

    // Space to ensure it's a properly delimited chunk.
    if input.contains(" = ") {
        // A function definition.
        let mut variable_names = Vec::new();
        variable_names.extend((&mut parts).take_while(|p| &**p != "=") );

        if variable_names.is_empty() || variable_names.iter().any(|v| starts_with_digit(v)) {
            print_error(total_span, ErrorKind::InvalidVariableOrFunction);
            return;
        }

        let mut body = parts.peekable();
        let body_start = if let Some(span) = body.peek() {
            span.start()
        } else {
            // Empty body.
            print_error(total_span, ErrorKind::InvalidVariableOrFunction);
            return;
        };

        let name = variable_names.pop().unwrap();
        let body = parse_operations(body);

        // Quick test of the function to make sure it works.
        // We don't need the result, just to evaluate to see if it fails.
        let mut fun_vars = variables.clone();
        for variable_name in variable_names.iter().rev() {
            fun_vars.insert(variable_name, 1.0);
        }

        match evaluate_operations(&body, &fun_vars, &functions, total_span.clone()) {
            Err((span, e)) => { print_error(span, e); },
            Ok(res) if variable_names.is_empty() => {
                println!("Variable defined: {}", input);
                println!();

                let mut body_span = total_span.clone();
                body_span.set_start(body_start);

                insert_hint((*name).to_owned());
                functions.insert((*name).to_owned(), CustomFunction {
                    span: total_span,
                    body: vec![Operation {
                        op_type: OperationType::Number(res),
                        span: body_span }],
                    variable_names});
            },
            Ok(_) => {
                println!("Function defined: {}", input);
                println!();
                insert_hint((*name).to_owned());
                functions.insert((*name).to_owned(), CustomFunction { span: total_span, body, variable_names });
            }
        };
    } else {
        // Just a regular expression.
        let ops = parse_operations(parts);
        let result = evaluate_operations(&ops, &variables, &functions, total_span);

        match result {
            Ok(result) => {
                variables.insert("ans", result);

                println!("Result: {}", result);
                println!();
            },
            Err((span, e)) => print_error(span, e),
        }
    }
}

fn print_help() {
    println!("-- Commands --");

    let stdout = std::io::stdout();
    let mut tw = TabWriter::new(stdout).padding(1);
    let _ = tw.write_all(b"functions\tList all custom defined functions");
    let _ = tw.write_all(b"variablesriablt all defined variables");
    let _ = tw.write_all(b"clear functions\tRemove all custom defined functions");
    let _ = tw.write_all(b"clear variables\tRemove all defined variables");
    let _ = tw.write_all(b"remove <name>\tRemove a variable or custom function by name");
    let _ = tw.flush();

    println!();

    println!("-- Built in Functions --");

    let _ = tw.write_all(b"+(add)\t-(sub)\t*(mul)\t/(div)\t^(pow)\t%(rem)");
    let _ = tw.write_all(b"abs\tceil\tfloor\texp\tln\tlog10");
    let _ = tw.write_all(b"log\tsqrt\td2rad\tr2deg\tround\tcos");
    let _ = tw.write_all(b"cosh\tacos\tacosh\tsin\tsinh\tasin");
    let _ = tw.write_all(b"asinh\ttan\ttanh\tatan\tatanh\tatan2");
    let _ = tw.write_all(b"sum\tprod\tpi\te\t\t");
    let _ = tw.flush();

    println!();
}

fn print_functions(functions: &HashMap<String, CustomFunction>) {
    if !functions.iter().any(|(_, f)| !f.variable_names.is_empty()) {
        println!("No custom functions defined");
    } else {
        println!("-- Custom Functions --");
        let stdout = std::io::stdout();
        let mut tw = TabWriter::new(stdout).padding(1);
        let _ = writeln!(&mut tw, "Name\t|\tArgs\t|\tBody");

        for (name, fun) in functions.iter().filter(|(_, f)| !f.variable_names.is_empty()) {
            let _ = write!(&mut tw, "{}", &**name);
            let _ = write!(&mut tw, "\t|\t{}", fun.variable_names.iter().map(|s| &**s).join_with(", "));

            let body = fun.body[0].span.start();

            let _ = writeln!(&mut tw, "\t|\t{}", &fun.body[0].span.input()[body..]);
        }

        let _ = tw.flush();
    }

    println!()
}

fn print_variables(variables: &HashMap<&str, f64>, functions: &HashMap<String, CustomFunction>) {
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
            let _ = write!(&mut tw, "{}", &**name);

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
                                clear_hinter(helper, functions.iter().filter(|(_, f)| f.variable_names.is_empty()).map(|(name, _)| &**name));
                            }
                            functions.retain(|_, f| !f.variable_names.is_empty());
                            variables.clear();
                            println!("Variables cleared");
                            println!();
                        },
                        "clear functions" => {
                            if let Some(helper) = rl.helper_mut() {
                                clear_hinter(helper, functions.iter().filter(|(_, f)| !f.variable_names.is_empty()).map(|(name, _)| &**name));
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
