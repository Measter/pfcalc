use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{BTreeSet, HashMap},
    io::Write,
    ops::{Add, Div, Mul, Range, Rem, Sub},
    rc::Rc,
};

use derive_more::Display;
use joinery::*;
use lasso::{Rodeo, Spur};
use rustyline::{
    completion::Completer, error::ReadlineError, highlight::Highlighter, hint::Hinter,
    validate::Validator, Context, Editor, Helper,
};
use tabwriter::TabWriter;

mod lexer;
use lexer::*;

struct AutoCompleter {
    builtins: BTreeSet<String>,
    hints: RefCell<BTreeSet<String>>,
}

impl AutoCompleter {
    fn new() -> Self {
        #[rustfmt::skip]
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
            hints: RefCell::new(BTreeSet::new()),
        }
    }
}

impl Completer for AutoCompleter {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Self::Candidate>)> {
        if pos < line.len() || line.is_empty() || line.ends_with(' ') {
            return Ok((pos, Vec::new()));
        }

        // Find the last entered token after a whitespace
        let (start_pos, line) = line
            .rmatch_indices(' ')
            .next()
            .map(|(idx, _)| (idx + 1, &line[idx + 1..]))
            .unwrap_or((0, line));

        let Self { builtins, hints } = self;
        let hints = hints.borrow();

        let mut matches: Vec<_> = builtins
            .iter()
            .filter(|f| f.starts_with(line))
            .chain(hints.iter().filter(|f| f.starts_with(line)))
            .cloned()
            .collect();
        matches.sort();

        Ok((start_pos, matches))
    }
}
impl Hinter for AutoCompleter {
    type Hint = String;
    fn hint(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> Option<String> {
        if pos < line.len() || line.is_empty() || line.ends_with(' ') {
            return None;
        }

        // Find the last entered token after a whitespace
        let (pos, line) = line
            .rmatch_indices(' ')
            .next()
            .map(|(idx, _)| (pos - (idx + 1), &line[idx + 1..]))
            .unwrap_or((pos, line));

        let finder = |map: &BTreeSet<String>| {
            map.iter().find_map(|hint| {
                if hint.starts_with(&line[..pos]) {
                    Some(hint[pos..].into())
                } else {
                    None
                }
            })
        };

        let Self { builtins, hints } = self;
        let hints = hints.borrow();

        finder(builtins).or_else(|| finder(&hints))
    }
}

impl Highlighter for AutoCompleter {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Owned("\x1b[90m".to_owned() + hint + "\x1b[0m")
    }
}

impl Validator for AutoCompleter {}
impl Helper for AutoCompleter {}

#[derive(Display)]
enum ErrorKind {
    #[display(fmt = "Attempted to evaluate an operator with insufficient values on stack.")]
    InsufficientStack,
    #[display(fmt = "Unknown function or variable.")]
    UnknownFunction,
    #[display(fmt = "Mismatched numbers and operations.")]
    NonEmptyStack,
    #[display(fmt = "Invalid variable or function definition.")]
    InvalidVariableOrFunction,
}

#[derive(Debug, Clone)]
struct CustomFunction {
    name: Token,
    body: Vec<Operation>,
    body_string: Cow<'static, str>,
    params: Vec<Token>,
}

impl CustomFunction {
    fn variable(name: Token, value: f64) -> Self {
        Self {
            name,
            body: vec![Operation::number(value)],
            body_string: "".into(),
            params: Vec::new(),
        }
    }
}

fn apply_mono_func(stack: &mut Vec<f64>, f: fn(f64) -> f64) -> Result<(), ErrorKind> {
    let a = stack.last_mut().ok_or(ErrorKind::InsufficientStack)?;
    *a = f(*a);

    Ok(())
}

fn apply_bi_func(stack: &mut Vec<f64>, f: fn(f64, f64) -> f64) -> Result<(), ErrorKind> {
    let (b, a) = stack
        .pop()
        .and_then(|b| stack.last_mut().map(|a| (b, a)))
        .ok_or(ErrorKind::InsufficientStack)?;

    *a = f(*a, b);

    Ok(())
}

fn evaluate_operations(
    ops: &[Operation],
    environment: &HashMap<Spur, Rc<CustomFunction>>,
    interner: &Rodeo,
    total_span: Range<usize>,
) -> Result<f64, (Range<usize>, ErrorKind)> {
    let mut stack = Vec::new();

    for op in ops {
        #[rustfmt::skip]
        let ret = match op.op_type {
            OperationType::Number(num) => {
                stack.push(num);
                Ok(())
            },
            OperationType::Native(Operator::Add)   => apply_bi_func(&mut stack, Add::add),
            OperationType::Native(Operator::Sub)   => apply_bi_func(&mut stack, Sub::sub),
            OperationType::Native(Operator::Mul)   => apply_bi_func(&mut stack, Mul::mul),
            OperationType::Native(Operator::Div)   => apply_bi_func(&mut stack, Div::div),
            OperationType::Native(Operator::Pow)   => apply_bi_func(&mut stack, f64::powf),
            OperationType::Native(Operator::Mod)   => apply_bi_func(&mut stack, Rem::rem),

            OperationType::Native(Operator::Abs)   => apply_mono_func(&mut stack, f64::abs),
            OperationType::Native(Operator::Ceil)  => apply_mono_func(&mut stack, f64::ceil),
            OperationType::Native(Operator::Floor) => apply_mono_func(&mut stack, f64::floor),
            OperationType::Native(Operator::Exp)   => apply_mono_func(&mut stack, f64::exp),
            OperationType::Native(Operator::Ln)    => apply_mono_func(&mut stack, f64::ln),
            OperationType::Native(Operator::Log10) => apply_mono_func(&mut stack, f64::log10),
            OperationType::Native(Operator::Sqrt)  => apply_mono_func(&mut stack, f64::sqrt),
            OperationType::Native(Operator::D2Rad) => apply_mono_func(&mut stack, f64::to_radians),
            OperationType::Native(Operator::R2Deg) => apply_mono_func(&mut stack, f64::to_degrees),
            OperationType::Native(Operator::Rnd)   => apply_mono_func(&mut stack, f64::round),

            OperationType::Native(Operator::Log)   => apply_bi_func(&mut stack, f64::log),

            OperationType::Native(Operator::Cos)   => apply_mono_func(&mut stack, f64::cos),
            OperationType::Native(Operator::Cosh)  => apply_mono_func(&mut stack, f64::cosh),
            OperationType::Native(Operator::ACos)  => apply_mono_func(&mut stack, f64::acos),
            OperationType::Native(Operator::ACosh) => apply_mono_func(&mut stack, f64::acosh),

            OperationType::Native(Operator::Sin)   => apply_mono_func(&mut stack, f64::sin),
            OperationType::Native(Operator::Sinh)  => apply_mono_func(&mut stack, f64::sinh),
            OperationType::Native(Operator::ASin)  => apply_mono_func(&mut stack, f64::asin),
            OperationType::Native(Operator::ASinh) => apply_mono_func(&mut stack, f64::asinh),

            OperationType::Native(Operator::Tan)   => apply_mono_func(&mut stack, f64::tan),
            OperationType::Native(Operator::Tanh)  => apply_mono_func(&mut stack, f64::tanh),
            OperationType::Native(Operator::Atan)  => apply_mono_func(&mut stack, f64::atan),
            OperationType::Native(Operator::Atanh) => apply_mono_func(&mut stack, f64::atanh),
            OperationType::Native(Operator::Atan2) => apply_bi_func(&mut stack, f64::atan2),

            OperationType::Native(Operator::Sum) => {
                if stack.is_empty() {
                    Err(ErrorKind::InsufficientStack)
                } else {
                    let sum = stack.drain(..).sum();
                    stack.push(sum);
                    Ok(())
                }
            }
            OperationType::Native(Operator::Product) => {
                if stack.is_empty() {
                    Err(ErrorKind::InsufficientStack)
                } else {
                    let product = stack.drain(..).product();
                    stack.push(product);
                    Ok(())
                }
            }

            OperationType::Native(Operator::Pi) => {
                stack.push(std::f64::consts::PI);
                Ok(())
            }
            OperationType::Native(Operator::E) => {
                stack.push(std::f64::consts::E);
                Ok(())
            }
            OperationType::Custom(name) => {
                match environment.get(&name.lexeme) {
                    Some(func) => {
                        let mut new_environment = environment.clone();
                        for param in func.params.iter().rev() {
                            let val = stack.pop().ok_or((param.range(), ErrorKind::InsufficientStack))?;
                            new_environment.insert(param.lexeme, Rc::new(CustomFunction::variable(*param, val)));
                        }

                        let result = evaluate_operations(&func.body, &new_environment, interner, func.name.range())?;
                        stack.push(result);
                        Ok(())
                    },
                    None => Err(ErrorKind::UnknownFunction),
                }
            }
        };

        ret.map_err(|e| (op.token.range(), e))?;
    }

    if stack.len() != 1 {
        Err((total_span, ErrorKind::NonEmptyStack))
    } else {
        Ok(stack.pop().unwrap())
    }
}

fn print_error(input: &str, range: Range<usize>, err: ErrorKind) {
    eprintln!("Error: {}", err);
    eprintln!("   {}", input);
    eprintln!(
        "   {1:0$}{3:^^2$}",
        range.start,
        "",
        range.end - range.start,
        ""
    );
}

fn starts_with_digit(input: &str) -> bool {
    input.chars().next().filter(char::is_ascii_digit).is_some()
}

fn process_input(
    answer_token: Token,
    input: &str,
    environment: &mut HashMap<Spur, Rc<CustomFunction>>,
    interner: &mut Rodeo,
    output_success: bool,
    mut insert_hint: impl FnMut(String),
) {
    let (operations, body) = lexer::lex_input(input, interner);
    let total_span = 0..input.len();

    // Having a body means we have a function definition rather than a simple expression.
    if let Some(body) = body {
        if body.is_empty() {
            print_error(input, 0..input.len(), ErrorKind::InvalidVariableOrFunction);
            return;
        };

        let mut parameter_names = operations.into_iter().map(|t| t.token).collect::<Vec<_>>();
        let all_valid_names = parameter_names.iter().all(|v| {
            let lexeme = interner.resolve(&v.lexeme);
            !starts_with_digit(lexeme)
        });
        if parameter_names.is_empty() || !all_valid_names {
            print_error(input, 0..input.len(), ErrorKind::InvalidVariableOrFunction);
            return;
        }

        let name = parameter_names.pop().unwrap();

        let mut test_environment = environment.clone();
        // Quick test of the function to make sure it works.
        // We don't need the result, just to evaluate to see if it fails.
        for parameter in parameter_names.iter().rev() {
            test_environment.insert(
                parameter.lexeme,
                Rc::new(CustomFunction::variable(*parameter, 1.0)),
            );
        }

        let body_span = body.first().unwrap().token.source_start..input.len();
        match evaluate_operations(&body, &test_environment, interner, total_span) {
            Err((span, e)) => {
                print_error(input, span, e);
            }
            Ok(res) if parameter_names.is_empty() => {
                if output_success {
                    println!("Variable defined: {}", input);
                    println!();
                }

                let name_lexeme = interner.resolve(&name.lexeme);
                insert_hint(name_lexeme.to_owned());
                environment.insert(
                    name.lexeme,
                    Rc::new(CustomFunction {
                        name,
                        body: vec![Operation::number(res)],
                        params: parameter_names,
                        body_string: input[body_span].to_owned().into(),
                    }),
                );
            }
            Ok(_) => {
                if output_success {
                    println!("Function defined: {}", input);
                    println!();
                }

                let name_lexeme = interner.resolve(&name.lexeme);
                insert_hint(name_lexeme.to_owned());
                environment.insert(
                    name.lexeme,
                    Rc::new(CustomFunction {
                        name,
                        body,
                        params: parameter_names,
                        body_string: input[body_span].to_owned().into(),
                    }),
                );
            }
        };
    } else {
        // Nothing fancy, just an expression to evaluate.
        let result = evaluate_operations(&operations, environment, interner, total_span);

        match result {
            Ok(result) => {
                environment.insert(
                    answer_token.lexeme,
                    Rc::new(CustomFunction {
                        name: answer_token,
                        body: vec![Operation::number(result)],
                        body_string: result.to_string().into(),
                        params: Vec::new(),
                    }),
                );

                if output_success {
                    println!("Result: {}", result);
                    println!();
                }
            }
            Err((span, e)) => print_error(input, span, e),
        }
    }
}

fn print_help() {
    println!("-- Commands --");

    let stdout = std::io::stdout();
    let mut tw = TabWriter::new(stdout).padding(1);
    let _ = tw.write_all(b"functions\tList all custom defined functions\n");
    let _ = tw.write_all(b"variables\tList all defined variables\n");
    let _ = tw.write_all(b"clear functions\tRemove all custom defined functions\n");
    let _ = tw.write_all(b"clear variables\tRemove all defined variables\n");
    let _ = tw.write_all(b"remove <name>\tRemove a variable or custom function by name\n");
    let _ = tw.flush();

    println!();

    println!("-- Built in Functions --\n");

    let _ = tw.write_all(b"+(add)\t-(sub)\t*(mul)\t/(div)\t^(pow)\t%(rem)\n");
    let _ = tw.write_all(b"abs\tceil\tfloor\texp\tln\tlog10\n");
    let _ = tw.write_all(b"log\tsqrt\td2rad\tr2deg\tround\tcos\n");
    let _ = tw.write_all(b"cosh\tacos\tacosh\tsin\tsinh\tasin\n");
    let _ = tw.write_all(b"asinh\ttan\ttanh\tatan\tatanh\tatan2\n");
    let _ = tw.write_all(b"sum\tprod\tpi\te\t\t\n");
    let _ = tw.flush();

    println!();
}

fn print_functions(environment: &HashMap<Spur, Rc<CustomFunction>>, interner: &Rodeo) {
    if !environment.iter().any(|(_, f)| !f.params.is_empty()) {
        println!("No custom functions defined");
    } else {
        println!("-- Custom Functions --");
        let stdout = std::io::stdout();
        let mut tw = TabWriter::new(stdout).padding(1);
        let _ = tw.write_all(b"Name\t|\tArgs\t|\tBody\n");

        for (name, fun) in environment.iter().filter(|(_, f)| !f.params.is_empty()) {
            let name = interner.resolve(name);
            let _ = tw.write_all(name.as_bytes());
            let _ = write!(
                &mut tw,
                "\t|\t{}",
                fun.params
                    .iter()
                    .map(|s| interner.resolve(&s.lexeme))
                    .join_with(", ")
            );

            let _ = writeln!(&mut tw, "\t|\t{}", fun.body_string);
        }

        let _ = tw.flush();
    }

    println!()
}

fn print_variables(environment: &HashMap<Spur, Rc<CustomFunction>>, interner: &Rodeo) {
    if !environment.iter().any(|(_, f)| f.params.is_empty()) {
        println!("No variables defined");
    } else {
        println!("-- Variables --");
        let stdout = std::io::stdout();
        let mut tw = TabWriter::new(stdout).padding(1);
        let _ = tw.write_all(b"Name\t|\tValue\n");

        for (name, fun) in environment.iter().filter(|(_, f)| f.params.is_empty()) {
            let name = interner.resolve(name);
            let _ = tw.write_all(name.as_bytes());
            let _ = tw.write_all(b"\t|\t");
            let _ = tw.write_all(fun.body_string.as_bytes());
            let _ = tw.write_all(b"\n");
        }

        let _ = tw.flush();
    }

    println!()
}

fn remove_all_custom(
    environment: &mut HashMap<Spur, Rc<CustomFunction>>,
    helper: &AutoCompleter,
    interner: &Rodeo,
    has_params: bool,
) {
    let mut hints = helper.hints.borrow_mut();
    environment
        .iter()
        .filter(|(_, f)| f.params.is_empty() == !has_params)
        .map(|(name, _)| interner.resolve(name))
        .for_each(|name| {
            hints.remove(name);
        });

    environment.retain(|_, f| f.params.is_empty() != !has_params);
}

fn repl(
    answer_token: Token,
    environment: &mut HashMap<Spur, Rc<CustomFunction>>,
    interner: &mut Rodeo,
    completer: &mut AutoCompleter,
) {
    println!("Postfix Calculator");
    println!("type \"help\" for more information.");

    let mut rl = Editor::new();
    rl.set_helper(Some(&*completer));
    loop {
        let line = rl.readline(">>> ");
        match line {
            Ok(input) => {
                rl.add_history_entry(&input);
                match input.trim() {
                    "help" => print_help(),
                    "functions" => print_functions(environment, interner),
                    "variables" => print_variables(environment, interner),
                    "clear variables" => {
                        remove_all_custom(environment, &completer, interner, false);
                        println!("Variables cleared");
                        println!();
                    }
                    "clear functions" => {
                        remove_all_custom(environment, &completer, interner, true);
                        println!("Custom functions cleared");
                        println!();
                    }
                    _ if input.starts_with("remove ") => {
                        let name = input.trim_start_matches("remove ");

                        let func = interner.get(name).and_then(|n| environment.remove(&n));
                        if let Some(f) = func {
                            let mut hints = completer.hints.borrow_mut();
                            hints.remove(name);

                            if f.params.is_empty() {
                                println!("Removed variable \"{}\"", name);
                            } else {
                                println!("Removed function \"{}\"", name);
                            }
                            println!();
                        } else {
                            println!("Unknown variable or function \"{}\"", name);
                            println!();
                        }
                    }
                    _ => {
                        let mut hints = completer.hints.borrow_mut();
                        let helper = |hint| {
                            hints.insert(hint);
                        };
                        process_input(answer_token, &input, environment, interner, true, helper);
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(e) => {
                eprintln!("{:?}", e);
                std::process::exit(1);
            }
        }
    }
}

fn load_conf(
    answer_token: Token,
    environment: &mut HashMap<Spur, Rc<CustomFunction>>,
    interner: &mut Rodeo,
    completer: &mut AutoCompleter,
) {
    let mut path = match dirs::home_dir() {
        Some(path) => path,
        None => {
            eprintln!("Failed to get home directory");
            return;
        }
    };

    path.push("pfcalc.conf");
    if !path.exists() {
        return;
    }

    let contents = match std::fs::read_to_string(&path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Failed to load config from `{}`", path.display());
            eprintln!("{}", e);
            return;
        }
    };

    let mut hints = completer.hints.borrow_mut();
    for line in contents.lines() {
        process_input(answer_token, line, environment, interner, false, |hint| {
            hints.insert(hint);
        });
    }
}

fn main() {
    let inputs: Vec<_> = std::env::args().skip(1).collect();

    let mut environment = HashMap::new();
    let mut interner = Rodeo::default();
    let mut completer = AutoCompleter::new();

    let answer_token = Token {
        lexeme: interner.get_or_intern_static("ans"),
        source_end: 0,
        source_start: 0,
    };
    load_conf(
        answer_token,
        &mut environment,
        &mut interner,
        &mut completer,
    );

    if inputs.is_empty() {
        repl(
            answer_token,
            &mut environment,
            &mut interner,
            &mut completer,
        );
    } else {
        for input in inputs {
            println!("Evaluating Expression: {}", input);
            process_input(
                answer_token,
                &input,
                &mut environment,
                &mut interner,
                true,
                |_| {},
            );
        }
    }
}
