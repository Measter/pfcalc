use std::{iter::Peekable, ops::Range, str::CharIndices};

use lasso::{Rodeo, Spur};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub lexeme: Spur,
    pub source_start: usize,
    pub source_end: usize,
}

impl Token {
    fn new(lexeme: Spur, range: Range<usize>) -> Self {
        Self {
            lexeme,
            source_start: range.start,
            source_end: range.end,
        }
    }

    pub fn range(self) -> Range<usize> {
        self.source_start..self.source_end
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
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

#[derive(Debug, Clone)]
pub enum OperationType {
    Number(f64),
    Native(Operator),
    Custom(Token),
}

#[derive(Debug, Clone)]
pub struct Operation {
    pub token: Token,
    pub op_type: OperationType,
}

impl Operation {
    pub fn number(num: f64) -> Self {
        Self {
            token: Token {
                lexeme: Spur::default(),
                source_start: 0,
                source_end: 0,
            },
            op_type: OperationType::Number(num),
        }
    }

    fn parse(token: Token, lexeme: &str) -> Self {
        if let Ok(num) = lexeme.parse() {
            return Operation {
                token,
                op_type: OperationType::Number(num),
            };
        }

        let op_type = match lexeme {
            "+" | "add" => OperationType::Native(Operator::Add),
            "-" | "sub" => OperationType::Native(Operator::Sub),
            "*" | "mul" => OperationType::Native(Operator::Mul),
            "/" | "div" => OperationType::Native(Operator::Div),
            "^" | "pow" => OperationType::Native(Operator::Pow),
            "%" | "rem" => OperationType::Native(Operator::Mod),

            "abs" => OperationType::Native(Operator::Abs),
            "ceil" => OperationType::Native(Operator::Ceil),
            "floor" => OperationType::Native(Operator::Floor),
            "exp" => OperationType::Native(Operator::Exp),
            "ln" => OperationType::Native(Operator::Ln),
            "log10" => OperationType::Native(Operator::Log10),
            "sqrt" => OperationType::Native(Operator::Sqrt),
            "d2rad" => OperationType::Native(Operator::D2Rad),
            "r2deg" => OperationType::Native(Operator::R2Deg),
            "round" => OperationType::Native(Operator::Rnd),

            "log" => OperationType::Native(Operator::Log),

            "cos" => OperationType::Native(Operator::Cos),
            "cosh" => OperationType::Native(Operator::Cosh),
            "acos" => OperationType::Native(Operator::ACos),
            "acosh" => OperationType::Native(Operator::ACosh),

            "sin" => OperationType::Native(Operator::Sin),
            "sinh" => OperationType::Native(Operator::Sinh),
            "asin" => OperationType::Native(Operator::ASin),
            "asinh" => OperationType::Native(Operator::ASinh),

            "tan" => OperationType::Native(Operator::Tan),
            "tanh" => OperationType::Native(Operator::Tanh),
            "atan" => OperationType::Native(Operator::Atan),
            "atanh" => OperationType::Native(Operator::Atanh),
            "atan2" => OperationType::Native(Operator::Atan2),
            "pi" => OperationType::Native(Operator::Pi),
            "e" => OperationType::Native(Operator::E),

            "sum" => OperationType::Native(Operator::Sum),
            "prod" => OperationType::Native(Operator::Product),
            _ => OperationType::Custom(token),
        };

        Operation { token, op_type }
    }
}

fn end_token(c: char) -> bool {
    matches!(c, '+' | '-' | '*' | '/' | '%' | '^' | '=') || c.is_whitespace()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScanResult<'a> {
    Token(Token, &'a str),
    Function,
    None,
}

struct Scanner<'a> {
    cur_token_start: usize,
    next_token_start: usize,
    chars: Peekable<CharIndices<'a>>,
}

impl Scanner<'_> {
    fn advance(&mut self) -> char {
        let (idx, ch) = self.chars.next().expect("unexpected end of input");
        self.next_token_start = idx + ch.len_utf8();
        ch
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn scan_token<'a>(&mut self, input: &'a str, interner: &mut Rodeo) -> ScanResult<'a> {
        let ch = self.advance();
        let next_ch = self.peek().unwrap_or_default();
        match ch {
            c if c.is_whitespace() => ScanResult::None,
            '+' | '/' | '*' | '%' | '^' => {
                let lexeme = &input[self.cur_token_start..self.next_token_start];
                ScanResult::Token(
                    Token::new(
                        interner.get_or_intern(lexeme),
                        self.cur_token_start..self.next_token_start,
                    ),
                    lexeme,
                )
            }
            '-' if !next_ch.is_ascii_digit() => {
                let lexeme = &input[self.cur_token_start..self.next_token_start];
                ScanResult::Token(
                    Token::new(
                        interner.get_or_intern(lexeme),
                        self.cur_token_start..self.next_token_start,
                    ),
                    lexeme,
                )
            }
            '-' | '0'..='9' => {
                // Integer part
                while let Some('0'..='9') = self.peek() {
                    self.advance();
                }

                // Fractional part
                if let Some('.') = self.peek() {
                    self.advance(); // Consume the '.'
                    while let Some('0'..='9') = self.peek() {
                        self.advance();
                    }
                }

                // Exponent
                if let Some('e' | 'E') = self.peek() {
                    self.advance(); // Consume the 'e'

                    // Exponent might be negative, so a possible '-' here
                    if let Some('-') = self.peek() {
                        self.advance();
                    }
                    while let Some('0'..='9') = self.peek() {
                        self.advance();
                    }
                }

                let lexeme = &input[self.cur_token_start..self.next_token_start];
                ScanResult::Token(
                    Token::new(
                        interner.get_or_intern(lexeme),
                        self.cur_token_start..self.next_token_start,
                    ),
                    lexeme,
                )
            }
            '=' => ScanResult::Function,
            _ => {
                while matches!(self.peek(), Some(c) if !end_token(c)) {
                    self.advance();
                }

                let lexeme = &input[self.cur_token_start..self.next_token_start];
                ScanResult::Token(
                    Token::new(
                        interner.get_or_intern(lexeme),
                        self.cur_token_start..self.next_token_start,
                    ),
                    lexeme,
                )
            }
        }
    }
}

pub fn lex_input(input: &str, interner: &mut Rodeo) -> (Vec<Operation>, Option<Vec<Operation>>) {
    let mut scanner = Scanner {
        chars: input.char_indices().peekable(),
        cur_token_start: 0,
        next_token_start: 0,
    };

    let mut input_tokens = Vec::new();
    let mut body_tokens = Vec::new();
    let mut is_function = false;

    while scanner.peek().is_some() {
        scanner.cur_token_start = scanner.next_token_start;

        let (token, lexeme) = match scanner.scan_token(input, interner) {
            ScanResult::Token(token, lexeme) => (token, lexeme),
            ScanResult::Function => {
                is_function = true;
                continue;
            }
            ScanResult::None => {
                continue;
            }
        };

        let operation = Operation::parse(token, lexeme);
        if is_function {
            body_tokens.push(operation);
        } else {
            input_tokens.push(operation);
        }
    }

    (input_tokens, is_function.then(|| body_tokens))
}
