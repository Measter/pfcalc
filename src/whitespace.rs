use std::{
    rc::Rc,
    hash::{Hash, Hasher},
    cmp::{Eq, PartialEq},
    borrow::Borrow,
};

#[derive(Debug, Clone)]
pub struct Span {
    input: Rc<str>,
    start: usize,
    len: usize
}

impl Hash for Span {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value().hash(state)
    }
}

impl PartialEq for Span {
    fn eq(&self, rhs: &Self) -> bool {
        self.value() == rhs.value()
    }
}

impl Eq for Span {}

impl Borrow<str> for Span {
    fn borrow(&self) -> &str {
        self.value()
    }
}

impl Span {
    pub fn new(input: Rc<str>) -> Span {
        Span {
            start: 0,
            len: input.len(),
            input,
        }
    }

    pub fn value(&self) -> &str {
        let start = self.start;
        let end = start + self.len;
        &self.input[start..end]
    }

    pub fn input(&self) -> &str {
        &*self.input
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn set_start(&mut self, start: usize) {
        assert!(start < self.len);
        self.start += start;
        self.len -= start;
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

pub fn get_parts(input: Rc<str>) -> Vec<Span> {
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

        let span = Span {
            input: Rc::clone(&input),
            start: idx,
            len: end_idx-idx,
        };
        parts.push(span);
    }

    parts
}