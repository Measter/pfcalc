use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Span {
    input: Rc<str>,
    start: usize,
    len: usize
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
        unsafe { self.input.get_unchecked(start..end) }
    }

    pub fn input(&self) -> &str {
        &*self.input
    }

    pub fn start(&self) -> usize {
        self.start
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