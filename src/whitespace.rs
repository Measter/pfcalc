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

#[derive(Copy, Clone)]
pub struct SplitWhitespaceIndicesIter<'a> {
    value: &'a str,
    cur_idx: usize,
}

impl<'a> Iterator for SplitWhitespaceIndicesIter<'a> {
    type Item = (usize, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        if self.value.is_empty() {
            return None;
        }

        let mut chars = self.value.char_indices().skip_while(|(_, c)| c.is_whitespace());
        let start_idx = if let Some((idx, _)) = chars.next() {
            idx
        } else {
            // End of string.
            self.value = "";
            return None;
        };

        let cur_value = &self.value[start_idx..];

        let end_idx = if let Some(cur_part) = cur_value.split_whitespace().next() {
            cur_part.len()
        } else {
            // End of string, so just grab the rest.
            cur_value.len()
        };

        let (ret_str, remainder) = cur_value.split_at(end_idx);
        let ret_idx = self.cur_idx + start_idx;

        self.value = remainder;
        self.cur_idx += start_idx + end_idx;

        Some((ret_idx, ret_str))
    }
}

pub fn get_spans<'a>(input: &'a Rc<str>) -> impl Iterator<Item=Span> + 'a {
    let iter = SplitWhitespaceIndicesIter {
        value: &*input,
        cur_idx: 0,
    };

    iter.map(move |(start, st)| {
        Span {
            input: Rc::clone(&input),
            start,
            len: st.len(),
        }
    })
}