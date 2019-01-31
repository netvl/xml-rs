use reader2::error::{Result, ParseError};
use attribute2::Attribute;
use name2::Name;
use chars::{is_whitespace_str, is_whitespace_char, is_name_char, is_name_start_char};

// Expects buffer to contain only key='value'/key="value" pairs, possibly separated by whitespace
pub struct Attributes<'buf> {
    buffer: &'buf str,
    next_char: char,
}

impl<'buf> Attributes<'buf> {
    pub fn new(buffer: &'buf str, next_char: char) -> Attributes<'buf> {
        Attributes { buffer, next_char, }
    }

    pub fn parse(buffer: &'buf str, next_char: char) -> Result<Vec<Attribute<'buf>>> {
        Attributes::new(buffer, next_char).collect()
    }

    fn next_unexpected_token<T>(&self, expected: &[&str]) -> Result<T> {
        Err(ParseError::unexpected_token(self.next_char.to_string(), expected).into())
    }

    fn first_unexpected_token<T>(&self, expected: &[&str]) -> Result<T> {
        Err(ParseError::unexpected_token(self.buffer.chars().next().unwrap().to_string(), expected).into())
    }
}

impl<'buf> Iterator for Attributes<'buf> {
    type Item = Result<Attribute<'buf>>;

    // TODO: replace predicates with literal matches for whitespace, which can potentially be faster
    fn next(&mut self) -> Option<Result<Attribute<'buf>>> {
        if self.buffer.is_empty() || is_whitespace_str(self.buffer) {
            return None;
        }

        let attr_name_start = match self.buffer.find(is_name_start_char) {
            Some(idx) => idx,
            None => return Some(self.first_unexpected_token(&["name start character"])),
        };
        if !self.buffer[..attr_name_start].is_empty() && !is_whitespace_str(&self.buffer[..attr_name_start]) {
            return Some(self.first_unexpected_token(&["name start character"]));
        }

        self.buffer = &self.buffer[attr_name_start..];

        let attr_name_end = match self.buffer.find(|c| is_whitespace_char(c) || c == '=') {
            Some(idx) => idx,
            None => return Some(self.next_unexpected_token(&["=", "whitespace"])),
        };

        let attr_name = &self.buffer[..attr_name_end];
        if let Some(idx) = attr_name.find(|c| !is_name_char(c)) {
            // FIXME: does not seem right, the error would point to the start of name, not to the
            // FIXME: invalid character
            return Some(self.first_unexpected_token(&["name character"]));
        }
        let attr_name = match Name::from_str(attr_name) {
            Some(name) => name,
            None => return Some(Err(ParseError::invalid_attribute_name(attr_name).into())),
        };

        self.buffer = &self.buffer[attr_name_end..];

        if !self.buffer.starts_with("=") {
            let eq_idx = match self.buffer.find('=') {
                Some(idx) => idx,
                None => return Some(self.next_unexpected_token(&["="])),
            };

            if !self.buffer[..eq_idx].is_empty() && !is_whitespace_str(&self.buffer[..eq_idx]) {
                return Some(self.first_unexpected_token(&["=", "whitespace"]));
            }

            self.buffer = &self.buffer[eq_idx + 1..];
        } else {
            self.buffer = &self.buffer[1..];
        }

        let quote_start = match self.buffer.find(|c| c == '\'' || c == '"') {
            Some(idx) => idx,
            None => return Some(self.next_unexpected_token(&["'", "\""])),
        };

        if !self.buffer[..quote_start].is_empty() && !is_whitespace_str(&self.buffer[..quote_start]) {
            return Some(self.first_unexpected_token(&["="]));
        }

        // FIXME: handle whitespace before quote
        let quote_char = self.buffer[quote_start..].chars().next().unwrap();

        self.buffer = &self.buffer[quote_start + 1..];
        let quote_end = match self.buffer.find(quote_char) {
            Some(idx) => idx,
            None => return Some(Err(ParseError::UnclosedAttributeValue.into())),
        };

        let attr_value = &self.buffer[..quote_end];

        // TODO: validate value and expand character entities in it

        self.buffer = &self.buffer[quote_end + 1..];

        Some(Ok(Attribute {
            name: attr_name,
            value: attr_value.into(),
        }))
    }
}
