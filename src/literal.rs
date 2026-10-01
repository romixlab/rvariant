//! Literal syntax used to display and parse `List` and `Map`:
//! `[1, 2.5, "text", true, null, [1, 2], {"key": 1}]`.
//!
//! Integers parse as `I64` (or `U64` / `I128` / `U128` if they don't fit), floats as `F64`,
//! quoted strings as `Str`, `null` as `Empty`. Other variant kinds are written as quoted strings
//! and therefore come back as `Str`.

use crate::number::{IntVal, parse_int_literal};
use crate::{Map, Number, Variant};
use std::fmt::{Formatter, Write};

pub(crate) fn write_literal(v: &Variant, f: &mut Formatter<'_>) -> std::fmt::Result {
    match v {
        Variant::Empty => f.write_str("null"),
        Variant::Bool(b) => write!(f, "{b}"),
        Variant::Number(Number::F32(x)) => write!(f, "{:?}", x.0),
        Variant::Number(Number::F64(x)) => write!(f, "{:?}", x.0),
        Variant::Number(n) => write!(f, "{n}"),
        Variant::Str(s) => write_quoted(s, f),
        Variant::List(list) => write_list(list, f),
        Variant::Map(map) => write_map(map, f),
        other => write_quoted(&other.to_string(), f),
    }
}

pub(crate) fn write_list(list: &[Variant], f: &mut Formatter<'_>) -> std::fmt::Result {
    f.write_char('[')?;
    for (idx, v) in list.iter().enumerate() {
        if idx > 0 {
            f.write_str(", ")?;
        }
        write_literal(v, f)?;
    }
    f.write_char(']')
}

pub(crate) fn write_map(map: &Map, f: &mut Formatter<'_>) -> std::fmt::Result {
    f.write_char('{')?;
    for (idx, (k, v)) in map.0.iter().enumerate() {
        if idx > 0 {
            f.write_str(", ")?;
        }
        write_literal(k, f)?;
        f.write_str(": ")?;
        write_literal(v, f)?;
    }
    f.write_char('}')
}

fn write_quoted(s: &str, f: &mut Formatter<'_>) -> std::fmt::Result {
    f.write_char('"')?;
    for c in s.chars() {
        match c {
            '"' => f.write_str("\\\"")?,
            '\\' => f.write_str("\\\\")?,
            '\n' => f.write_str("\\n")?,
            '\r' => f.write_str("\\r")?,
            '\t' => f.write_str("\\t")?,
            c if (c as u32) < 0x20 => write!(f, "\\u{:04x}", c as u32)?,
            c => f.write_char(c)?,
        }
    }
    f.write_char('"')
}

pub(crate) fn parse_literal(s: &str) -> Result<Variant, String> {
    let mut p = Parser {
        chars: s.char_indices().collect(),
        pos: 0,
    };
    let v = p.value(0)?;
    p.ws();
    if p.pos != p.chars.len() {
        return Err(format!("unexpected trailing input at {}", p.offset()));
    }
    Ok(v)
}

const MAX_DEPTH: usize = 64;

struct Parser {
    chars: Vec<(usize, char)>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).map(|(_, c)| *c)
    }

    fn offset(&self) -> usize {
        self.chars
            .get(self.pos)
            .map(|(o, _)| *o)
            .unwrap_or(usize::MAX)
    }

    fn ws(&mut self) {
        while self.peek().is_some_and(char::is_whitespace) {
            self.pos += 1;
        }
    }

    fn expect(&mut self, c: char) -> Result<(), String> {
        self.ws();
        if self.peek() == Some(c) {
            self.pos += 1;
            Ok(())
        } else {
            Err(format!("expected '{c}' at {}", self.offset()))
        }
    }

    fn value(&mut self, depth: usize) -> Result<Variant, String> {
        if depth > MAX_DEPTH {
            return Err("nesting is too deep".into());
        }
        self.ws();
        match self.peek() {
            None => Err("unexpected end of input".into()),
            Some('[') => {
                self.pos += 1;
                let mut list = vec![];
                self.ws();
                if self.peek() == Some(']') {
                    self.pos += 1;
                    return Ok(Variant::List(list));
                }
                loop {
                    list.push(self.value(depth + 1)?);
                    self.ws();
                    match self.peek() {
                        Some(',') => self.pos += 1,
                        Some(']') => {
                            self.pos += 1;
                            return Ok(Variant::List(list));
                        }
                        _ => return Err(format!("expected ',' or ']' at {}", self.offset())),
                    }
                }
            }
            Some('{') => {
                self.pos += 1;
                let mut map = Map::default();
                self.ws();
                if self.peek() == Some('}') {
                    self.pos += 1;
                    return Ok(Variant::Map(map));
                }
                loop {
                    let k = self.value(depth + 1)?;
                    self.expect(':')?;
                    let v = self.value(depth + 1)?;
                    map.0.insert(k, v);
                    self.ws();
                    match self.peek() {
                        Some(',') => self.pos += 1,
                        Some('}') => {
                            self.pos += 1;
                            return Ok(Variant::Map(map));
                        }
                        _ => return Err(format!("expected ',' or '}}' at {}", self.offset())),
                    }
                }
            }
            Some('"') => {
                self.pos += 1;
                self.string().map(Variant::Str)
            }
            Some(_) => {
                let start = self.pos;
                while self
                    .peek()
                    .is_some_and(|c| !c.is_whitespace() && !matches!(c, ',' | ']' | '}' | ':'))
                {
                    self.pos += 1;
                }
                let token: String = self.chars[start..self.pos].iter().map(|(_, c)| c).collect();
                token_to_variant(&token).ok_or_else(|| format!("unexpected token {token:?}"))
            }
        }
    }

    fn string(&mut self) -> Result<String, String> {
        let mut out = String::new();
        loop {
            let Some(c) = self.peek() else {
                return Err("unterminated string".into());
            };
            self.pos += 1;
            match c {
                '"' => return Ok(out),
                '\\' => {
                    let Some(e) = self.peek() else {
                        return Err("unterminated escape".into());
                    };
                    self.pos += 1;
                    match e {
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        '/' => out.push('/'),
                        'n' => out.push('\n'),
                        'r' => out.push('\r'),
                        't' => out.push('\t'),
                        'u' => {
                            let hex: String = (0..4)
                                .filter_map(|_| {
                                    let c = self.peek()?;
                                    self.pos += 1;
                                    Some(c)
                                })
                                .collect();
                            let code = u32::from_str_radix(&hex, 16)
                                .map_err(|_| format!("bad unicode escape \\u{hex}"))?;
                            out.push(
                                char::from_u32(code)
                                    .ok_or_else(|| format!("bad unicode escape \\u{hex}"))?,
                            );
                        }
                        other => return Err(format!("unknown escape \\{other}")),
                    }
                }
                c => out.push(c),
            }
        }
    }
}

fn token_to_variant(token: &str) -> Option<Variant> {
    match token {
        "null" => return Some(Variant::Empty),
        "true" => return Some(Variant::Bool(true)),
        "false" => return Some(Variant::Bool(false)),
        _ => {}
    }
    if let Some(v) = parse_int_literal(&token.replace('_', "")) {
        return Some(Variant::Number(match v {
            IntVal::Neg(i) => match i64::try_from(i) {
                Ok(i) => Number::I64(i),
                Err(_) => Number::I128(i),
            },
            IntVal::Pos(u) => {
                if let Ok(i) = i64::try_from(u) {
                    Number::I64(i)
                } else if let Ok(u) = u64::try_from(u) {
                    Number::U64(u)
                } else {
                    Number::U128(u)
                }
            }
        }));
    }
    token
        .parse::<f64>()
        .ok()
        .map(|f| Variant::Number(Number::F64(f.into())))
}
