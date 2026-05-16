use crate::runtime::Runtime;
use crate::value::Value;

pub struct Reader {
    input: Vec<char>,
    pos: usize,
    renaming_quasiquote: bool,
}

impl Reader {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
            renaming_quasiquote: false,
        }
    }

    pub fn read(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        self.skip_ws_and_comments()?;
        if self.eof() {
            return Ok(Value::Eof);
        }
        self.read_expr(rt)
    }

    fn read_expr(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        self.skip_ws_and_comments()?;
        let Some(ch) = self.peek() else {
            return Ok(Value::Eof);
        };

        match ch {
            '(' => self.read_list(rt),
            ')' => Err("unexpected right paren".to_string()),
            '\'' => {
                self.bump();
                let expr = self.read_expr(rt)?;
                Ok(rt.list(vec![Value::Symbol("quote".to_string()), expr]))
            }
            '`' => {
                self.bump();
                let expr = self.read_expr(rt)?;
                Ok(rt.list(vec![Value::Symbol("quasiquote".to_string()), expr]))
            }
            ',' => {
                self.bump();
                if self.peek() == Some('@') {
                    self.bump();
                    let old = self.renaming_quasiquote;
                    self.renaming_quasiquote = false;
                    let expr = self.read_expr(rt);
                    self.renaming_quasiquote = old;
                    Ok(rt.list(vec![Value::Symbol("unquote-splicing".to_string()), expr?]))
                } else {
                    let old = self.renaming_quasiquote;
                    self.renaming_quasiquote = false;
                    let expr = self.read_expr(rt);
                    self.renaming_quasiquote = old;
                    Ok(rt.list(vec![Value::Symbol("unquote".to_string()), expr?]))
                }
            }
            '"' => self.read_string(rt),
            '#' => self.read_sharp(rt),
            '{' => self.read_table(rt),
            '}' => Err("unexpected right bracket".to_string()),
            '.' if self.next_is_delimiter() => Err("unexpected . at toplevel".to_string()),
            _ => self.read_atom(rt),
        }
    }

    fn read_list(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        self.expect('(')?;
        let mut values = Vec::new();

        loop {
            self.skip_expression_comments(rt)?;
            if self.eof() {
                return Err("unexpected end of input in list".to_string());
            }
            if self.peek() == Some(')') {
                self.bump();
                return Ok(rt.list(values));
            }
            if self.peek() == Some('.') && self.next_is_delimiter() {
                self.bump();
                if values.is_empty() {
                    return Err("unexpected . at beginning of list".to_string());
                }
                let tail = self.read_expr(rt)?;
                self.skip_ws_and_comments()?;
                self.expect(')')?;
                return Ok(rt.dotted_list(values, tail));
            }
            values.push(self.read_expr(rt)?);
        }
    }

    fn read_vector(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        let mut values = Vec::new();
        loop {
            self.skip_expression_comments(rt)?;
            if self.eof() {
                return Err("unexpected end of input in vector".to_string());
            }
            if self.peek() == Some(')') {
                self.bump();
                return Ok(rt.vector(values));
            }
            values.push(self.read_expr(rt)?);
        }
    }

    fn read_table(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        self.expect('{')?;
        let table = rt.table();
        loop {
            self.skip_expression_comments(rt)?;
            if self.eof() {
                return Err("unexpected end of input in table literal".to_string());
            }
            if self.peek() == Some('}') {
                self.bump();
                return Ok(table);
            }
            let key = self.read_expr(rt)?;
            let value = self.read_expr(rt)?;
            rt.table_set_value(&table, key, value)?;
            self.skip_ws_and_comments()?;
            if self.peek() == Some('.') {
                self.bump();
            }
        }
    }

    fn read_string(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        self.expect('"')?;
        let mut out = String::new();
        while let Some(ch) = self.bump() {
            match ch {
                '"' => return Ok(rt.string(out)),
                '\\' => {
                    let Some(esc) = self.bump() else {
                        return Err("unexpected end of input in string".to_string());
                    };
                    match esc {
                        'n' => out.push('\n'),
                        'r' => out.push('\r'),
                        't' => out.push('\t'),
                        '"' | '\\' => out.push(esc),
                        other => return Err(format!("unknown string escape \\{other}")),
                    }
                }
                other => out.push(other),
            }
        }
        Err("unexpected end of input in string".to_string())
    }

    fn read_sharp(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        self.expect('#')?;
        let Some(ch) = self.peek() else {
            return Err("unexpected end of input after #".to_string());
        };

        match ch {
            't' => {
                self.bump();
                Ok(Value::Bool(true))
            }
            'f' => {
                self.bump();
                Ok(Value::Bool(false))
            }
            '(' => {
                self.bump();
                self.read_vector(rt)
            }
            '\\' => {
                self.bump();
                self.read_character()
            }
            '!' => {
                self.bump();
                let token = self.read_token();
                match token.as_str() {
                    "optional" => Ok(Value::OptionalObject),
                    "key" => Ok(Value::KeyObject),
                    "keys" => Ok(Value::KeysObject),
                    "rest" => Ok(Value::RestObject),
                    _ => Err(format!("unknown #! constant {token}")),
                }
            }
            ';' => {
                self.bump();
                let _ = self.read_expr(rt)?;
                self.read(rt)
            }
            '|' => {
                self.skip_multiline_comment()?;
                self.read(rt)
            }
            '`' => {
                self.bump();
                let old = self.renaming_quasiquote;
                self.renaming_quasiquote = true;
                let expr = self.read_expr(rt);
                self.renaming_quasiquote = old;
                Ok(rt.list(vec![Value::Symbol("quasiquote".to_string()), expr?]))
            }
            '\'' => {
                self.bump();
                let old = self.renaming_quasiquote;
                self.renaming_quasiquote = false;
                let expr = self.read_expr(rt);
                self.renaming_quasiquote = old;
                let quoted = rt.list(vec![Value::Symbol("quote".to_string()), expr?]);
                Ok(rt.list(vec![Value::Symbol("rename".to_string()), quoted]))
            }
            'x' | 'o' | 'b' | 'd' | 'e' | 'i' => {
                self.pos -= 1;
                self.read_atom(rt)
            }
            '#' => {
                let token = self.read_token_with_prefix("#");
                Ok(Value::Symbol(token))
            }
            _ => Err("unknown # read syntax".to_string()),
        }
    }

    fn read_character(&mut self) -> Result<Value, String> {
        let token = self.read_token();
        match token.as_str() {
            "space" => Ok(Value::Char(' ')),
            "newline" => Ok(Value::Char('\n')),
            "tab" => Ok(Value::Char('\t')),
            "return" => Ok(Value::Char('\r')),
            "" => Err("missing character literal".to_string()),
            _ if token.chars().count() == 1 => Ok(Value::Char(token.chars().next().unwrap())),
            _ => Err(format!("unknown character literal {token}")),
        }
    }

    fn read_atom(&mut self, rt: &mut Runtime) -> Result<Value, String> {
        let token = self.read_token();
        if token.is_empty() {
            return Err("empty token".to_string());
        }

        if looks_numeric(&token) {
            if let Some(value) = parse_number(&token) {
                return Ok(value);
            }
        }

        let sym = Value::Symbol(token);
        if self.renaming_quasiquote && !sym.is_keyword_symbol() {
            let quoted = rt.list(vec![Value::Symbol("quote".to_string()), sym]);
            let renamed = rt.list(vec![Value::Symbol("rename".to_string()), quoted]);
            Ok(rt.list(vec![Value::Symbol("unquote".to_string()), renamed]))
        } else {
            Ok(sym)
        }
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), String> {
        loop {
            while matches!(self.peek(), Some(' ' | '\r' | '\t' | '\n' | '\u{000c}')) {
                self.bump();
            }

            if self.peek() == Some(';') {
                while let Some(ch) = self.bump() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            if self.peek() == Some('#') && self.peek_n(1) == Some('|') {
                self.skip_multiline_comment()?;
                continue;
            }

            break;
        }
        Ok(())
    }

    fn skip_expression_comments(&mut self, rt: &mut Runtime) -> Result<(), String> {
        loop {
            self.skip_ws_and_comments()?;
            if self.peek() == Some('#') && self.peek_n(1) == Some(';') {
                self.bump();
                self.bump();
                let _ = self.read_expr(rt)?;
                continue;
            }
            break;
        }
        Ok(())
    }

    fn skip_multiline_comment(&mut self) -> Result<(), String> {
        self.expect('#')?;
        self.expect('|')?;
        let mut depth = 1usize;
        while let Some(ch) = self.bump() {
            if ch == '#' && self.peek() == Some('|') {
                self.bump();
                depth += 1;
            } else if ch == '|' && self.peek() == Some('#') {
                self.bump();
                depth -= 1;
                if depth == 0 {
                    return Ok(());
                }
            }
        }
        Err("unexpected end of input in #| multiline comment".to_string())
    }

    fn read_token(&mut self) -> String {
        let mut out = String::new();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            out.push(ch);
            self.bump();
        }
        out
    }

    fn read_token_with_prefix(&mut self, prefix: &str) -> String {
        let mut out = prefix.to_string();
        while let Some(ch) = self.peek() {
            if is_delimiter(ch) {
                break;
            }
            out.push(ch);
            self.bump();
        }
        out
    }

    fn expect(&mut self, expected: char) -> Result<(), String> {
        match self.bump() {
            Some(ch) if ch == expected => Ok(()),
            Some(ch) => Err(format!("expected {expected}, got {ch}")),
            None => Err(format!("expected {expected}, got end of input")),
        }
    }

    fn eof(&self) -> bool {
        self.pos >= self.input.len()
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn peek_n(&self, n: usize) -> Option<char> {
        self.input.get(self.pos + n).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn next_is_delimiter(&self) -> bool {
        self.peek_n(1).map_or(true, is_delimiter)
    }
}

fn is_delimiter(ch: char) -> bool {
    matches!(
        ch,
        ' ' | '\r' | '\t' | '\n' | '\u{000c}' | '(' | ')' | '"' | ';' | '{' | '}'
    )
}

fn looks_numeric(token: &str) -> bool {
    let stripped = token
        .trim_start_matches("#e")
        .trim_start_matches("#i")
        .trim_start_matches("#x")
        .trim_start_matches("#b")
        .trim_start_matches("#o")
        .trim_start_matches("#d");
    let mut chars = stripped.chars();
    match chars.next() {
        Some(ch) if ch.is_ascii_digit() => true,
        Some('+') | Some('-') => chars
            .next()
            .map_or(false, |ch| ch.is_ascii_digit() || ch == '.'),
        Some('.') => chars.next().map_or(false, |ch| ch.is_ascii_digit()),
        _ => token.starts_with('#'),
    }
}

fn parse_number(token: &str) -> Option<Value> {
    let mut rest = token;
    let mut radix = 10;
    let mut exact = true;
    let mut exact_set = false;

    loop {
        if let Some(after) = rest.strip_prefix("#e") {
            exact = true;
            exact_set = true;
            rest = after;
        } else if let Some(after) = rest.strip_prefix("#i") {
            exact = false;
            exact_set = true;
            rest = after;
        } else if let Some(after) = rest.strip_prefix("#x") {
            radix = 16;
            rest = after;
        } else if let Some(after) = rest.strip_prefix("#b") {
            radix = 2;
            rest = after;
        } else if let Some(after) = rest.strip_prefix("#o") {
            radix = 8;
            rest = after;
        } else if let Some(after) = rest.strip_prefix("#d") {
            radix = 10;
            rest = after;
        } else {
            break;
        }
    }

    if rest.is_empty() {
        return None;
    }

    let negative = rest.starts_with('-');
    let rest = rest.strip_prefix(['+', '-']).unwrap_or(rest);
    if rest.is_empty() {
        return None;
    }

    let float_syntax = radix == 10 && (rest.contains('.') || rest.contains('e') || rest.contains('E'));
    if float_syntax {
        let signed = if negative {
            format!("-{rest}")
        } else {
            rest.to_string()
        };
        let f = signed.parse::<f64>().ok()?;
        if exact_set && exact && f.fract() == 0.0 {
            Some(Value::Fixnum(f as i64))
        } else {
            Some(Value::Flonum(f))
        }
    } else {
        let value = i64::from_str_radix(rest, radix).ok()?;
        let value = if negative { -value } else { value };
        if exact {
            Some(Value::Fixnum(value))
        } else {
            Some(Value::Flonum(value as f64))
        }
    }
}
