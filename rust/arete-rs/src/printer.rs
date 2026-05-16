use std::collections::{HashMap, HashSet};

use crate::value::{ObjectKind, Value};

pub fn write_value(value: &Value) -> String {
    write_plain(value)
}

pub fn display_value(value: &Value) -> String {
    match value {
        Value::Object(obj) => match &obj.borrow().kind {
            ObjectKind::String(s) => s.clone(),
            _ => write_plain(value),
        },
        Value::Char(ch) => ch.to_string(),
        _ => write_plain(value),
    }
}

pub fn pretty_value(value: &Value) -> String {
    let mut counts = HashMap::new();
    let mut seen = HashSet::new();
    count_refs(value, &mut counts, &mut seen);

    let mut state = SharedPrintState {
        counts,
        labels: HashMap::new(),
        started: HashSet::new(),
        next_label: 0,
    };
    write_shared(value, &mut state)
}

fn write_plain(value: &Value) -> String {
    match value {
        Value::Bool(true) => "#t".to_string(),
        Value::Bool(false) => "#f".to_string(),
        Value::Nil => "()".to_string(),
        Value::Eof => "#<eof>".to_string(),
        Value::Unspecified => "#<unspecified>".to_string(),
        Value::Undefined => "#<undefined>".to_string(),
        Value::OptionalObject => "#!optional".to_string(),
        Value::KeyObject => "#!key".to_string(),
        Value::KeysObject => "#!keys".to_string(),
        Value::RestObject => "#!rest".to_string(),
        Value::Fixnum(i) => i.to_string(),
        Value::Flonum(f) => format_flonum(*f),
        Value::Char(ch) => write_char(*ch),
        Value::Symbol(name) => name.clone(),
        Value::Builtin(_) => "#&cfn".to_string(),
        Value::Object(obj) => match &obj.borrow().kind {
            ObjectKind::Pair(_) => write_pair_plain(value),
            ObjectKind::Vector(values) => {
                let items = values.iter().map(write_plain).collect::<Vec<_>>().join(" ");
                format!("#({items})")
            }
            ObjectKind::String(s) => write_string(s),
            ObjectKind::Table(entries) => {
                let mut out = String::from("{");
                for (key, val) in entries {
                    out.push_str(&write_plain(key));
                    out.push(' ');
                    out.push_str(&write_plain(val));
                    out.push_str(" . ");
                }
                out.push('}');
                out
            }
            ObjectKind::Function(function) => {
                let name = function.name.as_deref().unwrap_or("#f");
                format!("#<function {name}>")
            }
            ObjectKind::RecordType(rt) => {
                format!(
                    "#<record-type {} {} 0>",
                    rt.name, rt.own_field_count
                )
            }
            ObjectKind::Record(record) => {
                let rt_name = match record.record_type.object_kind() {
                    Some(ObjectKind::RecordType(rt)) => rt.name,
                    _ => "record".to_string(),
                };
                let fields = record
                    .fields
                    .iter()
                    .map(write_plain)
                    .collect::<Vec<_>>()
                    .join(" ");
                if fields.is_empty() {
                    format!("#<{rt_name}>")
                } else {
                    format!("#<{rt_name} {fields}>")
                }
            }
            ObjectKind::InputPort(_) => "#<input-file-port>".to_string(),
            ObjectKind::OutputStringPort(_) => "#<output-string-port>".to_string(),
            ObjectKind::Exception(exc) => {
                format!(
                    "#<exception '{} {}>",
                    write_plain(&exc.tag),
                    exc.message
                )
            }
        },
    }
}

fn write_pair_plain(value: &Value) -> String {
    let mut out = String::from("(");
    let mut first = true;
    let mut cursor = value.clone();

    loop {
        match cursor.object_kind() {
            Some(ObjectKind::Pair(pair)) => {
                if !first {
                    out.push(' ');
                }
                first = false;
                out.push_str(&write_plain(&pair.car));
                cursor = pair.cdr;
            }
            _ => {
                if !matches!(cursor, Value::Nil) {
                    out.push_str(" . ");
                    out.push_str(&write_plain(&cursor));
                }
                out.push(')');
                return out;
            }
        }
    }
}

fn write_string(s: &str) -> String {
    let mut out = String::from("\"");
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn write_char(ch: char) -> String {
    match ch {
        '\u{1b}' => "#\\backspace".to_string(),
        '\u{7f}' => "#\\delete".to_string(),
        '\u{7}' => "#\\alarm".to_string(),
        '\t' => "#\\tab".to_string(),
        '\r' => "#\\return".to_string(),
        '\n' => "#\\newline".to_string(),
        ' ' => "#\\space".to_string(),
        other => format!("#\\{other}"),
    }
}

fn format_flonum(f: f64) -> String {
    if f.fract() == 0.0 {
        format!("{f:.1}")
    } else {
        f.to_string()
    }
}

struct SharedPrintState {
    counts: HashMap<usize, usize>,
    labels: HashMap<usize, usize>,
    started: HashSet<usize>,
    next_label: usize,
}

fn count_refs(value: &Value, counts: &mut HashMap<usize, usize>, seen: &mut HashSet<usize>) {
    let Some(id) = value.object_id() else {
        return;
    };
    *counts.entry(id).or_insert(0) += 1;
    if !seen.insert(id) {
        return;
    }

    match value.object_kind() {
        Some(ObjectKind::Pair(pair)) => {
            count_refs(&pair.car, counts, seen);
            count_refs(&pair.cdr, counts, seen);
        }
        Some(ObjectKind::Vector(values)) => {
            for value in values {
                count_refs(&value, counts, seen);
            }
        }
        Some(ObjectKind::Record(record)) => {
            count_refs(&record.record_type, counts, seen);
            for field in record.fields {
                count_refs(&field, counts, seen);
            }
        }
        Some(ObjectKind::Table(entries)) => {
            for (key, value) in entries {
                count_refs(&key, counts, seen);
                count_refs(&value, counts, seen);
            }
        }
        _ => {}
    }
}

fn write_shared(value: &Value, state: &mut SharedPrintState) -> String {
    let Some(id) = value.object_id() else {
        return write_plain(value);
    };
    let shared = state.counts.get(&id).copied().unwrap_or(0) > 1;
    if shared {
        if let Some(label) = state.labels.get(&id).copied() {
            if state.started.contains(&id) {
                return format!("#{label}#");
            }
        }
        let label = state.next_label;
        state.next_label += 1;
        state.labels.insert(id, label);
        state.started.insert(id);
        return format!("#{label}={}", write_shared_body(value, state));
    }
    write_shared_body(value, state)
}

fn write_shared_body(value: &Value, state: &mut SharedPrintState) -> String {
    match value.object_kind() {
        Some(ObjectKind::Pair(_)) => write_pair_shared(value, state),
        Some(ObjectKind::Vector(values)) => {
            let items = values
                .iter()
                .map(|value| write_shared(value, state))
                .collect::<Vec<_>>()
                .join(" ");
            format!("#({items})")
        }
        _ => write_plain(value),
    }
}

fn write_pair_shared(value: &Value, state: &mut SharedPrintState) -> String {
    let mut out = String::from("(");
    let mut first = true;
    let mut cursor = value.clone();

    loop {
        match cursor.object_kind() {
            Some(ObjectKind::Pair(pair)) => {
                if !first {
                    out.push(' ');
                }
                first = false;
                out.push_str(&write_shared(&pair.car, state));
                cursor = pair.cdr;
            }
            _ => {
                if !matches!(cursor, Value::Nil) {
                    out.push_str(" . ");
                    out.push_str(&write_shared(&cursor, state));
                }
                out.push(')');
                return out;
            }
        }
    }
}
