use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::fmt;
use std::rc::Rc;

use crate::printer::{display_value, pretty_value, write_value};
use crate::reader::Reader;
use crate::value::{
    Builtin, ExceptionValue, Function, InputPort, Object, ObjectKind, Pair, Params, Record,
    RecordType, Value,
};

pub type Cell = Rc<RefCell<Value>>;
pub type EnvRef = Rc<Env>;

pub struct RuntimeError {
    message: String,
    exception: Option<Value>,
}

impl RuntimeError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            exception: None,
        }
    }

    fn raised(message: impl Into<String>, exception: Value) -> Self {
        Self {
            message: message.into(),
            exception: Some(exception),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Evaluation error: {}", self.message)
    }
}

pub struct Env {
    parent: Option<EnvRef>,
    vars: RefCell<HashMap<String, Cell>>,
}

impl Env {
    fn new(parent: Option<EnvRef>) -> EnvRef {
        Rc::new(Env {
            parent,
            vars: RefCell::new(HashMap::new()),
        })
    }

    fn define(&self, name: String, value: Value) {
        self.vars
            .borrow_mut()
            .insert(name, Rc::new(RefCell::new(value)));
    }

    fn lookup_cell(&self, name: &str) -> Option<Cell> {
        if let Some(value) = self.vars.borrow().get(name) {
            Some(value.clone())
        } else {
            self.parent.as_ref().and_then(|parent| parent.lookup_cell(name))
        }
    }
}

pub struct Runtime {
    next_id: usize,
    top_env: EnvRef,
}

impl Runtime {
    pub fn new() -> Self {
        let top_env = Env::new(None);
        let mut rt = Self { next_id: 1, top_env };
        rt.install_builtins();
        rt
    }

    pub fn eval_program(&mut self, forms: Vec<Value>) -> Result<Value, RuntimeError> {
        self.eval_sequence_tail(forms, self.top_env.clone())
    }

    pub fn pair(&mut self, car: Value, cdr: Value) -> Value {
        self.object(ObjectKind::Pair(Pair { car, cdr }))
    }

    pub fn list(&mut self, values: Vec<Value>) -> Value {
        self.dotted_list(values, Value::Nil)
    }

    pub fn dotted_list(&mut self, values: Vec<Value>, tail: Value) -> Value {
        let mut out = tail;
        for value in values.into_iter().rev() {
            out = self.pair(value, out);
        }
        out
    }

    pub fn vector(&mut self, values: Vec<Value>) -> Value {
        self.object(ObjectKind::Vector(values))
    }

    pub fn string(&mut self, value: String) -> Value {
        self.object(ObjectKind::String(value))
    }

    pub fn table(&mut self) -> Value {
        self.object(ObjectKind::Table(Vec::new()))
    }

    pub fn table_set_value(&mut self, table: &Value, key: Value, value: Value) -> Result<Value, String> {
        let Value::Object(obj) = table else {
            return Err("table-set!: expected table".to_string());
        };
        let mut obj = obj.borrow_mut();
        let ObjectKind::Table(entries) = &mut obj.kind else {
            return Err("table-set!: expected table".to_string());
        };
        if let Some((_, existing)) = entries
            .iter_mut()
            .find(|(entry_key, _)| value_equal(entry_key, &key))
        {
            *existing = value;
        } else {
            entries.push((key, value));
        }
        Ok(Value::Unspecified)
    }

    fn object(&mut self, kind: ObjectKind) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        Value::Object(Rc::new(RefCell::new(Object { id, kind })))
    }

    fn install_builtins(&mut self) {
        use Builtin::*;
        let builtins = [
            ("+", Add),
            ("-", Sub),
            ("*", Mul),
            ("/", Div),
            ("=", NumEq),
            ("<", Lt),
            (">", Gt),
            ("<=", Lte),
            (">=", Gte),
            ("fx+", FxAdd),
            ("fx-", FxSub),
            ("fx<", FxLt),
            ("fx=", FxEq),
            ("floor", Floor),
            ("cons", Cons),
            ("car", Car),
            ("cdr", Cdr),
            ("set-car!", SetCar),
            ("set-cdr!", SetCdr),
            ("list", List),
            ("eq?", Eq),
            ("eqv?", Eqv),
            ("equal?", Equal),
            ("print", Print),
            ("pretty-print", PrettyPrint),
            ("make-table", MakeTable),
            ("table-ref", TableRef),
            ("table-set!", TableSet),
            ("table-for-each", TableForEach),
            ("open-input-file", OpenInputFile),
            ("close-input-port", CloseInputPort),
            ("open-output-string", OpenOutputString),
            ("close-output-port", CloseOutputPort),
            ("get-output-string", GetOutputString),
            ("read", Read),
            ("write", Write),
            ("display", Display),
            ("newline", Newline),
            ("string-copy", StringCopy),
            ("value-bits", ValueBits),
            ("gc-collect", GcCollect),
            ("try", Try),
            ("raise", Raise),
            ("exception-tag", ExceptionTag),
            ("exception-message", ExceptionMessage),
            ("exception-irritants", ExceptionIrritants),
            ("unwind-protect", UnwindProtect),
            ("register-record-type", RegisterRecordType),
            ("make-record", MakeRecord),
            ("record-set!", RecordSet),
            ("record-ref", RecordRef),
            ("record-isa?", RecordIsa),
            ("set-record-type-printer!", SetRecordTypePrinter),
            ("set-record-type-apply", SetRecordTypeApply),
        ];

        for (name, builtin) in builtins {
            self.top_env.define(name.to_string(), Value::Builtin(builtin));
        }

        self.top_env.define("unspecified".to_string(), Value::Unspecified);
        self.top_env.define("undefined".to_string(), Value::Undefined);
    }

    fn eval(&mut self, expr: Value, env: EnvRef) -> Result<Value, RuntimeError> {
        self.eval_tail(expr, env)
    }

    fn eval_sequence_tail(
        &mut self,
        exprs: Vec<Value>,
        env: EnvRef,
    ) -> Result<Value, RuntimeError> {
        if exprs.is_empty() {
            return Ok(Value::Unspecified);
        }
        let mut exprs = exprs;
        while exprs.len() > 1 {
            let expr = exprs.remove(0);
            self.eval(expr, env.clone())?;
        }
        self.eval_tail(exprs.remove(0), env)
    }

    fn eval_tail(&mut self, mut expr: Value, mut env: EnvRef) -> Result<Value, RuntimeError> {
        loop {
            match expr.clone() {
                Value::Fixnum(_)
                | Value::Flonum(_)
                | Value::Char(_)
                | Value::Bool(_)
                | Value::Nil
                | Value::Eof
                | Value::Unspecified
                | Value::Undefined
                | Value::OptionalObject
                | Value::KeyObject
                | Value::KeysObject
                | Value::RestObject
                | Value::Builtin(_) => return Ok(expr),
                Value::Object(obj) => match &obj.borrow().kind {
                    ObjectKind::Pair(_) => {}
                    _ => return Ok(expr),
                },
                Value::Symbol(name) => {
                    if name.ends_with(':') {
                        return Ok(Value::Symbol(name));
                    }
                    let Some(cell) = env.lookup_cell(&name) else {
                        return Err(RuntimeError::new(format!("unbound symbol {name}")));
                    };
                    return Ok(cell.borrow().clone());
                }
            }

            let (operator, operands) = list_head_tail(&expr)?;
            if let Value::Symbol(name) = &operator {
                match name.as_str() {
                    "quote" => return one_operand(&operands, "quote"),
                    "if" => {
                        let items = proper_list(&operands)?;
                        if items.len() < 2 || items.len() > 3 {
                            return Err(RuntimeError::new("if expected two or three operands"));
                        }
                        let test = self.eval(items[0].clone(), env.clone())?;
                        expr = if test.is_truthy() {
                            items[1].clone()
                        } else if items.len() == 3 {
                            items[2].clone()
                        } else {
                            Value::Unspecified
                        };
                        continue;
                    }
                    "begin" => {
                        let items = proper_list(&operands)?;
                        if items.is_empty() {
                            return Ok(Value::Unspecified);
                        }
                        let mut seq = items;
                        while seq.len() > 1 {
                            self.eval(seq.remove(0), env.clone())?;
                        }
                        expr = seq.remove(0);
                        continue;
                    }
                    "define" => {
                        self.eval_define(&operands, env.clone())?;
                        return Ok(Value::Unspecified);
                    }
                    "set!" => {
                        let items = proper_list(&operands)?;
                        if items.len() != 2 {
                            return Err(RuntimeError::new("set! expected two operands"));
                        }
                        let Some(name) = items[0].symbol_name() else {
                            return Err(RuntimeError::new("set! expected a symbol"));
                        };
                        let value = self.eval(items[1].clone(), env.clone())?;
                        let Some(cell) = env.lookup_cell(name) else {
                            return Err(RuntimeError::new(format!("set!: unbound symbol {name}")));
                        };
                        *cell.borrow_mut() = value;
                        return Ok(Value::Unspecified);
                    }
                    "lambda" => {
                        return self.eval_lambda(&operands, env.clone());
                    }
                    "let" => {
                        let items = proper_list(&operands)?;
                        if items.len() < 2 {
                            return Err(RuntimeError::new("let expected bindings and body"));
                        }
                        let new_env = Env::new(Some(env.clone()));
                        for binding in proper_list(&items[0])? {
                            let pair = proper_list(&binding)?;
                            if pair.len() != 2 {
                                return Err(RuntimeError::new("let binding expected name and value"));
                            }
                            let Some(name) = pair[0].symbol_name() else {
                                return Err(RuntimeError::new("let binding name must be a symbol"));
                            };
                            let value = self.eval(pair[1].clone(), env.clone())?;
                            new_env.define(name.to_string(), value);
                        }
                        env = new_env;
                        let mut body = items[1..].to_vec();
                        while body.len() > 1 {
                            self.eval(body.remove(0), env.clone())?;
                        }
                        expr = body.remove(0);
                        continue;
                    }
                    _ => {}
                }
            }

            let function = self.eval(operator, env.clone())?;
            let args = self.eval_operands(&operands, env.clone())?;
            if let Some((body, call_env)) = self.prepare_user_call(&function, args.clone())? {
                env = call_env;
                let mut body = body;
                if body.is_empty() {
                    return Ok(Value::Unspecified);
                }
                while body.len() > 1 {
                    self.eval(body.remove(0), env.clone())?;
                }
                expr = body.remove(0);
                continue;
            }
            return self.apply_non_user(function, args);
        }
    }

    fn eval_define(&mut self, operands: &Value, env: EnvRef) -> Result<(), RuntimeError> {
        let items = proper_list(operands)?;
        if items.len() < 2 {
            return Err(RuntimeError::new("define expected name and value"));
        }

        if let Some(name) = items[0].symbol_name() {
            let value = self.eval(items[1].clone(), env.clone())?;
            env.define(name.to_string(), value);
            return Ok(());
        }

        let Some(ObjectKind::Pair(signature)) = items[0].object_kind() else {
            return Err(RuntimeError::new("define expected symbol or function signature"));
        };
        let Some(name) = signature.car.symbol_name() else {
            return Err(RuntimeError::new("define function name must be a symbol"));
        };
        let params = parse_params(signature.cdr)?;
        let function = Function {
            params,
            body: items[1..].to_vec(),
            env: env.clone(),
            name: Some(name.to_string()),
        };
        env.define(name.to_string(), self.object(ObjectKind::Function(function)));
        Ok(())
    }

    fn eval_lambda(&mut self, operands: &Value, env: EnvRef) -> Result<Value, RuntimeError> {
        let items = proper_list(operands)?;
        if items.len() < 2 {
            return Err(RuntimeError::new("lambda expected parameters and body"));
        }
        let params = parse_params(items[0].clone())?;
        Ok(self.object(ObjectKind::Function(Function {
            params,
            body: items[1..].to_vec(),
            env,
            name: None,
        })))
    }

    fn eval_operands(&mut self, operands: &Value, env: EnvRef) -> Result<Vec<Value>, RuntimeError> {
        proper_list(operands)?
            .into_iter()
            .map(|arg| self.eval(arg, env.clone()))
            .collect()
    }

    fn prepare_user_call(
        &mut self,
        function: &Value,
        args: Vec<Value>,
    ) -> Result<Option<(Vec<Value>, EnvRef)>, RuntimeError> {
        let Some(ObjectKind::Function(function)) = function.object_kind() else {
            return Ok(None);
        };
        let env = Env::new(Some(function.env.clone()));
        bind_params(&function.params, args, env.clone(), self)?;
        Ok(Some((function.body, env)))
    }

    fn apply(&mut self, function: Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Some((body, env)) = self.prepare_user_call(&function, args.clone())? {
            return self.eval_sequence_tail(body, env);
        }
        self.apply_non_user(function, args)
    }

    fn apply_non_user(&mut self, function: Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match function {
            Value::Builtin(builtin) => self.apply_builtin(builtin, args),
            other => Err(RuntimeError::new(format!(
                "expected procedure, got {}",
                other.type_name()
            ))),
        }
    }

    fn apply_builtin(&mut self, builtin: Builtin, args: Vec<Value>) -> Result<Value, RuntimeError> {
        use Builtin::*;
        match builtin {
            Add => self.fold_number(args, 0.0, 0, |a, b| a + b, |a, b| a + b),
            Mul => self.fold_number(args, 1.0, 1, |a, b| a * b, |a, b| a * b),
            Sub => self.sub_numbers(args),
            Div => self.div_numbers(args),
            NumEq => compare_chain(args, |a, b| a == b),
            Lt => compare_chain(args, |a, b| a < b),
            Gt => compare_chain(args, |a, b| a > b),
            Lte => compare_chain(args, |a, b| a <= b),
            Gte => compare_chain(args, |a, b| a >= b),
            FxAdd => Ok(Value::Fixnum(
                expect_fixnum(args.get(0), "fx+")? + expect_fixnum(args.get(1), "fx+")?,
            )),
            FxSub => {
                if args.len() == 1 {
                    Ok(Value::Fixnum(-expect_fixnum(args.get(0), "fx-")?))
                } else {
                    Ok(Value::Fixnum(
                        expect_fixnum(args.get(0), "fx-")?
                            - expect_fixnum(args.get(1), "fx-")?,
                    ))
                }
            }
            FxLt => Ok(Value::Bool(
                expect_fixnum(args.get(0), "fx<")? < expect_fixnum(args.get(1), "fx<")?,
            )),
            FxEq => Ok(Value::Bool(
                expect_fixnum(args.get(0), "fx=")? == expect_fixnum(args.get(1), "fx=")?,
            )),
            Floor => {
                expect_arity(&args, 1, "floor")?;
                Ok(Value::Flonum(number_to_f64(&args[0], "floor")?.floor()))
            }
            Cons => {
                expect_arity(&args, 2, "cons")?;
                Ok(self.pair(args[0].clone(), args[1].clone()))
            }
            Car => {
                expect_arity(&args, 1, "car")?;
                let Some(ObjectKind::Pair(pair)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("car expected pair"));
                };
                Ok(pair.car)
            }
            Cdr => {
                expect_arity(&args, 1, "cdr")?;
                let Some(ObjectKind::Pair(pair)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("cdr expected pair"));
                };
                Ok(pair.cdr)
            }
            SetCar => {
                expect_arity(&args, 2, "set-car!")?;
                mutate_pair(&args[0], |pair| pair.car = args[1].clone())?;
                Ok(Value::Unspecified)
            }
            SetCdr => {
                expect_arity(&args, 2, "set-cdr!")?;
                mutate_pair(&args[0], |pair| pair.cdr = args[1].clone())?;
                Ok(Value::Unspecified)
            }
            List => Ok(self.list(args)),
            Eq => {
                expect_arity(&args, 2, "eq?")?;
                Ok(Value::Bool(value_eq(&args[0], &args[1])))
            }
            Eqv => {
                expect_arity(&args, 2, "eqv?")?;
                Ok(Value::Bool(value_eqv(&args[0], &args[1])))
            }
            Equal => {
                expect_arity(&args, 2, "equal?")?;
                Ok(Value::Bool(value_equal(&args[0], &args[1])))
            }
            Print => {
                let line = args.iter().map(display_value).collect::<Vec<_>>().join(" ");
                println!("{line}");
                Ok(Value::Unspecified)
            }
            PrettyPrint => {
                let line = args.iter().map(pretty_value).collect::<Vec<_>>().join(" ");
                println!("{line}");
                Ok(Value::Unspecified)
            }
            MakeTable => Ok(self.table()),
            TableRef => {
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new("table-ref expected two or three arguments"));
                }
                let Some(ObjectKind::Table(entries)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("table-ref expected table"));
                };
                for (key, value) in entries {
                    if value_equal(&key, &args[1]) {
                        return Ok(value);
                    }
                }
                Ok(args.get(2).cloned().unwrap_or(Value::Bool(false)))
            }
            TableSet => {
                expect_arity(&args, 3, "table-set!")?;
                self.table_set_value(&args[0], args[1].clone(), args[2].clone())
                    .map_err(RuntimeError::new)
            }
            TableForEach => {
                expect_arity(&args, 2, "table-for-each")?;
                let Some(ObjectKind::Table(entries)) = args[1].object_kind() else {
                    return Err(RuntimeError::new("table-for-each expected table"));
                };
                for (key, value) in entries {
                    self.apply(args[0].clone(), vec![key, value])?;
                }
                Ok(Value::Unspecified)
            }
            OpenInputFile => {
                expect_arity(&args, 1, "open-input-file")?;
                let path = expect_string(&args[0], "open-input-file")?;
                let source = fs::read_to_string(&path)
                    .map_err(|err| RuntimeError::new(format!("open-input-file: {err}")))?;
                let mut reader = Reader::new(&source);
                let mut forms = Vec::new();
                loop {
                    let value = reader.read(self).map_err(RuntimeError::new)?;
                    if value.is_eof() {
                        break;
                    }
                    forms.push(value);
                }
                Ok(self.object(ObjectKind::InputPort(InputPort { forms, index: 0 })))
            }
            CloseInputPort | CloseOutputPort | GcCollect | SetRecordTypePrinter | SetRecordTypeApply => {
                Ok(Value::Unspecified)
            }
            OpenOutputString => Ok(self.object(ObjectKind::OutputStringPort(String::new()))),
            GetOutputString => {
                expect_arity(&args, 1, "get-output-string")?;
                let Some(ObjectKind::OutputStringPort(s)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("get-output-string expected output string port"));
                };
                Ok(self.string(s))
            }
            Read => {
                if args.is_empty() {
                    return Err(RuntimeError::new("read without a port is not implemented"));
                }
                let Value::Object(obj) = &args[0] else {
                    return Err(RuntimeError::new("read expected input port"));
                };
                let mut obj = obj.borrow_mut();
                let ObjectKind::InputPort(port) = &mut obj.kind else {
                    return Err(RuntimeError::new("read expected input port"));
                };
                if port.index >= port.forms.len() {
                    Ok(Value::Eof)
                } else {
                    let value = port.forms[port.index].clone();
                    port.index += 1;
                    Ok(value)
                }
            }
            Write => {
                if args.is_empty() || args.len() > 2 {
                    return Err(RuntimeError::new("write expected one or two arguments"));
                }
                self.write_to_port(&args, true)
            }
            Display => {
                if args.is_empty() || args.len() > 2 {
                    return Err(RuntimeError::new("display expected one or two arguments"));
                }
                self.write_to_port(&args, false)
            }
            Newline => {
                let text = "\n".to_string();
                if let Some(port) = args.get(0) {
                    append_to_output_port(port, &text)?;
                } else {
                    print!("{text}");
                }
                Ok(Value::Unspecified)
            }
            StringCopy => {
                expect_arity(&args, 1, "string-copy")?;
                Ok(self.string(expect_string(&args[0], "string-copy")?))
            }
            ValueBits => {
                expect_arity(&args, 1, "value-bits")?;
                Ok(Value::Fixnum(value_bits(&args[0])))
            }
            Try => {
                expect_arity(&args, 2, "try")?;
                match self.apply(args[0].clone(), Vec::new()) {
                    Ok(value) => Ok(value),
                    Err(err) => {
                        let message = err.message;
                        let exc = err.exception.unwrap_or_else(|| self.object(ObjectKind::Exception(ExceptionValue {
                            tag: Value::Symbol("rust-error".to_string()),
                            message: message.clone(),
                            irritants: Value::Unspecified,
                        })));
                        let handled = self.apply(args[1].clone(), vec![exc.clone()])?;
                        if matches!(handled, Value::Bool(false)) {
                            Err(RuntimeError::raised(message, exc))
                        } else {
                            Ok(handled)
                        }
                    }
                }
            }
            Raise => {
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new("raise expected two or three arguments"));
                }
                let message = expect_string(&args[1], "raise").unwrap_or_else(|_| write_value(&args[1]));
                let exception = self.object(ObjectKind::Exception(ExceptionValue {
                    tag: args[0].clone(),
                    message: message.clone(),
                    irritants: args.get(2).cloned().unwrap_or(Value::Unspecified),
                }));
                Err(RuntimeError::raised(
                    format!("{}: {message}", write_value(&args[0])),
                    exception,
                ))
            }
            ExceptionTag => {
                expect_arity(&args, 1, "exception-tag")?;
                let Some(ObjectKind::Exception(exc)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("exception-tag expected exception"));
                };
                Ok(exc.tag)
            }
            ExceptionMessage => {
                expect_arity(&args, 1, "exception-message")?;
                let Some(ObjectKind::Exception(exc)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("exception-message expected exception"));
                };
                Ok(self.string(exc.message))
            }
            ExceptionIrritants => {
                expect_arity(&args, 1, "exception-irritants")?;
                let Some(ObjectKind::Exception(exc)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("exception-irritants expected exception"));
                };
                Ok(exc.irritants)
            }
            UnwindProtect => {
                expect_arity(&args, 2, "unwind-protect")?;
                let result = self.apply(args[0].clone(), Vec::new());
                let cleanup = self.apply(args[1].clone(), Vec::new());
                cleanup?;
                result
            }
            RegisterRecordType => {
                expect_arity(&args, 5, "register-record-type")?;
                let name = expect_string(&args[0], "register-record-type")?;
                let own = expect_fixnum(args.get(1), "register-record-type")? as usize;
                let parent = if matches!(args[4], Value::Bool(false)) {
                    None
                } else {
                    Some(args[4].clone())
                };
                let parent_fields = parent
                    .as_ref()
                    .and_then(|parent| match parent.object_kind() {
                        Some(ObjectKind::RecordType(rt)) => Some(rt.total_field_count),
                        _ => None,
                    })
                    .unwrap_or(0);
                Ok(self.object(ObjectKind::RecordType(RecordType {
                    name,
                    own_field_count: own,
                    total_field_count: parent_fields + own,
                    parent,
                })))
            }
            MakeRecord => {
                expect_arity(&args, 1, "make-record")?;
                let Some(ObjectKind::RecordType(rt)) = args[0].object_kind() else {
                    return Err(RuntimeError::new("make-record expected record type"));
                };
                Ok(self.object(ObjectKind::Record(Record {
                    record_type: args[0].clone(),
                    fields: vec![Value::Unspecified; rt.total_field_count],
                })))
            }
            RecordSet => {
                expect_arity(&args, 4, "record-set!")?;
                let index = expect_fixnum(args.get(2), "record-set!")? as usize;
                let Value::Object(obj) = &args[1] else {
                    return Err(RuntimeError::new("record-set! expected record"));
                };
                let mut obj = obj.borrow_mut();
                let ObjectKind::Record(record) = &mut obj.kind else {
                    return Err(RuntimeError::new("record-set! expected record"));
                };
                let Some(field) = record.fields.get_mut(index) else {
                    return Err(RuntimeError::new("record-set!: field index out of range"));
                };
                *field = args[3].clone();
                Ok(Value::Unspecified)
            }
            RecordRef => {
                expect_arity(&args, 3, "record-ref")?;
                let index = expect_fixnum(args.get(2), "record-ref")? as usize;
                let Some(ObjectKind::Record(record)) = args[1].object_kind() else {
                    return Err(RuntimeError::new("record-ref expected record"));
                };
                record
                    .fields
                    .get(index)
                    .cloned()
                    .ok_or_else(|| RuntimeError::new("record-ref: field index out of range"))
            }
            RecordIsa => {
                expect_arity(&args, 2, "record-isa?")?;
                let Some(ObjectKind::Record(record)) = args[0].object_kind() else {
                    return Ok(Value::Bool(false));
                };
                Ok(Value::Bool(record_isa(&record.record_type, &args[1])))
            }
        }
    }

    fn write_to_port(&mut self, args: &[Value], write: bool) -> Result<Value, RuntimeError> {
        let text = if write {
            write_value(&args[0])
        } else {
            display_value(&args[0])
        };
        if let Some(port) = args.get(1) {
            append_to_output_port(port, &text)?;
        } else {
            print!("{text}");
        }
        Ok(Value::Unspecified)
    }

    fn fold_number(
        &self,
        args: Vec<Value>,
        float_identity: f64,
        int_identity: i64,
        float_op: fn(f64, f64) -> f64,
        int_op: fn(i64, i64) -> i64,
    ) -> Result<Value, RuntimeError> {
        if args.iter().any(matches_flonum) {
            let mut out = float_identity;
            for arg in args {
                out = float_op(out, number_to_f64(&arg, "number operation")?);
            }
            Ok(Value::Flonum(out))
        } else {
            let mut out = int_identity;
            for arg in args {
                out = int_op(out, expect_fixnum(Some(&arg), "number operation")?);
            }
            Ok(Value::Fixnum(out))
        }
    }

    fn sub_numbers(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new("- expected at least one argument"));
        }
        if args.iter().any(matches_flonum) {
            let mut out = number_to_f64(&args[0], "-")?;
            if args.len() == 1 {
                return Ok(Value::Flonum(-out));
            }
            for arg in &args[1..] {
                out -= number_to_f64(arg, "-")?;
            }
            Ok(Value::Flonum(out))
        } else {
            let mut out = expect_fixnum(args.get(0), "-")?;
            if args.len() == 1 {
                return Ok(Value::Fixnum(-out));
            }
            for arg in &args[1..] {
                out -= expect_fixnum(Some(arg), "-")?;
            }
            Ok(Value::Fixnum(out))
        }
    }

    fn div_numbers(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity(&args, 2, "/")?;
        match (&args[0], &args[1]) {
            (Value::Fixnum(a), Value::Fixnum(b)) if *b != 0 && a % b == 0 => Ok(Value::Fixnum(a / b)),
            _ => Ok(Value::Flonum(
                number_to_f64(&args[0], "/")? / number_to_f64(&args[1], "/")?,
            )),
        }
    }
}

fn parse_params(value: Value) -> Result<Params, RuntimeError> {
    if let Value::Symbol(name) = value {
        return Ok(Params::Variadic(name));
    }

    let mut fixed = Vec::new();
    let mut cursor = value;
    loop {
        match cursor {
            Value::Nil => return Ok(Params::Fixed(fixed)),
            Value::Symbol(rest) => return Ok(Params::Dotted(fixed, rest)),
            Value::Object(obj) => match obj.borrow().kind.clone() {
                ObjectKind::Pair(pair) => {
                    let Some(name) = pair.car.symbol_name() else {
                        return Err(RuntimeError::new("lambda parameter must be a symbol"));
                    };
                    fixed.push(name.to_string());
                    cursor = pair.cdr;
                }
                _ => return Err(RuntimeError::new("invalid lambda parameter list")),
            },
            _ => return Err(RuntimeError::new("invalid lambda parameter list")),
        }
    }
}

fn bind_params(
    params: &Params,
    args: Vec<Value>,
    env: EnvRef,
    rt: &mut Runtime,
) -> Result<(), RuntimeError> {
    match params {
        Params::Fixed(names) => {
            if names.len() != args.len() {
                return Err(RuntimeError::new(format!(
                    "function expected {} arguments but got {}",
                    names.len(),
                    args.len()
                )));
            }
            for (name, value) in names.iter().zip(args) {
                env.define(name.clone(), value);
            }
        }
        Params::Variadic(name) => {
            env.define(name.clone(), rt.list(args));
        }
        Params::Dotted(names, rest) => {
            if args.len() < names.len() {
                return Err(RuntimeError::new(format!(
                    "function expected at least {} arguments but got {}",
                    names.len(),
                    args.len()
                )));
            }
            for (name, value) in names.iter().zip(args.iter().cloned()) {
                env.define(name.clone(), value);
            }
            env.define(rest.clone(), rt.list(args[names.len()..].to_vec()));
        }
    }
    Ok(())
}

fn list_head_tail(value: &Value) -> Result<(Value, Value), RuntimeError> {
    let Some(ObjectKind::Pair(pair)) = value.object_kind() else {
        return Err(RuntimeError::new("expected list"));
    };
    Ok((pair.car, pair.cdr))
}

fn proper_list(value: &Value) -> Result<Vec<Value>, RuntimeError> {
    let mut out = Vec::new();
    let mut cursor = value.clone();
    loop {
        match cursor {
            Value::Nil => return Ok(out),
            Value::Object(obj) => match obj.borrow().kind.clone() {
                ObjectKind::Pair(pair) => {
                    out.push(pair.car);
                    cursor = pair.cdr;
                }
                _ => return Err(RuntimeError::new("expected proper list")),
            },
            _ => return Err(RuntimeError::new("expected proper list")),
        }
    }
}

fn one_operand(value: &Value, name: &str) -> Result<Value, RuntimeError> {
    let items = proper_list(value)?;
    if items.len() != 1 {
        return Err(RuntimeError::new(format!("{name} expected one operand")));
    }
    Ok(items[0].clone())
}

fn expect_arity(args: &[Value], arity: usize, name: &str) -> Result<(), RuntimeError> {
    if args.len() == arity {
        Ok(())
    } else {
        Err(RuntimeError::new(format!(
            "{name} expected {arity} arguments but got {}",
            args.len()
        )))
    }
}

fn expect_fixnum(value: Option<&Value>, name: &str) -> Result<i64, RuntimeError> {
    match value {
        Some(Value::Fixnum(i)) => Ok(*i),
        _ => Err(RuntimeError::new(format!("{name} expected fixnum"))),
    }
}

fn expect_string(value: &Value, name: &str) -> Result<String, RuntimeError> {
    match value.object_kind() {
        Some(ObjectKind::String(s)) => Ok(s),
        _ => Err(RuntimeError::new(format!("{name} expected string"))),
    }
}

fn number_to_f64(value: &Value, name: &str) -> Result<f64, RuntimeError> {
    match value {
        Value::Fixnum(i) => Ok(*i as f64),
        Value::Flonum(f) => Ok(*f),
        _ => Err(RuntimeError::new(format!("{name} expected number"))),
    }
}

fn matches_flonum(value: &Value) -> bool {
    matches!(value, Value::Flonum(_))
}

fn compare_chain(args: Vec<Value>, cmp: fn(f64, f64) -> bool) -> Result<Value, RuntimeError> {
    if args.len() < 2 {
        return Ok(Value::Bool(true));
    }
    for pair in args.windows(2) {
        if !cmp(number_to_f64(&pair[0], "comparison")?, number_to_f64(&pair[1], "comparison")?) {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

fn mutate_pair(pair_value: &Value, f: impl FnOnce(&mut Pair)) -> Result<(), RuntimeError> {
    let Value::Object(obj) = pair_value else {
        return Err(RuntimeError::new("expected pair"));
    };
    let mut obj = obj.borrow_mut();
    let ObjectKind::Pair(pair) = &mut obj.kind else {
        return Err(RuntimeError::new("expected pair"));
    };
    f(pair);
    Ok(())
}

fn append_to_output_port(port: &Value, text: &str) -> Result<(), RuntimeError> {
    let Value::Object(obj) = port else {
        return Err(RuntimeError::new("expected output port"));
    };
    let mut obj = obj.borrow_mut();
    let ObjectKind::OutputStringPort(buffer) = &mut obj.kind else {
        return Err(RuntimeError::new("expected output string port"));
    };
    buffer.push_str(text);
    Ok(())
}

fn value_bits(value: &Value) -> i64 {
    match value {
        Value::Bool(false) => 0,
        Value::Nil => 10,
        Value::Eof => 18,
        Value::Unspecified => 26,
        Value::Undefined => 34,
        Value::Bool(true) => 42,
        _ => value.object_id().map(|id| (id as i64) << 3).unwrap_or(0),
    }
}

fn value_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Nil, Value::Nil)
        | (Value::Eof, Value::Eof)
        | (Value::Unspecified, Value::Unspecified)
        | (Value::Undefined, Value::Undefined)
        | (Value::OptionalObject, Value::OptionalObject)
        | (Value::KeyObject, Value::KeyObject)
        | (Value::KeysObject, Value::KeysObject)
        | (Value::RestObject, Value::RestObject) => true,
        (Value::Fixnum(a), Value::Fixnum(b)) => a == b,
        (Value::Char(a), Value::Char(b)) => a == b,
        (Value::Symbol(a), Value::Symbol(b)) => a == b,
        (Value::Builtin(a), Value::Builtin(b)) => a == b,
        (Value::Object(a), Value::Object(b)) => Rc::ptr_eq(a, b),
        _ => false,
    }
}

fn value_eqv(a: &Value, b: &Value) -> bool {
    value_eq(a, b)
        || matches!((a, b), (Value::Flonum(a), Value::Flonum(b)) if a == b)
}

pub fn value_equal(a: &Value, b: &Value) -> bool {
    if value_eqv(a, b) {
        return true;
    }

    match (a.object_kind(), b.object_kind()) {
        (Some(ObjectKind::String(a)), Some(ObjectKind::String(b))) => a == b,
        (Some(ObjectKind::Pair(a)), Some(ObjectKind::Pair(b))) => {
            value_equal(&a.car, &b.car) && value_equal(&a.cdr, &b.cdr)
        }
        (Some(ObjectKind::Vector(a)), Some(ObjectKind::Vector(b))) => {
            a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| value_equal(a, b))
        }
        _ => false,
    }
}

fn record_isa(record_type: &Value, expected: &Value) -> bool {
    if value_eq(record_type, expected) {
        return true;
    }
    let Some(ObjectKind::RecordType(rt)) = record_type.object_kind() else {
        return false;
    };
    match rt.parent {
        Some(parent) => record_isa(&parent, expected),
        None => false,
    }
}
