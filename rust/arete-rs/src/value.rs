use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::EnvRef;

pub type ObjRef = Rc<RefCell<Object>>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Builtin {
    Add,
    Sub,
    Mul,
    Div,
    NumEq,
    Lt,
    Gt,
    Lte,
    Gte,
    FxAdd,
    FxSub,
    FxLt,
    FxEq,
    Floor,
    Cons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
    List,
    Eq,
    Eqv,
    Equal,
    Print,
    PrettyPrint,
    MakeTable,
    TableRef,
    TableSet,
    TableForEach,
    OpenInputFile,
    CloseInputPort,
    OpenOutputString,
    CloseOutputPort,
    GetOutputString,
    Read,
    Write,
    Display,
    Newline,
    StringCopy,
    ValueBits,
    GcCollect,
    Try,
    Raise,
    ExceptionTag,
    ExceptionMessage,
    ExceptionIrritants,
    UnwindProtect,
    RegisterRecordType,
    MakeRecord,
    RecordSet,
    RecordRef,
    RecordIsa,
    SetRecordTypePrinter,
    SetRecordTypeApply,
}

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Eof,
    Unspecified,
    Undefined,
    OptionalObject,
    KeyObject,
    KeysObject,
    RestObject,
    Fixnum(i64),
    Flonum(f64),
    Char(char),
    Symbol(String),
    Builtin(Builtin),
    Object(ObjRef),
}

#[derive(Clone)]
pub struct Object {
    pub id: usize,
    pub kind: ObjectKind,
}

#[derive(Clone)]
pub enum ObjectKind {
    Pair(Pair),
    Vector(Vec<Value>),
    String(String),
    Table(Vec<(Value, Value)>),
    Function(Function),
    RecordType(RecordType),
    Record(Record),
    InputPort(InputPort),
    OutputStringPort(String),
    Exception(ExceptionValue),
}

#[derive(Clone)]
pub struct Pair {
    pub car: Value,
    pub cdr: Value,
}

#[derive(Clone)]
pub struct Function {
    pub params: Params,
    pub body: Vec<Value>,
    pub env: EnvRef,
    pub name: Option<String>,
}

#[derive(Clone)]
pub enum Params {
    Fixed(Vec<String>),
    Variadic(String),
    Dotted(Vec<String>, String),
}

#[derive(Clone)]
pub struct RecordType {
    pub name: String,
    pub own_field_count: usize,
    pub total_field_count: usize,
    pub parent: Option<Value>,
}

#[derive(Clone)]
pub struct Record {
    pub record_type: Value,
    pub fields: Vec<Value>,
}

#[derive(Clone)]
pub struct InputPort {
    pub forms: Vec<Value>,
    pub index: usize,
}

#[derive(Clone)]
pub struct ExceptionValue {
    pub tag: Value,
    pub message: String,
    pub irritants: Value,
}

impl Value {
    pub fn is_eof(&self) -> bool {
        matches!(self, Value::Eof)
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false))
    }

    pub fn symbol_name(&self) -> Option<&str> {
        match self {
            Value::Symbol(name) => Some(name.as_str()),
            _ => None,
        }
    }

    pub fn is_keyword_symbol(&self) -> bool {
        matches!(self, Value::Symbol(name) if name.ends_with(':'))
    }

    pub fn object_id(&self) -> Option<usize> {
        match self {
            Value::Object(obj) => Some(obj.borrow().id),
            _ => None,
        }
    }

    pub fn object_kind(&self) -> Option<ObjectKind> {
        match self {
            Value::Object(obj) => Some(obj.borrow().kind.clone()),
            _ => None,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(_) => "boolean",
            Value::Nil => "nil",
            Value::Eof => "eof-object",
            Value::Unspecified => "unspecified",
            Value::Undefined => "undefined",
            Value::OptionalObject | Value::KeyObject | Value::KeysObject | Value::RestObject => {
                "parameter-object"
            }
            Value::Fixnum(_) => "fixnum",
            Value::Flonum(_) => "flonum",
            Value::Char(_) => "character",
            Value::Symbol(_) => "symbol",
            Value::Builtin(_) => "cfunction",
            Value::Object(obj) => match &obj.borrow().kind {
                ObjectKind::Pair(_) => "pair",
                ObjectKind::Vector(_) => "vector",
                ObjectKind::String(_) => "string",
                ObjectKind::Table(_) => "table",
                ObjectKind::Function(_) => "function",
                ObjectKind::RecordType(_) => "record-type",
                ObjectKind::Record(_) => "record",
                ObjectKind::InputPort(_) => "file-port",
                ObjectKind::OutputStringPort(_) => "file-port",
                ObjectKind::Exception(_) => "exception",
            },
        }
    }
}
