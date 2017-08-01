use enum_primitive::FromPrimitive;
use std::iter;
use syntax::symbol::Symbol;

#[derive(Debug)]
pub enum Item {
    LibraryName {
        metadata: Metadata,
        path: Vec<Symbol>,
    },
    Import(Metadata, Import),
    Export(Metadata, StringLiteral, Vec<ImportFilter>),
    Part {
        metadata: Metadata,
        uri: StringLiteral,
    },
    PartOf {
        metadata: Metadata,
        path: Vec<Symbol>,
    },
    Class {
        metadata: Metadata,
        abstract_: bool,
        name: Symbol,
        generics: Vec<TypeParameter>,
        superclass: Option<Box<Type>>,
        mixins: Vec<Box<Type>>,
        interfaces: Vec<Box<Type>>,
        members: Vec<ClassMember>,
    },
    MixinClass {
        metadata: Metadata,
        abstract_: bool,
        name: Symbol,
        generics: Vec<TypeParameter>,
        mixins: Vec<Box<Type>>,
        interfaces: Vec<Box<Type>>,
    },
    Enum {
        metadata: Metadata,
        name: Symbol,
        values: Vec<Symbol>,
    },
    TypeAlias {
        metadata: Metadata,
        name: Symbol,
        generics: Vec<TypeParameter>,
        ty: Box<Type>,
    },
    Function(Function),
    Global(VarType, Vec<NameAndInitializer>),
}

#[derive(Debug)]
pub struct ImportFilter {
    pub hide: bool,
    pub names: Vec<Symbol>,
}

#[derive(Debug)]
pub struct Import {
    pub uri: StringLiteral,
    pub deferred: bool,
    pub as_ident: Option<Symbol>,
    pub filters: Vec<ImportFilter>,
}

#[derive(Debug)]
pub struct Function {
    pub name: FnName,
    pub generics: Vec<TypeParameter>,
    pub sig: FnSig,
    pub body: Option<FnBody>,
}

#[derive(Debug)]
pub enum ClassMember {
    Redirect {
        metadata: Metadata,
        method_qualifiers: Vec<MethodQualifiers>,
        name: Option<Symbol>,
        sig: FnSig,
        ty: Box<Type>,
    },
    Constructor {
        metadata: Metadata,
        method_qualifiers: Vec<MethodQualifiers>,
        name: Option<Symbol>,
        sig: FnSig,
        initializers: Vec<ConstructorInitializer>,
        function_body: Option<FnBody>,
    },
    Method(Metadata, Vec<MethodQualifiers>, Function),
    Fields {
        metadata: Metadata,
        static_: bool,
        var_type: VarType,
        initializers: Vec<NameAndInitializer>,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum MethodQualifiers {
    External,
    Static,
    Const,
    Final,
    Factory,
}

#[derive(Copy, Clone, Debug)]
pub enum FnName {
    Regular(Symbol),
    Getter(Symbol),
    Setter(Symbol),
    Operator(OverloadedOp),
}

#[derive(Copy, Clone, Debug)]
pub enum OverloadedOp {
    BitNot,
    Index,
    IndexAssign,
    Bool(BoolBinOp),
    Value(ValueBinOp),
}

#[derive(Debug)]
pub enum ConstructorInitializer {
    Super(Option<Symbol>, Args),
    This(Option<Symbol>, Args),
    Assert(Args),
    Field(bool, Symbol, Box<Expr>),
}

#[derive(Debug)]
pub struct TypeParameter {
    pub metadata: Metadata,
    pub name: Symbol,
    pub extends: Option<Box<Type>>,
}

#[derive(Debug)]
pub struct Qualified {
    pub prefix: Option<Symbol>,
    pub name: Symbol,
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
    Not,
    BitNot,
    Await,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

enum_from_primitive! {
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BoolBinOp {
    Or,
    And,
    Eq,
    Ne,
    Ge,
    Gt,
    Le,
    Lt,
}
}

impl BoolBinOp {
    pub fn values() -> impl Iterator<Item = Self> {
        (0..)
            .map(Self::from_usize)
            .take_while(|x| x.is_some())
            .map(|x| x.unwrap())
    }
}

enum_from_primitive! {
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub enum ValueBinOp {
        IfNull,
        Add,
        Sub,
        Mul,
        Div,
        Mod,
        TruncDiv,
        Lsh,
        Rsh,
        BitAnd,
        BitXor,
        BitOr,
    }
}

impl ValueBinOp {
    pub fn values() -> impl Iterator<Item = Self> {
        (0..)
            .map(Self::from_usize)
            .take_while(|x| x.is_some())
            .map(|x| x.unwrap())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinOp {
    Bool(BoolBinOp),
    Value(ValueBinOp),
    Assign(Option<ValueBinOp>),
}

impl BinOp {
    pub fn values() -> impl Iterator<Item = Self> {
        (BoolBinOp::values().map(BinOp::Bool))
            .chain(ValueBinOp::values().map(BinOp::Value))
            .chain(ValueBinOp::values().map(Some).chain(iter::once(None)).map(
                BinOp::Assign,
            ))
    }
}

impl BinOp {
    pub fn as_str(self) -> &'static str {
        macro_rules! value_bin_op {
            ($op:expr, $suffix:expr) => (match $op {
                ValueBinOp::IfNull => concat!("??", $suffix),
                ValueBinOp::Add => concat!("+", $suffix),
                ValueBinOp::Sub => concat!("-", $suffix),
                ValueBinOp::Mul => concat!("*", $suffix),
                ValueBinOp::Div => concat!("/", $suffix),
                ValueBinOp::Mod => concat!("%", $suffix),
                ValueBinOp::TruncDiv => concat!("~/", $suffix),
                ValueBinOp::Lsh => concat!("<<", $suffix),
                ValueBinOp::Rsh => concat!(">>", $suffix),
                ValueBinOp::BitAnd => concat!("&", $suffix),
                ValueBinOp::BitXor => concat!("^", $suffix),
                ValueBinOp::BitOr => concat!("|", $suffix),
            })
        }
        match self {
            BinOp::Bool(BoolBinOp::Or) => "||",
            BinOp::Bool(BoolBinOp::And) => "&&",
            BinOp::Bool(BoolBinOp::Eq) => "==",
            BinOp::Bool(BoolBinOp::Ne) => "!=",
            BinOp::Bool(BoolBinOp::Ge) => ">=",
            BinOp::Bool(BoolBinOp::Gt) => ">",
            BinOp::Bool(BoolBinOp::Le) => "<=",
            BinOp::Bool(BoolBinOp::Lt) => "<",

            BinOp::Value(op) => value_bin_op!(op, ""),

            BinOp::Assign(Some(op)) => value_bin_op!(op, "="),
            BinOp::Assign(None) => "=",
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Path(Qualified, Vec<Box<Type>>),
    FunctionOld(FnSig),
    Function(FnSig),
    Infer,
}

#[derive(Debug)]
pub enum Expr {
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    Is(Box<Expr>, Box<Type>),
    IsNot(Box<Expr>, Box<Type>),
    As(Box<Expr>, Box<Type>),
    Suffix(Box<Expr>, Suffix),
    Identifier(Symbol),
    Closure(FnSig, FnBody),
    New {
        const_: bool,
        ty: Box<Type>,
        ctor: Option<Symbol>,
        args: Args,
    },
    List {
        const_: bool,
        element_ty: Option<Box<Type>>,
        elements: Vec<Box<Expr>>,
    },
    Map {
        const_: bool,
        kv_ty: Option<(Box<Type>, Box<Type>)>,
        kv: Vec<(Box<Expr>, Box<Expr>)>,
    },
    Number(Symbol),
    String(Vec<StringLiteral>),
    Symbol(SymbolLiteral),
    Paren(Box<Expr>),
    Throw(Box<Expr>),
    Cascade(Box<Expr>, Cascade),
}

#[derive(Debug)]
pub enum SymbolLiteral {
    Op(OverloadedOp),
    Path(Vec<Symbol>),
}

#[derive(Debug)]
pub struct StringLiteral {
    pub raw: bool,
    pub triple: bool,
    pub quote: char,
    pub prefix: Symbol,
    pub interpolated: Vec<(Box<Expr>, Symbol)>,
}

#[derive(Debug)]
pub enum Suffix {
    Index(Box<Expr>),
    Field(Symbol),
    FieldIfNotNull(Symbol),
    Call(Vec<Box<Type>>, Args),
}

#[derive(Debug)]
pub struct Cascade {
    pub suffixes: Vec<Suffix>,
    pub assign: Option<(Option<ValueBinOp>, Box<Expr>)>,
}

#[derive(Debug)]
pub struct NamedArg {
    pub name: Symbol,
    pub expr: Box<Expr>,
}

#[derive(Debug)]
pub struct Args {
    pub unnamed: Vec<Box<Expr>>,
    pub named: Vec<NamedArg>,
}

#[derive(Debug)]
pub struct MetadataItem {
    pub qualified: Qualified,
    pub suffix: Option<Symbol>,
    pub arguments: Option<Args>,
}

pub type Metadata = Vec<MetadataItem>;

#[derive(Debug)]
pub struct FnSig {
    pub return_type: Box<Type>,
    pub required: Vec<ArgDef>,
    pub optional: Vec<OptionalArgDef>,
    pub optional_kind: OptionalArgKind,
    pub async: bool,
    pub generator: bool,
}

impl Default for FnSig {
    fn default() -> Self {
        FnSig {
            return_type: Box::new(Type::Infer),
            required: vec![],
            optional: vec![],
            optional_kind: OptionalArgKind::default(),
            async: false,
            generator: false,
        }
    }
}

#[derive(Debug)]
pub enum FnBody {
    Arrow(Box<Expr>),
    Block(Box<Statement>),
    Native(Option<StringLiteral>),
}

#[derive(Debug)]
pub enum OptionalArgKind {
    Positional,
    Named,
}

impl Default for OptionalArgKind {
    fn default() -> Self {
        OptionalArgKind::Positional
    }
}

#[derive(Debug)]
pub struct ArgDef {
    pub metadata: Metadata,
    pub covariant: bool,
    pub ty: VarType,
    pub field: bool,
    pub name: Symbol,
}

#[derive(Debug)]
pub struct OptionalArgDef {
    pub arg: ArgDef,
    pub default: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct VarType {
    pub fcv: FinalConstVar,
    pub ty: Box<Type>,
}

#[derive(Debug)]
pub enum FinalConstVar {
    Final,
    Const,
    Var,
}

#[derive(Debug)]
pub struct NameAndInitializer {
    pub name: Symbol,
    pub init: Option<Box<Expr>>,
}

#[derive(Debug)]
pub enum ForLoop {
    CLike(Box<Statement>, Option<Box<Expr>>, Vec<Box<Expr>>),
    In(Option<VarType>, Symbol, Box<Expr>),
}

#[derive(Debug)]
pub struct SwitchCase {
    pub labels: Vec<Symbol>,
    pub value: Option<Box<Expr>>,
    pub statements: Vec<Box<Statement>>,
}

#[derive(Debug)]
pub struct CatchPart {
    pub exception: Symbol,
    pub trace: Option<Symbol>,
}

#[derive(Debug)]
pub struct TryPart {
    pub on: Option<Box<Type>>,
    pub catch: Option<CatchPart>,
    pub block: Box<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Box<Statement>>),
    Var(VarType, Vec<NameAndInitializer>),
    Function(Function),
    For(bool, ForLoop, Box<Statement>),
    While(Box<Expr>, Box<Statement>),
    DoWhile(Box<Statement>, Box<Expr>),
    Switch(Box<Expr>, Vec<SwitchCase>),
    If(Box<Expr>, Box<Statement>, Option<Box<Statement>>),
    Rethrow,
    Try(Box<Statement>, Vec<TryPart>),
    Break(Option<Symbol>),
    Continue(Option<Symbol>),
    Return(Option<Box<Expr>>),
    Yield(Box<Expr>),
    YieldEach(Box<Expr>),
    Expression(Option<Box<Expr>>),
    Assert(Args),
    Labelled(Symbol, Box<Statement>),
}
