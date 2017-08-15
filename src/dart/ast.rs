use enum_primitive::FromPrimitive;
use std::iter;
use syntax::symbol::Symbol;
use syntax::codemap::Span;
use node::Node;

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
        superclass: Option<Node<Type>>,
        mixins: Vec<Node<Type>>,
        interfaces: Vec<Node<Type>>,
        members: Vec<ClassMember>,
    },
    MixinClass {
        metadata: Metadata,
        abstract_: bool,
        name: Symbol,
        generics: Vec<TypeParameter>,
        mixins: Vec<Node<Type>>,
        interfaces: Vec<Node<Type>>,
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
        ty: Node<Type>,
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
        ty: Node<Type>,
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
    Field(bool, Symbol, Node<Expr>),
}

#[derive(Debug)]
pub struct TypeParameter {
    pub metadata: Metadata,
    pub name: Symbol,
    pub extends: Option<Node<Type>>,
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
            .chain(
                ValueBinOp::values()
                    .map(Some)
                    .chain(iter::once(None))
                    .map(BinOp::Assign),
            )
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
    Path(Qualified, Vec<Node<Type>>),
    FunctionOld(FnSig),
    Function(FnSig),
    Infer,
}

#[derive(Debug)]
pub enum Expr {
    Unary(UnOp, Node<Expr>),
    Binary(BinOp, Node<Expr>, Node<Expr>),
    Conditional(Node<Expr>, Node<Expr>, Node<Expr>),
    Is(Node<Expr>, Node<Type>),
    IsNot(Node<Expr>, Node<Type>),
    As(Node<Expr>, Node<Type>),
    Suffix(Node<Expr>, Suffix),
    Identifier(Symbol),
    Closure(FnSig, FnBody),
    New {
        const_: bool,
        ty: Node<Type>,
        ctor: Option<Symbol>,
        args: Args,
    },
    List {
        const_: bool,
        element_ty: Option<Node<Type>>,
        elements: Vec<Node<Expr>>,
    },
    Map {
        const_: bool,
        kv_ty: Option<(Node<Type>, Node<Type>)>,
        kv: Vec<(Node<Expr>, Node<Expr>)>,
    },
    Number(Symbol),
    String(Vec<StringLiteral>),
    Symbol(SymbolLiteral),
    Paren(Node<Expr>),
    Throw(Node<Expr>),
    Cascade(Node<Expr>, Cascade),
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
    pub prefix: Span,
    pub interpolated: Vec<(Node<Expr>, Span)>,
}

#[derive(Debug)]
pub enum Suffix {
    Index(Node<Expr>),
    Field(Symbol),
    FieldIfNotNull(Symbol),
    Call(Vec<Node<Type>>, Args),
}

#[derive(Debug)]
pub struct Cascade {
    pub suffixes: Vec<Suffix>,
    pub assign: Option<(Option<ValueBinOp>, Node<Expr>)>,
}

#[derive(Debug)]
pub struct NamedArg {
    pub name: Symbol,
    pub expr: Node<Expr>,
}

#[derive(Debug)]
pub struct Args {
    pub unnamed: Vec<Node<Expr>>,
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
    pub return_type: Node<Type>,
    pub required: Vec<ArgDef>,
    pub optional: Vec<OptionalArgDef>,
    pub optional_kind: OptionalArgKind,
    pub async: bool,
    pub generator: bool,
}

impl Default for FnSig {
    fn default() -> Self {
        FnSig {
            return_type: Node::new(Type::Infer),
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
    Arrow(Node<Expr>),
    Block(Node<Statement>),
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
    pub default: Option<Node<Expr>>,
}

#[derive(Debug)]
pub struct VarType {
    pub fcv: FinalConstVar,
    pub ty: Node<Type>,
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
    pub init: Option<Node<Expr>>,
}

#[derive(Debug)]
pub enum ForLoop {
    CLike(Node<Statement>, Option<Node<Expr>>, Vec<Node<Expr>>),
    In(Option<VarType>, Symbol, Node<Expr>),
}

#[derive(Debug)]
pub struct SwitchCase {
    pub labels: Vec<Symbol>,
    pub value: Option<Node<Expr>>,
    pub statements: Vec<Node<Statement>>,
}

#[derive(Debug)]
pub struct CatchPart {
    pub exception: Symbol,
    pub trace: Option<Symbol>,
}

#[derive(Debug)]
pub struct TryPart {
    pub on: Option<Node<Type>>,
    pub catch: Option<CatchPart>,
    pub block: Node<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Node<Statement>>),
    Var(VarType, Vec<NameAndInitializer>),
    Function(Function),
    For(bool, ForLoop, Node<Statement>),
    While(Node<Expr>, Node<Statement>),
    DoWhile(Node<Statement>, Node<Expr>),
    Switch(Node<Expr>, Vec<SwitchCase>),
    If(Node<Expr>, Node<Statement>, Option<Node<Statement>>),
    Rethrow,
    Try(Node<Statement>, Vec<TryPart>),
    Break(Option<Symbol>),
    Continue(Option<Symbol>),
    Return(Option<Node<Expr>>),
    Yield(Node<Expr>),
    YieldEach(Node<Expr>),
    Expression(Option<Node<Expr>>),
    Assert(Args),
    Labelled(Symbol, Node<Statement>),
}
