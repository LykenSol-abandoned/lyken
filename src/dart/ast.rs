use dart::parse;
use dart::visit::{Visit, VisitNode, Visitor};
use enum_primitive::FromPrimitive;
use node::Node;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter;
use std::path::{Path, PathBuf};
use syntax::symbol::Symbol;
use Span;

node_field!(parent_any: Node<Any>);

impl<T: 'static> Node<T> {
    pub fn parent<U: 'static>(&self) -> Option<Node<U>> {
        if let Some(parent) = self.parent_any().get() {
            parent.downcast()
        } else {
            None
        }
    }
    pub fn root_module(&self) -> Option<Node<Module>> {
        let mut node: Node<Any> = self.clone();
        while let Some(parent) = node.parent_any().get() {
            node = parent;
        }
        node.downcast()
    }
}

/// A Dart source file.
#[derive(Debug)]
pub struct Module {
    /// The file path where the module is located.
    pub path: PathBuf,
    /// The contents of the module.
    pub items: Vec<Node<Item>>,
    /// Keeps track of whether any IO or parsing errors occurred
    /// while loading the module.
    pub has_error: bool,
}

impl Module {
    pub fn load(path: &Path) -> Node<Module> {
        thread_local!(static CACHE: RefCell<HashMap<PathBuf, Node<Module>>> =
        RefCell::new(HashMap::new()));

        let path_buf;
        let mut path = path;
        let module: parse::ParseResult<_> = do catch {
            path_buf = path.canonicalize()?;
            path = &path_buf;
            if let Some(module) = CACHE.with(|c| c.borrow().get(path).cloned()) {
                return module;
            }
            parse::Parser::with_file(path, |p| p.dart_module())?
        };
        let module = match module {
            Ok(module) => module,
            Err(error) => {
                if let parse::ErrorKind::Io(_) = *error.kind() {
                    println!("{}: {}", path.display(), error);
                } else {
                    println!("{}", error);
                }
                Node::new(Module {
                    path: path.to_path_buf(),
                    items: vec![],
                    has_error: true,
                })
            }
        };

        struct Parenter {
            parent: Node<Any>,
        }

        impl Visitor for Parenter {
            fn visit_node<T: VisitNode>(&mut self, node: Node<T>) {
                let parent = self.parent.clone();
                node.parent_any().set(parent.clone());
                self.parent = node.clone();
                node.super_visit(self);
                self.parent = parent;
            }
        }

        module.super_visit(&mut Parenter {
            parent: module.clone(),
        });

        CACHE.with(|c| {
            c.borrow_mut().insert(module.path.clone(), module.clone())
        });
        module
    }
}

/// A top-level item.
#[derive(Debug)]
pub enum Item {
    /// Provides the name of the library (`library name;`).
    LibraryName { meta: Meta, path: Vec<Symbol> },
    /// A directive that bring items from a library
    /// in the scope of the current library (`import 'library';`).
    Import(Meta, Import),
    /// A directive that makes the items of a library available to
    /// users of the current library (`export 'library';`).
    Export(Meta, StringLiteral, Vec<ImportFilter>),
    /// A directive that specifies where a file that should be incorporated
    /// into the current library is located (`part 'library';`).
    Part {
        meta: Meta,
        uri: StringLiteral,
        module: Node<Module>,
    },
    /// A directive that specifies that the current file is incorporated
    /// in a library (`part of 'library';`).
    PartOf { meta: Meta, path: Vec<Symbol> },
    /// A class defines the form and behaviour of a set of objects
    /// that are its instances (`class example { ... }`).
    Class {
        meta: Meta,
        abstract_: bool,
        name: Symbol,
        generics: Vec<Node<TypeParameter>>,
        superclass: Option<Node<Qualified>>,
        mixins: Vec<Node<Qualified>>,
        interfaces: Vec<Node<Qualified>>,
        members: Vec<Node<ClassMember>>,
    },
    /// A mixin class combines the members of multiple classes
    /// (`class A = B with C;`).
    MixinClass {
        meta: Meta,
        abstract_: bool,
        name: Symbol,
        generics: Vec<Node<TypeParameter>>,
        mixins: Vec<Node<Qualified>>,
        interfaces: Vec<Node<Qualified>>,
    },
    /// Enumerated types are a special kind of class used to represent
    /// a fixed set of constant values.
    ///
    /// (`enum Type { integer, boolean, string }`).
    Enum {
        meta: Meta,
        name: Symbol,
        values: Vec<(Meta, Symbol)>,
    },
    /// A type alias defines a new name for a given type
    /// (`typedef int example(Object a, Object b);`).
    TypeAlias {
        meta: Meta,
        name: Symbol,
        generics: Vec<Node<TypeParameter>>,
        ty: Node<Type>,
    },
    /// Function definition (`example(x) { return x }`).
    Function {
        meta: Meta,
        external: bool,
        function: Node<Function>,
    },
    /// Global variables (`var example = 123;`).
    Vars(Meta, VarType, Vec<Node<VarDef>>),
}

/// Specifies which items of the imported library to be shown/hidden.
#[derive(Clone, Debug)]
pub struct ImportFilter {
    /// Specifies the action to be applied to the items specified.
    pub hide: bool,
    /// Specifies the items of the library to be shown/hidden.
    pub names: Vec<Symbol>,
}

/// Specifies a library to be used in the scope of the current library.
///
/// `import 'dart:io';`
/// `import '../util.dart';`
#[derive(Clone, Debug)]
pub struct Import {
    /// Holds the URI where the declaration of the imported library
    /// is to be found.
    pub uri: StringLiteral,
    /// Distinguishes between deferred and immediate imports.
    pub deferred: bool,
    /// Specifies a prefix for the library.
    pub alias: Option<Symbol>,
    /// Specify which part of the library to be imported or ignored
    /// through "hide" and "show".
    pub filters: Vec<ImportFilter>,
}

/// Describes actions to be carried out.
#[derive(Debug)]
pub struct Function {
    /// The identifier of the function.
    pub name: FnName,
    /// The parameters that are used by the function.
    pub generics: Vec<Node<TypeParameter>>,
    /// The signature of the function.
    pub sig: FnSig,
    /// The body to be executed upon calling the function.
    pub body: Option<FnBody>,
}

/// Member of a class definition.
#[derive(Debug)]
pub enum ClassMember {
    /// A clause that invokes another generative constructor.
    ///
    /// `class Animal { factory Animal.cat() = Cat; }`
    Redirect {
        meta: Meta,
        method_qualifiers: Vec<MethodQualifiers>,
        name: Option<Symbol>,
        sig: FnSig,
        path: Node<Qualified>,
    },
    /// A special function that used in instance creation expression
    /// to produce objects.
    ///
    /// `class Language { Language(params) { ... } }`
    Constructor {
        meta: Meta,
        method_qualifiers: Vec<MethodQualifiers>,
        name: Option<Symbol>,
        sig: FnSig,
        initializers: Vec<ConstructorInitializer>,
        function_body: Option<FnBody>,
    },
    /// A function that applies only to the object and provides behaviour.
    ///
    /// `printObject() { print(object_field) }`
    Method(Meta, Vec<MethodQualifiers>, Node<Function>),
    /// Variables that describe the state of the class instances.
    ///
    /// `int counter = 0;`
    Fields {
        meta: Meta,
        static_: bool,
        var_type: VarType,
        initializers: Vec<Node<VarDef>>,
    },
}

/// Method qualifiers.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MethodQualifiers {
    /// A function whose body is provided separately from
    /// its declaration.
    ///
    /// `static method(x);`
    External,
    /// A function whose declaration is immediately contained
    /// within class declaration, does not operate on an instance
    /// and can be used as compile-time constant.
    ///
    /// `external method(x);`
    Static,
    /// A function that defines behaviour that never changes.
    ///
    /// `const method(x);`
    Const,
    /// A function that does not allow overridding.
    ///
    /// `final method(x);`
    Final,
    /// A constructor that can return instances of different classes.
    ///
    /// `factory ClassName() {...}`
    Factory,
}

/// The function name intuitively describes its purpose.
#[derive(Copy, Clone, Debug)]
pub enum FnName {
    /// A function that implements any type of behaviour.
    ///
    /// `add(a, b) { return a + b }`
    Regular(Symbol),
    ///  A function that provides reading privileges to the private fields.
    ///
    /// ` get field => field;`
    Getter(Symbol),
    /// A function that provides writing privileges to the private fields.
    ///
    /// `set field(new_value) => field = new_value;`
    Setter(Symbol),
    /// A function that overloads an operator (`operator +() { ... }`).
    Operator(OverloadedOp),
}

impl FnName {
    pub fn regular<S: ::IntoSymbol>(name: S) -> Self {
        FnName::Regular(name.into_symbol())
    }
}

/// Overloaded operators.
#[derive(Copy, Clone, Debug)]
pub enum OverloadedOp {
    /// Bitwise negation. (`operator~`)
    BitNot,
    /// Indexing. (`operator[]`)
    Index,
    /// Indexed assignment. (`operator[]=`)
    IndexAssign,
    /// Boolean binary operator.
    Bool(BoolBinOp),
    /// Value binary operator.
    Value(ValueBinOp),
}

/// An element of constructor initializers list.
#[derive(Debug)]
pub enum ConstructorInitializer {
    /// Redirects to the superclass' constructor.
    Super(Option<Symbol>, Args),
    /// Redirects to the current class' constructor.
    This(Option<Symbol>, Args),
    /// Precondition to constructing the object.
    Assert(Args),
    /// Initializes a specific field (`Person():name('Name') {...}`).
    Field(bool, Symbol, Node<Expr>),
}

/// Type parameter definition.
#[derive(Debug)]
pub struct TypeParameter {
    /// Metadata of the parameter.
    pub meta: Meta,
    /// The name of the parameter.
    pub name: Symbol,
    /// Limits the type of the parameter (`<T extends V>`).
    pub extends: Option<Node<Qualified>>,
}

/// Optionally fully-qualified path.
#[derive(Debug)]
pub struct Qualified {
    /// The object to look up the name in.
    pub prefix: Option<Node<Qualified>>,
    /// The name of the object.
    pub name: Symbol,
    /// The type parameters of the object (`List<int>`, `Map<K,V>`, etc.).
    pub params: Vec<Node<Type>>,
}

impl Qualified {
    pub fn one<S: ::IntoSymbol>(name: S, params: Vec<Node<Type>>) -> Node<Qualified> {
        Node::new(Qualified {
            prefix: None,
            name: name.into_symbol(),
            params,
        })
    }
}

/// Unary operators.
#[derive(Copy, Clone, Debug)]
pub enum UnOp {
    /// Negation (`-x`).
    Neg,
    /// Logical Not (`!x`).
    Not,
    /// Bitwise negation (`~`).
    BitNot,
    /// Allows code to yield control until an asynchronous operation completes (`await`).
    Await,
    /// Pre-increment (`++x`).
    PreInc,
    /// Pre-decrement (`--x`).
    PreDec,
    /// Post-increment (`x++`).
    PostInc,
    /// Post-decrement (`x--`).
    PostDec,
}

enum_from_primitive! {
/// Boolean binary operators.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BoolBinOp {
    /// Logical OR (`a || b`).
    Or,
    /// Logical AND (`a && b`).
    And,
    /// Equals (`a == b`).
    Eq,
    /// Not equals (`a != b`).
    Ne,
    /// Greater or equal (`a >= b`).
    Ge,
    /// Greater than (`a > b`).
    Gt,
    /// Lower or equal (`a <= b`).
    Le,
    /// Lower than (`a < b`).
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
/// Value binary operators.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum ValueBinOp {
    /// Conditional expression (`a ?? b`).
    /// If `a` is non-null, returns its value;
    /// otherwise, evaluates and returns the value of `b`.
    IfNull,
    /// Addition (`a + b`).
    Add,
    /// Subtraction (`a - b`).
    Sub,
    /// Multiplication (`a * b`).
    Mul,
    /// Division (`a / b`).
    Div,
    /// Modulo (`a % b`).
    Mod,
    /// Division returning an integer result (`a ~/ b`).
    TruncDiv,
    /// Bitwise left shift (`a << 4`).
    Lsh,
    /// Bitwise right shift (`a >> 4`).
    Rsh,
    /// Bitwise AND (`a & b`).
    BitAnd,
    /// Bitwise XOR (`a ^ b`).
    BitXor,
    /// Bitwise OR (`a | b`).
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

/// Binary operators.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinOp {
    /// Boolean binary operator.
    Bool(BoolBinOp),
    /// Value binary operator.
    Value(ValueBinOp),
    /// Plain assignment or compound assignment should a value binary
    /// operator be involved.
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

/// Type annotations used in variable declarations, in return types
/// of functions and in bounds of type variables.
#[derive(Debug)]
pub enum Type {
    /// Named type definition, optionally parametrized.
    Path(Node<Qualified>),
    /// Old-style functon type (`R f(A, B, C)` for a variable `f`).
    FunctionOld(FnSig),
    /// Function type (`R Function(A, B, C)`).
    Function(FnSig),
    /// Unspecified type.
    Infer,
}

impl Type {
    pub fn simple_path<S: ::IntoSymbol>(name: S) -> Node<Type> {
        Node::new(Type::Path(Qualified::one(name, vec![])))
    }
}

/// An expression.
#[derive(Debug)]
pub enum Expr {
    /// Comments that describe the expression.
    Comments(Vec<Span>, Node<Expr>),
    ///A unary operation (`!x`, `~x`, etc.).
    Unary(UnOp, Node<Expr>),
    /// A binary operation (`a + b`, `a * b`, etc.).
    Binary(BinOp, Node<Expr>, Node<Expr>),
    /// A conditional expression (`a ? b : c`).
    Conditional(Node<Expr>, Node<Expr>, Node<Expr>),
    /// An expression that checks the type (`var is type`).
    Is(Node<Expr>, Node<Type>),
    /// A negated expression that checks the type (`var !is type`).
    IsNot(Node<Expr>, Node<Type>),
    /// An expression that acts as a cast (`x as Type`).
    As(Node<Expr>, Node<Type>),
    /// A sequence of operator, method, getter or setter invocations.
    ///
    /// `expr.field`
    /// `expr[idx]`
    Suffix(Node<Expr>, Suffix),
    /// An identifier (`x`).
    Identifier(Symbol),
    /// A function object that has access to variables in its lexical scope,
    /// even when the function is used outside of its original scope.
    ///
    /// `(a, b) => a + b`
    Closure(FnSig, FnBody),
    /// A constructor call.
    ///
    /// `new Constructor()`
    New {
        const_: bool,
        path: Node<Qualified>,
        args: Args,
    },
    /// `List<T>`.
    ///
    /// `var list = [1, 2, 3]`
    List {
        const_: bool,
        element_ty: Option<Node<Type>>,
        elements: Vec<Node<Expr>>,
    },
    /// `Map<K, V>`.
    ///
    /// `var map = { 'k1' : 'v1', 'k2' : 'v2', 'k3' : 'v3'}`
    Map {
        const_: bool,
        kv_ty: Option<(Node<Type>, Node<Type>)>,
        kv: Vec<(Node<Expr>, Node<Expr>)>,
    },
    /// A number literal (`1.2`).
    Number(Symbol),
    /// One or more string literals, implicitly
    /// concatenated to form a single `String`.
    ///
    /// `"first string" "second string"`
    String(Vec<StringLiteral>),
    /// A symbol literal (`#+`, `#x`, etc.).
    Symbol(SymbolLiteral),
    /// An expression enclosed in parantheses (`(expr)`).
    Paren(Node<Expr>),
    /// An expression thrown as a result of raising an exception.
    ///
    /// `throw expression`
    Throw(Node<Expr>),
    /// Cascade notation that allows a sequence
    /// of operations on the same object.
    ///
    /// `expr..suffix`
    Cascade(Node<Expr>, Cascade),
}

/// A symbol literal.
#[derive(Clone, Debug)]
pub enum SymbolLiteral {
    /// `#` followed by an overloaded operator.
    Op(OverloadedOp),
    /// Dot separated identifiers.
    Path(Vec<Symbol>),
}

/// A single line string that is delimited by matching single or double quotes.
#[derive(Clone, Debug)]
pub struct StringLiteral {
    /// Raw string literals do not allow for interpolation or case escapes.
    ///
    /// `r'this is a string literal that does not recognize ${interpolation}'`
    pub raw: bool,
    /// Triple matching quotes allow multiline strings.
    ///
    /// `'''Multi-line strings`
    ///
    /// `like this one`
    ///
    /// `are allowed through triple quotes.'''`
    pub triple: bool,
    /// Denotes the quote, be it single or double.
    pub quote: char,
    /// The first string literal piece.
    pub prefix: Span,
    /// The expressions to be included in the string literal.
    ///
    /// `'a is ${expr}'`
    pub interpolated: Vec<(Node<Expr>, Span)>,
}

impl StringLiteral {
    pub fn get_simple_string(&self) -> String {
        assert!(self.interpolated.is_empty());
        ::codemap().span_to_snippet(self.prefix.to_span()).unwrap()
    }
}

/// Non-unary postfix operations on expressions.
#[derive(Debug)]
pub enum Suffix {
    /// Indexing suffix (`..expr[idx]`).
    Index(Node<Expr>),
    /// Field suffix (`expr.name`).
    Field(Symbol),
    /// Gets the field if the expression returns a valid object (`expr?.name`).
    FieldIfNotNull(Symbol),
    /// Performs a function call on the object (`expr(a, b, c)`).
    Call(Vec<Node<Type>>, Args),
}

/// A cascaded method invocation that allows for a sequence
/// of operations to be performed on the same object.
///
/// `..children[0].pop().parent = this`
#[derive(Debug)]
pub struct Cascade {
    /// The sequence of operator, method, getter or setter invocations.
    pub suffixes: Vec<Suffix>,
    /// Optional assignment operation on the result of the suffixes.
    pub assign: Option<(Option<ValueBinOp>, Node<Expr>)>,
}

/// Labeled arguments.
#[derive(Debug)]
pub struct NamedArg {
    /// Comments that describe the named argument.
    pub comments: Vec<Span>,
    /// Name assigned to the argument corresponding to the named parameter.
    /// `method(name: value)`
    pub name: Symbol,
    /// The expression to be taken by the argument.
    pub expr: Node<Expr>,
}

/// The list of actual arguments to be used by the function call.
#[derive(Debug)]
pub struct Args {
    /// Positional arguments.
    pub unnamed: Vec<Node<Expr>>,
    /// Named arguments.
    pub named: Vec<NamedArg>,
}

/// An element of metadata prefixing a definition.
#[derive(Debug)]
pub enum MetaItem {
    /// Comments that describe the item.
    Comments(Vec<Span>),
    /// Metadata annotation that gives additional information (`@override`).
    Attribute {
        qualified: Node<Qualified>,
        arguments: Option<Args>,
    },
}

impl MetaItem {
    pub fn simple<S: ::IntoSymbol>(name: S) -> MetaItem {
        MetaItem::Attribute {
            qualified: Qualified::one(name, vec![]),
            arguments: None,
        }
    }
}

/// Metadata prefixing a definition.
pub type Meta = Vec<MetaItem>;

/// The signature of a function.
#[derive(Debug)]
pub struct FnSig {
    /// The type to be returned by the function.
    pub return_type: Node<Type>,
    /// The list of required parameters.
    pub required: Vec<ArgDef>,
    /// The list of optional parameters.
    pub optional: Vec<ArgDef>,
    /// Optional parameters to be bound.
    pub optional_kind: OptionalArgKind,
    /// Asynchronous function denoted through `async` or `async*`.
    pub async: bool,
    /// Generator function denoted through `sync*` or `async*`.
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

/// The body of a function.
#[derive(Debug)]
pub enum FnBody {
    /// The body is an expression preceded by an arrow and
    /// it returns its value (`=> expr`).
    Arrow(Node<Expr>),
    /// The body is a block statement (`{...}`).
    Block(Node<Statement>),
    /// The body is `native` / `native "..."`.
    Native(Option<StringLiteral>),
}

/// The style of optional arguments, if any.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum OptionalArgKind {
    /// Positional arguments (`[...]`).
    Positional,
    /// Named arguments (`{...}`).
    Named,
}

impl Default for OptionalArgKind {
    fn default() -> Self {
        OptionalArgKind::Positional
    }
}

/// Formal function argument definition.
#[derive(Debug)]
pub struct ArgDef {
    /// The metadata of the argument.
    pub meta: Meta,
    /// Allows overriding the type with a subtype (`covariant T x`).
    pub covariant: bool,
    /// The type of the argument.
    pub ty: VarType,
    /// Qualifies the argument as field, used in constructors (`this.x`).
    pub field: bool,
    /// Specifies if the default value is given through `=` instead of `:`.
    /// `(int x = 5)` instead of `(int x: 5)`
    pub default_uses_eq: bool,
    /// The variable.
    pub var: Node<VarDef>,
}

impl ArgDef {
    pub fn simple<S: ::IntoSymbol>(ty: Node<Type>, name: S) -> ArgDef {
        ArgDef {
            meta: vec![],
            covariant: false,
            ty: VarType { fcv: None, ty },
            field: false,
            default_uses_eq: false,
            var: Node::new(VarDef {
                name: name.into_symbol(),
                init: None,
            }),
        }
    }
}

/// Type specifier used in variable declaration.
#[derive(Debug)]
pub struct VarType {
    /// The variable qualifier which comes in addition to the type.
    pub fcv: Option<FinalConstVar>,
    /// The type of the variable to be declared (`int age = 30;`).
    pub ty: Node<Type>,
}

/// Variable qualifier.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum FinalConstVar {
    /// A final variable can only be set once (`final name = 'Dart';`).
    Final,
    /// A constant variable is a compile-time constant (`const length = 420;`).
    Const,
    /// Declares a variable without necessarily specifying its type (`var a = 0;`).
    Var,
}

/// Var definition.
#[derive(Debug)]
pub struct VarDef {
    /// The name assigned to the variable (`var x`).
    pub name: Symbol,
    /// The initial value given to the variable upon declaration (`var x = 0;`).
    pub init: Option<Node<Expr>>,
}

/// Control flow statement that iterates over a condition or collection.
#[derive(Debug)]
pub enum ForLoop {
    /// Standard form.
    ///
    /// `for(var v = 0; condition; expr) {...}`
    CLike(Node<Statement>, Option<Node<Expr>>, Vec<Node<Expr>>),
    /// A for loop that iterates over a collection.
    ///
    /// `for(x in collection) {...}`
    In(Symbol, Node<Expr>),
    /// A for loop that iterates over a collection while also
    /// declaring the variable for the iteration element.
    ///
    /// `for(var x in collection) {...}`
    InVar(VarType, Node<VarDef>, Node<Expr>),
}

/// Supports dispatching control among a large number of cases.
#[derive(Debug)]
pub struct SwitchCase {
    /// The labels assigned to the specific case.
    pub labels: Vec<Symbol>,
    /// The value it is matched against.
    pub value: Option<Node<Expr>>,
    /// The actions to be executed should the case match.
    pub statements: Vec<Node<Statement>>,
}

/// The catch clause of exception handlers.
#[derive(Debug)]
pub struct CatchPart {
    /// The exception object.
    ///
    /// `{...} catch(e) { print($e); }`
    pub exception: Node<VarDef>,
    /// The stacktrace object associated to the exception object
    /// to be used when additional information is required.
    ///
    /// `{...} catch(e, s) { print($e); print($s); }`
    pub trace: Option<Node<VarDef>>,
}

/// Supports the definition of exception handling code in a structured way.
#[derive(Debug)]
pub struct TryPart {
    /// Explicitly specifies the type of exception to be handled.
    ///
    /// `try {...} on ExceptionType catch (e) {...}`
    pub on: Option<Node<Type>>,
    /// Catches the exception as to make use of it later on.
    pub catch: Option<CatchPart>,
    /// The finally clause that contains a block statement.
    ///
    /// `try {...} catch (e) {...} finally {...}`
    pub block: Node<Statement>,
}

/// A statement.
#[derive(Debug)]
pub enum Statement {
    /// Comments that describe the statement.
    Comments(Vec<Span>, Option<Node<Statement>>),
    /// A block of statements (`{ ... }`).
    Block(Vec<Node<Statement>>),
    /// Statement to declare variables.
    ///
    /// `Type var_name`
    Vars(VarType, Vec<Node<VarDef>>),
    /// Statement defining a function.
    ///
    /// `function(x) { return x }`
    ///
    /// `function() => 10`
    Function(Node<Function>),
    /// A for loop that optionally assigns a variable, evaluates a condition
    /// and executes the statement.
    ///
    /// `for (assignment; looping condition; statement) { block }`
    For(bool, ForLoop, Node<Statement>),
    /// A while loop that evaluates the condition before the loop.
    ///
    /// `while (condition) statement`
    While(Node<Expr>, Node<Statement>),
    /// A do-while loop that evaluates the condition after the loop.
    ///
    /// `do { statement } while (condition)`
    DoWhile(Node<Statement>, Node<Expr>),
    /// A statement that compares integer, string,
    /// or compile-time constants using ==.
    ///
    /// `switch (variable) { ... }`
    Switch(Node<Expr>, Vec<SwitchCase>),
    /// An if block with an optional else block.
    ///
    /// `if (condition) statement else statement`
    If(Node<Expr>, Node<Statement>, Option<Node<Statement>>),
    /// A statement used to re-raise an exception.
    ///
    /// `rethrow`
    Rethrow,
    /// Statement that supports the definition of exception
    /// handling code in a structured way.
    ///
    /// `try block`
    Try(Node<Statement>, Vec<TryPart>),
    /// A break statement optionally followed by a label.
    ///
    /// `break label`
    Break(Option<Symbol>),
    /// A continue statement optionally followed by a label.
    ///
    /// `continue label`
    Continue(Option<Symbol>),
    /// A return statement optionally followed by an expression.
    ///
    /// `return expr`
    Return(Option<Node<Expr>>),
    /// A statement that adds an element
    /// to the result of a function.
    ///
    /// `yield expr`
    Yield(Node<Expr>),
    /// A statement that adds a series of values
    /// to the result of a function.
    ///
    /// `yield* expr`
    YieldEach(Node<Expr>),
    /// Expression or empty statement (`expr;` or `;`).
    Expression(Option<Node<Expr>>),
    /// A statement that disrupts normal execution
    /// if a boolean condition is false.
    ///
    /// `assert(condition)`
    ///
    /// `assert(condition, message)`
    Assert(Args),
    /// Statement preceded by a label to be used as a break
    /// or continue target from within the statement.
    ///
    /// `label: statement`
    Labelled(Symbol, Node<Statement>),
}
