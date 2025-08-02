//! Abstract Syntax Tree definitions for Flow language

use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Function(Function),
    Type(TypeDef),
    ExternalTool(ExternalTool),
    Context(Context),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub intent: Option<String>,
    pub name: String,
    pub params: Vec<Parameter>,
    pub accepts: Option<TypeExpr>,
    pub returns: Option<TypeExpr>,
    pub tests: Vec<TestCase>,
    pub properties: Vec<Property>,
    pub invariants: Vec<Invariant>,
    pub examples: Vec<Example>,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_expr: Option<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestCase {
    pub call: String,
    pub expected: TestExpectation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TestExpectation {
    Yields(Expr),
    Fails { message: Option<String> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    pub quantifier: Quantifier,
    pub variable: String,
    pub condition: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Quantifier {
    ForAll,
    Exists,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Invariant {
    pub condition: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Example {
    pub call: String,
    pub result: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionBody {
    Expression(Expr),
    Block(Vec<Statement>),
    PatternMatch(Vec<WhenClause>, Option<Box<Statement>>),
    InferFromExamples,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenClause {
    pub pattern: Pattern,
    pub body: Statement,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Variable(String),
    Wildcard,
    Comparison { var: String, op: CompareOp, value: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let { name: String, value: Expr },
    Return(Expr),
    Expression(Expr),
    Checkpoint { name: String, yields: Option<String> },
    Attempt { body: Box<Statement>, recover: Vec<RecoverClause>, finally_fail: Option<Box<Statement>> },
    Ensuring { body: Box<Statement>, cleanup: Box<Statement> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecoverClause {
    pub strategy: RecoverStrategy,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RecoverStrategy {
    Default,
    Retry(u32),
    With(String), // handler name
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Binary { left: Box<Expr>, op: BinaryOp, right: Box<Expr> },
    Unary { op: UnaryOp, expr: Box<Expr> },
    Call { func: String, args: Vec<Expr> },
    MemberAccess { object: Box<Expr>, member: String },
    Index { object: Box<Expr>, index: Box<Expr> },
    Pipe { expr: Box<Expr>, transform: Box<Transform> },
    Cast { expr: Box<Expr>, target_type: TypeExpr },
    Record { fields: Vec<(String, Expr)> },
    List { elements: Vec<Expr> },
    Lambda { params: Vec<String>, body: Box<Expr> },
    Ternary { condition: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Transform {
    Function(String),
    Map { func: Box<Expr> },
    Filter { predicate: Box<Expr> },
    Transform { language: String, code: String },
    Extract { pattern: RecordPattern },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordPattern {
    pub fields: Vec<(String, PatternField)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternField {
    Field(String),
    Optional { field: String, default: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
    And, Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompareOp {
    Equal, NotEqual, Less, Greater, LessEqual, GreaterEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    pub name: String,
    pub definition: TypeDefinition,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Alias(TypeExpr),
    Record { fields: Vec<(String, TypeExpr)> },
    Enum { variants: Vec<String> },
    Semantic { base: Box<TypeExpr>, constraints: Vec<Constraint> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    Named(String),
    Integer,
    Decimal,
    String,
    Boolean,
    List(Box<TypeExpr>),
    Map { key: Box<TypeExpr>, value: Box<TypeExpr> },
    Function { params: Vec<TypeExpr>, returns: Box<TypeExpr> },
    Record { fields: Vec<(String, TypeExpr)> },
    Optional(Box<TypeExpr>),
    Where { base: Box<TypeExpr>, constraint: Box<Expr> },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Matching { pattern: String },
    Where { condition: Box<Expr> },
    WithMeaning { description: String },
    WithUnit { unit: String },
    WithPrecision { digits: u32 },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternalTool {
    pub language: String,
    pub import_path: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    pub name: String,
    pub settings: Vec<(String, ContextValue)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContextValue {
    Duration { value: u32, unit: String },
    Integer(i64),
    String(String),
    Boolean(bool),
}