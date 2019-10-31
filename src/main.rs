mod pp;
use itertools::Itertools;
use pp::{PrettyPrinter, PP};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Symbol(u32, String);

#[derive(Debug, Clone)]
pub struct SymbolGenerator(Rc<Cell<u32>>);
impl SymbolGenerator {
    pub fn new() -> Self {
        Self(Rc::new(Cell::new(0)))
    }

    pub fn gensym(&mut self, hint: impl Into<String>) -> Symbol {
        let id = self.0.get();
        self.0.set(id + 1);
        Symbol(id, hint.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(String);

impl TypeId {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }
}

mod case {
    use super::*;

    #[derive(Debug, Clone)]
    pub enum Expr {
        Tuple(Vec<Expr>),
        Inject {
            descriminant: u8,
            data: Option<Box<Expr>>,
        },
        Case {
            cond: Box<Expr>,
            ty: TypeId,
            clauses: Vec<(Pattern, Expr)>,
        },
        Symbol(Symbol),
    }

    #[derive(Debug, Clone)]
    pub enum Value {
        Tuple(Vec<Value>),
        Constructor {
            descriminant: u8,
            value: Option<Box<Value>>,
        },
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        Tuple(Vec<Pattern>),
        Constructor {
            descriminant: u8,
            pattern: Option<Box<Pattern>>,
        },
        Variable(Symbol),
    }

    impl Pattern {
        pub fn is_tuple(&self) -> bool {
            use Pattern::*;
            match self {
                Tuple(_) => true,
                _ => false,
            }
        }

        pub fn tuple(self) -> Vec<Pattern> {
            use Pattern::*;
            match self {
                Tuple(patterns) => patterns,
                _ => panic!("pattern is not a tuple"),
            }
        }

        pub fn is_constructor(&self) -> bool {
            use Pattern::*;
            match self {
                Constructor { .. } => true,
                _ => false,
            }
        }

        pub fn constructor(self) -> (u8, Option<Pattern>) {
            match self {
                Pattern::Constructor {
                    descriminant,
                    pattern,
                } => (descriminant, pattern.map(|p| *p)),
                _ => panic!("pattern is not a constructor"),
            }
        }
        pub fn is_variable(&self) -> bool {
            use Pattern::*;
            match self {
                Variable { .. } => true,
                _ => false,
            }
        }

        pub fn variable(self) -> Symbol {
            match self {
                Pattern::Variable(s) => s,
                _ => panic!("pattern is not a variable"),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Constructor {
        pub descriminant: u8,
        pub param: Option<TypeId>,
    }

    #[derive(Debug, Clone)]
    pub enum Type {
        Tuple(Vec<TypeId>),
        Adt(Vec<Constructor>),
    }

    impl Type {
        pub fn tuple(self) -> Vec<TypeId> {
            use Type::*;
            match self {
                Tuple(ts) => ts,
                _ => panic!("type is not a tuple"),
            }
        }
        pub fn adt(self) -> Vec<Constructor> {
            use Type::*;
            match self {
                Adt(cs) => cs,
                _ => panic!("type is not an ADT"),
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct TypeDb(HashMap<TypeId, Type>);
    impl TypeDb {
        pub fn new() -> Self {
            Self(HashMap::new())
        }

        pub fn register_tuple(&mut self, type_id: TypeId, tys: impl IntoIterator<Item = TypeId>) {
            self.0
                .insert(type_id, Type::Tuple(tys.into_iter().collect()));
        }

        pub fn register_adt(
            &mut self,
            type_id: TypeId,
            constructors: impl IntoIterator<Item = Constructor>,
        ) {
            self.0
                .insert(type_id, Type::Adt(constructors.into_iter().collect()));
        }

        pub fn find(&self, type_id: &TypeId) -> Option<&Type> {
            self.0.get(type_id)
        }
    }
}

struct CaseInterp {
    scope: HashMap<Symbol, case::Value>,
}

struct Fail;
struct Match;

impl CaseInterp {
    pub fn new() -> Self {
        Self {
            scope: HashMap::new(),
        }
    }

    pub fn eval(&mut self, expr: case::Expr) -> Result<case::Value, Match> {
        use case::Expr::*;
        match expr {
            Tuple(tuple) => tuple
                .into_iter()
                .map(|e| self.eval(e))
                .collect::<Result<Vec<_>, Match>>()
                .map(case::Value::Tuple),
            Case { cond, clauses, .. } => {
                let cond = self.eval(*cond)?;
                let ret = clauses
                    .into_iter()
                    .map(|(pat, arm)| self.pattern_match(pat, cond.clone()).map(|()| arm))
                    .fold(Err(Fail), |is_match, ret| ret.or(is_match));
                match ret {
                    Ok(e) => self.eval(e),
                    Err(Fail) => Err(Match),
                }
            }
            Symbol(s) => Ok(self.resolve(&s)),
            Inject { descriminant, data } => Ok(case::Value::Constructor {
                descriminant,
                value: match data {
                    None => None,
                    Some(d) => Some(Box::new(self.eval(*d)?)),
                },
            }),
        }
    }

    fn pattern_match(&mut self, pattern: case::Pattern, value: case::Value) -> Result<(), Fail> {
        match (pattern, value) {
            (case::Pattern::Variable(s), v) => {
                self.scope.insert(s, v);
                Ok(())
            }
            (case::Pattern::Tuple(ps), case::Value::Tuple(vs)) => ps
                .into_iter()
                .zip(vs)
                .map(|(p, v)| self.pattern_match(p, v))
                .fold(Ok(()), |is_match, ret| ret.and(is_match)),
            (
                case::Pattern::Constructor {
                    descriminant: c1,
                    pattern,
                },
                case::Value::Constructor {
                    descriminant: c2,
                    value,
                },
            ) => {
                if c1 == c2 {
                    match (pattern, value) {
                        (None, None) => Ok(()),
                        (Some(p), Some(v)) => self.pattern_match(*p, *v),
                        _ => unreachable!("internal error, maybe type checker's bug"),
                    }
                } else {
                    Err(Fail)
                }
            }
            _ => unreachable!("internal error, maybe type checker's bug"),
        }
    }

    fn resolve(&mut self, s: &Symbol) -> case::Value {
        self.scope[s].clone()
    }
}
mod simple_case {
    use super::*;

    #[derive(Debug, Clone)]
    pub enum Expr {
        Tuple(Vec<Expr>),

        Let {
            var: Symbol,
            expr: Box<Expr>,
            body: Box<Expr>,
        },
        Inject {
            descriminant: u8,
            data: Option<Box<Expr>>,
        },
        Case {
            cond: Box<Expr>,
            clauses: Vec<(Pattern, Expr)>,
        },
        Symbol(Symbol),
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        Tuple(Vec<Symbol>),
        Constructor {
            descriminant: u8,
            data: Option<Symbol>,
        },
        Variable(Symbol),
    }
}

mod switch {
    use super::*;

    pub struct Block(pub Vec<Stmt>);

    pub enum Stmt {
        Assign(Symbol, Op),
        Store {
            base: Symbol,
            offset: u8,
            data: Symbol,
        },
        Switch {
            cond: Symbol,
            targets: Vec<(i32, Block)>,
            default: Block,
        },
        Unreachable,
        Ret(Symbol),
    }

    pub enum Op {
        Alloc { size: u8 },
        Load { base: Symbol, offset: u8 },
        Symbol(Symbol),
        Const(i32),
    }
}

type Stack<T> = Vec<T>;

trait PatternCompiler {
    fn compile(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr;
}

struct CaseToSimple<PC> {
    symbol_generator: SymbolGenerator,
    pattern_compiler: PC,
}

impl<PC> CaseToSimple<PC>
where
    PC: PatternCompiler,
{
    pub fn new(symbol_generator: SymbolGenerator, pattern_compiler: PC) -> Self {
        Self {
            symbol_generator,
            pattern_compiler,
        }
    }

    pub fn compile(&mut self, case: case::Expr) -> simple_case::Expr {
        match case {
            case::Expr::Tuple(vs) => self.compile_tuple(vs),
            case::Expr::Inject { descriminant, data } => {
                self.compile_inject(descriminant, data.map(|d| *d))
            }
            case::Expr::Case { cond, ty, clauses } => self.compile_case(*cond, ty, clauses),
            case::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_tuple(&mut self, data: Vec<case::Expr>) -> simple_case::Expr {
        simple_case::Expr::Tuple(data.into_iter().map(|d| self.compile(d)).collect())
    }

    fn compile_inject(&mut self, descriminant: u8, data: Option<case::Expr>) -> simple_case::Expr {
        simple_case::Expr::Inject {
            descriminant,
            data: data.map(|d| Box::new(self.compile(d))),
        }
    }

    fn compile_case(
        &mut self,
        cond: case::Expr,
        ty: TypeId,
        clauses: Vec<(case::Pattern, case::Expr)>,
    ) -> simple_case::Expr {
        let cond = self.compile(cond);
        let clauses = clauses
            .into_iter()
            .map(|(pat, arm)| (vec![pat], self.compile(arm)))
            .collect();
        let v = self.gensym("v");

        simple_case::Expr::Let {
            expr: Box::new(cond),
            var: v.clone(),
            body: Box::new(self.pattern_compiler.compile(vec![(v, ty)], clauses)),
        }
    }

    fn compile_symbol(&mut self, s: Symbol) -> simple_case::Expr {
        simple_case::Expr::Symbol(s)
    }

    fn gensym(&mut self, hint: impl Into<String>) -> Symbol {
        self.symbol_generator.gensym(hint)
    }
}

struct BackTrackPatternCompiler {
    symbol_generator: SymbolGenerator,
    type_db: case::TypeDb,
}
impl BackTrackPatternCompiler {
    pub fn new(symbol_generator: SymbolGenerator, type_db: case::TypeDb) -> Self {
        Self {
            symbol_generator,
            type_db,
        }
    }

    fn compile(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
        default: Option<simple_case::Expr>,
    ) -> simple_case::Expr {
        // assuming clauses.any(|(patterns, _)| patterns.len() == cond.len())
        if cond.len() == 0 {
            self.compile_empty(cond, clauses, default)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_variable())
        {
            self.compile_variable(cond, clauses, default)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_tuple())
        {
            self.compile_tuple(cond, clauses, default)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_constructor())
        {
            self.compile_constructor(cond, clauses, default)
        } else {
            self.compile_mixture(cond, clauses, default)
        }
    }

    fn compile_empty(
        &mut self,
        _: Stack<(Symbol, TypeId)>,
        mut clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
        _: Option<simple_case::Expr>,
    ) -> simple_case::Expr {
        match clauses.len() {
            0 => panic!("non-exhausitive pattern"),
            1 => clauses.remove(0).1,
            _ => panic!("redundant pattern"),
        }
    }

    fn compile_variable(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
        default: Option<simple_case::Expr>,
    ) -> simple_case::Expr {
        let (sym, _) = cond.pop().unwrap();
        let clauses = clauses
            .into_iter()
            .map(|(mut pat, arm)| {
                let var = pat.pop().unwrap().variable();
                (
                    pat,
                    simple_case::Expr::Let {
                        expr: Box::new(simple_case::Expr::Symbol(sym.clone())),
                        var,
                        body: Box::new(arm),
                    },
                )
            })
            .collect();
        self.compile(cond, clauses, default)
    }

    fn compile_tuple(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
        default: Option<simple_case::Expr>,
    ) -> simple_case::Expr {
        let (sym, ty) = cond.pop().unwrap();

        let param_tys: Vec<TypeId> = self.type_db.find(&ty).cloned().unwrap().tuple();
        let tmp_vars = std::iter::repeat_with(|| self.symbol_generator.gensym("v"))
            .take(param_tys.len())
            .collect::<Vec<_>>();
        let mut new_cond = cond.clone();
        new_cond.extend(tmp_vars.iter().cloned().zip(param_tys).rev());
        let clauses = clauses
            .into_iter()
            .map(|(mut patterns, arm)| {
                // front pattern must be a tuple
                let vars = patterns.pop().unwrap().tuple();
                patterns.extend(vars.into_iter().rev());
                (patterns, arm)
            })
            .collect();
        simple_case::Expr::Case {
            cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
            clauses: vec![(
                simple_case::Pattern::Tuple(tmp_vars),
                self.compile(new_cond, clauses, default.clone()),
            )],
        }
    }

    fn compile_constructor(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
        default: Option<simple_case::Expr>,
    ) -> simple_case::Expr {
        let (sym, ty) = cond.pop().unwrap();
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.pop().unwrap().constructor();
                (head, clause)
            })
            .collect::<Vec<_>>();
        let descriminants = clause_with_heads
            .iter()
            .map(|c| (c.0).0)
            .collect::<HashSet<_>>();
        let mut clauses = descriminants
            .iter()
            .map(|&descriminant| {
                let clauses = self.specialize(descriminant, clause_with_heads.iter());
                let param_ty = self
                    .type_db
                    .find(&ty)
                    .cloned()
                    .unwrap()
                    .adt()
                    .into_iter()
                    .find(|c| c.descriminant == descriminant)
                    .map(|c| c.param)
                    .unwrap();
                let tmp_var = param_ty.clone().map(|_| self.symbol_generator.gensym("v"));
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_var.iter().cloned().zip(param_ty).rev());
                (
                    simple_case::Pattern::Constructor {
                        descriminant,
                        data: tmp_var,
                    },
                    self.compile(new_cond, clauses, default.clone()),
                )
            })
            .collect();

        if self.is_exhausitive(&ty, descriminants) {
            if default.is_some() {
                panic!("redundant pattern")
            }
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses: clauses,
            }
        } else if let Some(default) = default {
            clauses.push((
                simple_case::Pattern::Variable(self.symbol_generator.gensym("_")),
                default,
            ));
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses: clauses,
            }
        } else {
            panic!("pattern is not exhausitive")
        }
    }

    fn compile_mixture(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
        default: Option<simple_case::Expr>,
    ) -> simple_case::Expr {
        if clauses[0].0.last().unwrap().is_variable() {
            let pos = clauses
                .iter()
                .position(|(pat, _)| pat.last().unwrap().is_constructor())
                .unwrap();
            let (vars, other) = clauses.split_at(pos);
            let default = self.compile(cond.clone(), other.to_vec(), default);
            self.compile(cond, vars.to_vec(), Some(default))
        } else {
            let pos = clauses
                .iter()
                .position(|(pat, _)| pat.last().unwrap().is_variable())
                .unwrap();
            let (consts, other) = clauses.split_at(pos);
            let default = self.compile(cond.clone(), other.to_vec(), default);
            self.compile(cond, consts.to_vec(), Some(default))
        }
    }

    fn specialize<'a, 'b>(
        &'a mut self,
        descriminant: u8,
        clause_with_heads: impl Iterator<
            Item = &'b (
                (u8, Option<case::Pattern>),
                (Stack<case::Pattern>, simple_case::Expr),
            ),
        >,
    ) -> Vec<(Stack<case::Pattern>, simple_case::Expr)> {
        clause_with_heads
            .filter(|(head, _)| head.0 == descriminant)
            .cloned()
            .map(|(head, (mut pat, arm))| {
                pat.extend(head.1.into_iter());
                (pat, arm)
            })
            .collect()
    }

    fn is_exhausitive(
        &self,
        type_id: &TypeId,
        descriminansts: impl IntoIterator<Item = u8>,
    ) -> bool {
        self.type_db
            .find(&type_id)
            .cloned()
            .unwrap()
            .adt()
            .into_iter()
            .map(|c| c.descriminant)
            .collect::<HashSet<_>>()
            == descriminansts.into_iter().collect::<HashSet<_>>()
    }
}

impl PatternCompiler for BackTrackPatternCompiler {
    fn compile(
        &mut self,
        cond: Vec<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        self.compile(cond, clauses, None)
    }
}

struct DecisionTreePatternCompiler {
    type_db: case::TypeDb,
    symbol_generator: SymbolGenerator,
}

impl DecisionTreePatternCompiler {
    pub fn new(symbol_generator: SymbolGenerator, type_db: case::TypeDb) -> Self {
        Self {
            type_db,
            symbol_generator,
        }
    }

    fn compile(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        // assuming clauses.any(|(patterns, _)| patterns.len() == cond.len())
        if clauses.len() == 0 {
            self.compile_empty(cond, clauses)
        } else if clauses[0].0.iter().all(|p| p.is_variable()) {
            self.compile_variable(cond, clauses)
        } else if clauses[0].0.iter().any(|p| p.is_tuple()) {
            self.compile_tuple(cond, clauses)
        } else {
            self.compile_mixture(cond, clauses)
        }
    }

    fn compile_empty(
        &mut self,
        _: Stack<(Symbol, TypeId)>,
        _: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        panic!("non-exhausitive pattern");
    }

    fn compile_variable(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        mut clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let (patterns, expr) = clauses.remove(0);
        patterns
            .into_iter()
            .map(|p| p.variable())
            .zip(cond.iter().cloned())
            .fold(expr, |acc, (p, (sym, _))| simple_case::Expr::Let {
                expr: Box::new(simple_case::Expr::Symbol(sym)),
                var: p,
                body: Box::new(acc),
            })
    }

    fn compile_tuple(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let pos = self.find_tuple(&clauses);

        let (sym, ty) = cond.swap_remove(pos);
        let param_tys: Vec<TypeId> = self.type_db.find(&ty).cloned().unwrap().tuple();
        let clauses = clauses
            .into_iter()
            .map(|(mut patterns, mut arm)| {
                let tuple = match patterns.swap_remove(pos) {
                    case::Pattern::Tuple(t) => t,
                    case::Pattern::Variable(var) => {
                        let pattern = std::iter::repeat_with(|| self.symbol_generator.gensym("_"))
                            .map(case::Pattern::Variable)
                            .take(param_tys.len())
                            .collect();
                        arm = simple_case::Expr::Let {
                            expr: Box::new(simple_case::Expr::Symbol(sym.clone())),
                            var: var.clone(),
                            body: Box::new(arm),
                        };
                        pattern
                    }
                    _ => unreachable!(),
                };
                patterns.extend(tuple.into_iter().rev());
                (patterns, arm)
            })
            .collect();

        let tmp_vars = std::iter::repeat_with(|| self.symbol_generator.gensym("v"))
            .take(param_tys.len())
            .collect::<Vec<_>>();
        cond.extend(tmp_vars.iter().cloned().zip(param_tys.clone()).rev());
        self.compile(cond, clauses)
    }
    fn compile_mixture(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let pos = self.find_constructor(&clauses);

        let (sym, ty) = cond.swap_remove(pos);
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.swap_remove(pos);
                (head, clause)
            })
            .collect::<Vec<_>>();
        let descriminants = clause_with_heads
            .iter()
            .filter_map(|(head, _)| match head {
                case::Pattern::Constructor { descriminant, .. } => Some(*descriminant),
                _ => None,
            })
            .collect::<HashSet<_>>();
        let mut clauses = descriminants
            .iter()
            .map(|&descriminant| {
                let clauses = self.specialized_patterns(
                    sym.clone(),
                    &ty,
                    descriminant,
                    clause_with_heads.iter(),
                );
                let param_ty = self
                    .type_db
                    .find(&ty)
                    .cloned()
                    .unwrap()
                    .adt()
                    .into_iter()
                    .find(|c| c.descriminant == descriminant)
                    .map(|c| c.param)
                    .unwrap();
                let tmp_var = param_ty.clone().map(|_| self.symbol_generator.gensym("v"));
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_var.iter().cloned().zip(param_ty).rev());
                (
                    simple_case::Pattern::Constructor {
                        descriminant,
                        data: tmp_var,
                    },
                    self.compile(new_cond, clauses),
                )
            })
            .collect();

        if self.is_exhausitive(&ty, descriminants) {
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses: clauses,
            }
        } else {
            let default = self.default_patterns(sym.clone(), cond, clause_with_heads.iter());
            clauses.push((
                simple_case::Pattern::Variable(self.symbol_generator.gensym("_")),
                default,
            ));
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses: clauses,
            }
        }
    }

    fn find_tuple(&mut self, clauses: &[(Stack<case::Pattern>, simple_case::Expr)]) -> usize {
        clauses[0].0.iter().rposition(|p| p.is_tuple()).unwrap()
    }

    fn find_constructor(&mut self, clauses: &[(Stack<case::Pattern>, simple_case::Expr)]) -> usize {
        clauses[0]
            .0
            .iter()
            .rposition(|p| p.is_constructor())
            .unwrap()
    }

    fn specialized_patterns<'a, 'b>(
        &'a mut self,
        cond: Symbol,
        type_id: &TypeId,
        descriminant: u8,
        clause_with_heads: impl Iterator<
            Item = &'b (case::Pattern, (Stack<case::Pattern>, simple_case::Expr)),
        >,
    ) -> Vec<(Stack<case::Pattern>, simple_case::Expr)> {
        let param_ty = self
            .type_db
            .find(&type_id)
            .cloned()
            .unwrap()
            .adt()
            .into_iter()
            .find(|c| c.descriminant == descriminant)
            .map(|c| c.param)
            .unwrap();
        clause_with_heads
            .filter_map(|(head, clause)| match head {
                case::Pattern::Constructor {
                    descriminant: d,
                    pattern,
                } if *d == descriminant => Some((pattern.clone().map(|p| *p), clause.clone())),
                case::Pattern::Variable(var) => {
                    let pattern = param_ty
                        .as_ref()
                        .map(|_| self.symbol_generator.gensym("_"))
                        .map(case::Pattern::Variable);

                    let (pat, arm) = clause.clone();
                    let arm = simple_case::Expr::Let {
                        expr: Box::new(simple_case::Expr::Symbol(cond.clone())),
                        var: var.clone(),
                        body: Box::new(arm),
                    };
                    Some((pattern, (pat, arm)))
                }
                _ => None,
            })
            .map(|(patterns, (mut pat, arm))| {
                pat.extend(patterns.into_iter().rev());
                (pat, arm)
            })
            .collect()
    }

    fn default_patterns<'a, 'b>(
        &'a mut self,
        sym: Symbol,
        cond: Stack<(Symbol, TypeId)>,
        clause_with_heads: impl Iterator<
            Item = &'b (case::Pattern, (Stack<case::Pattern>, simple_case::Expr)),
        >,
    ) -> simple_case::Expr {
        let clauses = clause_with_heads
            .filter(|(head, _)| head.is_variable())
            .cloned()
            .map(|(p, (pat, arm))| {
                let var = p.variable();
                let arm = simple_case::Expr::Let {
                    expr: Box::new(simple_case::Expr::Symbol(sym.clone())),
                    var: var.clone(),
                    body: Box::new(arm),
                };
                (pat, arm)
            })
            .collect();
        self.compile(cond, clauses)
    }

    fn is_exhausitive(
        &self,
        type_id: &TypeId,
        descriminansts: impl IntoIterator<Item = u8>,
    ) -> bool {
        self.type_db
            .find(&type_id)
            .cloned()
            .unwrap()
            .adt()
            .into_iter()
            .map(|c| c.descriminant)
            .collect::<HashSet<_>>()
            == descriminansts.into_iter().collect::<HashSet<_>>()
    }
}

impl PatternCompiler for DecisionTreePatternCompiler {
    fn compile(
        &mut self,
        cond: Vec<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        self.compile(cond, clauses)
    }
}

struct SimpleToSwitch {
    symbol_generator: SymbolGenerator,
}

impl SimpleToSwitch {
    pub fn new(symbol_generator: SymbolGenerator) -> Self {
        Self { symbol_generator }
    }

    pub fn compile(&mut self, case: simple_case::Expr) -> switch::Block {
        let (mut stmts, ret) = self.compile_expr(case);
        stmts.push(switch::Stmt::Ret(ret));
        switch::Block(stmts)
    }

    fn compile_expr(&mut self, case: simple_case::Expr) -> (Vec<switch::Stmt>, Symbol) {
        match case {
            simple_case::Expr::Tuple(t) => self.compile_tuple(t),
            simple_case::Expr::Let { var, expr, body } => self.compile_let(var, *expr, *body),
            simple_case::Expr::Inject { descriminant, data } => {
                self.compile_inject(descriminant, data.map(|d| *d))
            }
            simple_case::Expr::Case { cond, clauses } => self.compile_case(*cond, clauses),
            simple_case::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_tuple(&mut self, es: Vec<simple_case::Expr>) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};
        let mut stmts = vec![];
        let mut symbols = vec![];
        for e in es {
            let (s, sym) = self.compile_expr(e);
            stmts.extend(s);
            symbols.push(sym);
        }

        let t = self.gensym("tuple");
        stmts.push(Stmt::Assign(
            t.clone(),
            Op::Alloc {
                size: symbols.len() as u8,
            },
        ));
        for (i, s) in symbols.into_iter().enumerate() {
            stmts.push(Stmt::Store {
                base: t.clone(),
                offset: i as u8,
                data: s,
            })
        }
        (stmts, t)
    }

    fn compile_let(
        &mut self,
        var: Symbol,
        expr: simple_case::Expr,
        body: simple_case::Expr,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};
        let (mut stmts, sym) = self.compile_expr(expr);
        stmts.push(Stmt::Assign(var, Op::Symbol(sym)));
        let (stmts2, ret) = self.compile_expr(body);
        stmts.extend(stmts2);
        (stmts, ret)
    }

    fn compile_inject(
        &mut self,
        descriminant: u8,
        data: Option<simple_case::Expr>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};
        let mut ret = Vec::new();
        let size = (data.is_some() as u8) + 1;

        let des = self.gensym("des");
        ret.push(Stmt::Assign(des.clone(), Op::Const(descriminant as i32)));

        let mut syms = Vec::new();
        for d in data.into_iter() {
            let (stmts, d) = self.compile_expr(d);
            ret.extend(stmts);
            syms.push(d);
        }

        let v = self.gensym("v");
        ret.push(Stmt::Assign(v.clone(), Op::Alloc { size }));
        ret.push(Stmt::Store {
            base: v.clone(),
            offset: 0,
            data: des.clone(),
        });
        for (i, sym) in syms.into_iter().enumerate() {
            ret.push(Stmt::Store {
                base: v.clone(),
                offset: (i as u8) + 1,
                data: sym,
            })
        }
        (ret, v.clone())
    }

    fn compile_case(
        &mut self,
        cond: simple_case::Expr,
        clauses: Vec<(simple_case::Pattern, simple_case::Expr)>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Block, Op, Stmt};
        let mut default = None;
        let mut switch_clauses = Vec::new();
        let result_sym = self.gensym("switch_result");

        let (mut stmts, switch_cond) = self.compile_expr(cond);

        for (pat, arm) in clauses {
            let (switch_arm, symbol) = self.compile_expr(arm);
            match pat {
                simple_case::Pattern::Tuple(t) => {
                    let mut block = t
                        .into_iter()
                        .enumerate()
                        .map(|(i, s)| {
                            Stmt::Assign(
                                s,
                                Op::Load {
                                    base: switch_cond.clone(),
                                    offset: i as u8,
                                },
                            )
                        })
                        .collect::<Vec<_>>();
                    block.extend(switch_arm);
                    block.push(Stmt::Assign(result_sym.clone(), Op::Symbol(symbol)));
                }
                simple_case::Pattern::Variable(s) => {
                    let mut block = vec![Stmt::Assign(s.clone(), Op::Symbol(switch_cond.clone()))];
                    block.extend(switch_arm);
                    block.push(Stmt::Assign(result_sym.clone(), Op::Symbol(symbol)));
                    default = Some(Block(block))
                }
                simple_case::Pattern::Constructor { descriminant, data } => {
                    let mut block = data
                        .into_iter()
                        .enumerate()
                        .map(|(i, sym)| {
                            Stmt::Assign(
                                sym,
                                Op::Load {
                                    base: switch_cond.clone(),
                                    offset: (i + 1) as u8,
                                },
                            )
                        })
                        .collect::<Vec<_>>();
                    block.extend(switch_arm);
                    block.push(Stmt::Assign(result_sym.clone(), Op::Symbol(symbol)));
                    switch_clauses.push((descriminant as i32, Block(block)))
                }
            }
        }

        stmts.push(Stmt::Switch {
            cond: switch_cond,
            targets: switch_clauses,
            default: default.unwrap_or_else(|| Block(vec![Stmt::Unreachable])),
        });
        (stmts, result_sym)
    }

    fn compile_symbol(&mut self, s: Symbol) -> (Vec<switch::Stmt>, Symbol) {
        (vec![], s)
    }

    fn gensym(&mut self, hint: impl Into<String>) -> Symbol {
        self.symbol_generator.gensym(hint)
    }
}

fn main() {
    let mut type_db = case::TypeDb::new();
    type_db.register_tuple(TypeId::new("unit"), vec![]);

    type_db.register_adt(
        TypeId::new("bool"),
        vec![
            case::Constructor {
                descriminant: 0,
                param: None,
            },
            case::Constructor {
                descriminant: 1,
                param: None,
            },
        ],
    );

    type_db.register_tuple(
        TypeId::new("tuple2"),
        vec![TypeId::new("bool"), TypeId::new("bool")],
    );
    type_db.register_tuple(
        TypeId::new("tuple3"),
        vec![
            TypeId::new("bool"),
            TypeId::new("bool"),
            TypeId::new("bool"),
        ],
    );

    type_db.register_adt(
        TypeId::new("hoge"),
        vec![
            case::Constructor {
                descriminant: 0,
                param: None,
            },
            case::Constructor {
                descriminant: 1,
                param: Some(TypeId::new("bool")),
            },
            case::Constructor {
                descriminant: 2,
                param: Some(TypeId::new("tuple3")),
            },
        ],
    );

    let mut sg = SymbolGenerator::new();

    let m = {
        use case::*;
        use Expr::*;

        let truev = Expr::Inject {
            descriminant: 0,
            data: None,
        };
        let unitv = Expr::Inject {
            descriminant: 0,
            data: None,
        };

        Case {
            cond: Box::new(Inject {
                descriminant: 2,
                data: Some(Box::new(Tuple(vec![
                    truev.clone(),
                    truev.clone(),
                    truev.clone(),
                ]))),
            }),
            ty: TypeId::new("hoge"),
            clauses: vec![
                (
                    Pattern::Constructor {
                        descriminant: 0,
                        pattern: None,
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 1,
                        pattern: Some(Box::new(Pattern::Variable(sg.gensym("x")))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Constructor {
                                descriminant: 0,
                                pattern: None,
                            },
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Variable(sg.gensym("z")),
                        ]))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Constructor {
                                descriminant: 0,
                                pattern: None,
                            },
                            Pattern::Variable(sg.gensym("z")),
                        ]))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Constructor {
                                descriminant: 0,
                                pattern: None,
                            },
                        ]))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Variable(sg.gensym("z")),
                        ]))),
                    },
                    unitv.clone(),
                ),
            ],
        }
    };

    let m2 = {
        use case::*;
        use Expr::*;

        let falsev = Expr::Inject {
            descriminant: 0,
            data: None,
        };
        let truev = Expr::Inject {
            descriminant: 1,
            data: None,
        };
        let falsep = Pattern::Constructor {
            descriminant: 0,
            pattern: None,
        };
        let truep = Pattern::Constructor {
            descriminant: 1,
            pattern: None,
        };

        fn tuple2p(p1: Pattern, p2: Pattern) -> Pattern {
            Pattern::Tuple(vec![p1, p2])
        }

        Case {
            cond: Box::new(Tuple(vec![falsev.clone(), falsev.clone()])),
            ty: TypeId::new("tuple2"),
            clauses: vec![
                (tuple2p(truep.clone(), truep.clone()), falsev.clone()),
                (tuple2p(truep.clone(), falsep.clone()), truev.clone()),
                (tuple2p(falsep.clone(), truep.clone()), truev.clone()),
                (tuple2p(falsep.clone(), falsep.clone()), falsev.clone()),
            ],
        }
    };

    let mut p = PrettyPrinter::new();
    p.pp(&m2);

    println!("");
    println!("evaled to");
    let mut interpreter = CaseInterp::new();
    let v = interpreter.eval(m2.clone());

    match v {
        Ok(v) => {
            p.pp(&v);
            println!()
        }
        Err(Match) => println!("match failed"),
    }

    println!("");
    p.pp(&m);
    println!("");
    println!("case to simple (backtrack)");
    let mut compiler = CaseToSimple::new(
        sg.clone(),
        BackTrackPatternCompiler::new(sg.clone(), type_db.clone()),
    );
    let c = compiler.compile(m.clone());
    p.pp(&c);

    println!("simple to switch");
    let mut compiler = SimpleToSwitch::new(sg.clone());
    let s = compiler.compile(c);

    println!("");
    p.pp(&s);

    println!("case to simple (DecisionTree)");
    let mut compiler2 = CaseToSimple::new(
        sg.clone(),
        DecisionTreePatternCompiler::new(sg.clone(), type_db.clone()),
    );
    let c2 = compiler2.compile(m);
    p.pp(&c2);

    println!("simple to switch");
    let mut compiler = SimpleToSwitch::new(sg.clone());
    let s2 = compiler.compile(c2);

    println!("");
    p.pp(&s2);
}
