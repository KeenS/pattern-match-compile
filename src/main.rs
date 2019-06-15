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
        Inject {
            descriminant: u8,
            data: Vec<Expr>,
        },
        Case {
            cond: Box<Expr>,
            ty: TypeId,
            clauses: Vec<(Pattern, Expr)>,
        },
        Symbol(Symbol),
    }

    #[derive(Debug, Clone)]
    pub struct Value {
        pub descriminant: u8,
        pub data: Vec<Value>,
    }

    #[derive(Debug, Clone)]
    pub struct Constructor {
        pub descriminant: u8,
        pub pattern: Vec<Pattern>,
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        Constructor(Constructor),
        Variable(Symbol),
    }

    impl Pattern {
        pub fn is_constructor(&self) -> bool {
            use Pattern::*;
            match self {
                Constructor { .. } => true,
                Variable { .. } => false,
            }
        }

        pub fn constructor(self) -> Constructor {
            match self {
                Pattern::Constructor(c) => c,
                _ => panic!("pattern is not a constructor"),
            }
        }
        pub fn is_variable(&self) -> bool {
            use Pattern::*;
            match self {
                Constructor { .. } => false,
                Variable { .. } => true,
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
    pub struct ConstructorType {
        pub descriminant: u8,
        pub params: Vec<TypeId>,
    }

    #[derive(Debug, Clone)]
    pub struct TypeDb(HashMap<TypeId, Vec<ConstructorType>>);
    impl TypeDb {
        pub fn new() -> Self {
            Self(HashMap::new())
        }

        pub fn insert(
            &mut self,
            type_id: TypeId,
            constructors: impl IntoIterator<Item = ConstructorType>,
        ) {
            self.0.insert(type_id, constructors.into_iter().collect());
        }

        pub fn constructors(&self, type_id: &TypeId) -> Option<HashSet<u8>> {
            self.0
                .get(type_id)
                .map(|cs| cs.iter().map(|c| c.descriminant).collect())
        }

        pub fn arity(&self, type_id: &TypeId, descriminant: u8) -> Option<usize> {
            self.0.get(type_id).and_then(|cs| {
                cs.iter()
                    .find(|c| c.descriminant == descriminant)
                    .map(|c| c.params.len())
            })
        }
        pub fn param_tys(&self, type_id: &TypeId, descriminant: u8) -> Option<Vec<TypeId>> {
            self.0
                .get(type_id)
                .and_then(|cs| cs.iter().find(|c| c.descriminant == descriminant))
                .map(|c| c.params.clone())
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
            Inject { descriminant, data } => Ok(case::Value {
                descriminant,
                data: data
                    .into_iter()
                    .map(|e| self.eval(e))
                    .collect::<Result<Vec<case::Value>, Match>>()?,
            }),
        }
    }

    fn pattern_match(&mut self, pattern: case::Pattern, value: case::Value) -> Result<(), Fail> {
        match (pattern, value) {
            (case::Pattern::Variable(s), v) => {
                self.scope.insert(s, v);
                Ok(())
            }
            (
                case::Pattern::Constructor(case::Constructor {
                    descriminant: c1,
                    pattern: ps,
                    ..
                }),
                case::Value {
                    descriminant: c2,
                    data: vs,
                },
            ) => {
                if c1 == c2 {
                    ps.into_iter()
                        .zip(vs)
                        .map(|(p, v)| self.pattern_match(p, v))
                        .fold(Ok(()), |is_match, ret| ret.and(is_match))
                } else {
                    Err(Fail)
                }
            }
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
        Let {
            var: Symbol,
            expr: Box<Expr>,
            body: Box<Expr>,
        },
        Inject {
            descriminant: u8,
            data: Vec<Expr>,
        },
        Case {
            cond: Box<Expr>,
            clauses: Vec<(Pattern, Expr)>,
        },
        Symbol(Symbol),
    }

    #[derive(Debug, Clone)]
    pub enum Pattern {
        Constructor { descriminant: u8, data: Vec<Symbol> },
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
            case::Expr::Inject { descriminant, data } => self.compile_inject(descriminant, data),
            case::Expr::Case { cond, ty, clauses } => self.compile_case(*cond, ty, clauses),
            case::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_inject(&mut self, descriminant: u8, data: Vec<case::Expr>) -> simple_case::Expr {
        simple_case::Expr::Inject {
            descriminant,
            data: data.into_iter().map(|d| self.compile(d)).collect(),
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
            .map(|c| c.0.descriminant)
            .collect::<HashSet<_>>();
        let mut clauses = descriminants
            .iter()
            .map(|&descriminant| {
                let clauses = self.specialize(descriminant, clause_with_heads.iter());
                let param_tys = self.type_db.param_tys(&ty, descriminant).unwrap();
                let tmp_vars = std::iter::repeat_with(|| self.symbol_generator.gensym("v"))
                    .take(param_tys.len())
                    .collect::<Vec<_>>();
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_vars.iter().cloned().zip(param_tys).rev());
                (
                    simple_case::Pattern::Constructor {
                        descriminant,
                        data: tmp_vars,
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
            Item = &'b (case::Constructor, (Stack<case::Pattern>, simple_case::Expr)),
        >,
    ) -> Vec<(Stack<case::Pattern>, simple_case::Expr)> {
        clause_with_heads
            .filter(|(head, _)| head.descriminant == descriminant)
            .cloned()
            .map(|(head, (mut pat, arm))| {
                pat.extend(head.pattern.into_iter().rev());
                (pat, arm)
            })
            .collect()
    }

    fn is_exhausitive(
        &self,
        type_id: &TypeId,
        descriminansts: impl IntoIterator<Item = u8>,
    ) -> bool {
        self.type_db.constructors(&type_id).unwrap()
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
                case::Pattern::Constructor(case::Constructor { descriminant, .. }) => {
                    Some(*descriminant)
                }
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
                let param_tys = self.type_db.param_tys(&ty, descriminant).unwrap();
                let tmp_vars = std::iter::repeat_with(|| self.symbol_generator.gensym("v"))
                    .take(param_tys.len())
                    .collect::<Vec<_>>();
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_vars.iter().cloned().zip(param_tys).rev());
                (
                    simple_case::Pattern::Constructor {
                        descriminant,
                        data: tmp_vars,
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
        let arity = self.type_db.arity(type_id, descriminant).unwrap();
        clause_with_heads
            .filter_map(|(head, clause)| match head {
                case::Pattern::Constructor(c) if c.descriminant == descriminant => {
                    Some((c.pattern.clone(), clause.clone()))
                }
                case::Pattern::Variable(var) => {
                    let patterns = std::iter::repeat_with(|| self.symbol_generator.gensym("_"))
                        .take(arity)
                        .map(case::Pattern::Variable)
                        .collect::<Vec<_>>();
                    let (pat, arm) = clause.clone();
                    let arm = simple_case::Expr::Let {
                        expr: Box::new(simple_case::Expr::Symbol(cond.clone())),
                        var: var.clone(),
                        body: Box::new(arm),
                    };
                    Some((patterns, (pat, arm)))
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
        self.type_db.constructors(&type_id).unwrap()
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

struct CaseToSwitch {
    symbol_generator: SymbolGenerator,
}

impl CaseToSwitch {
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
            simple_case::Expr::Let { var, expr, body } => self.compile_let(var, *expr, *body),
            simple_case::Expr::Inject { descriminant, data } => {
                self.compile_inject(descriminant, data)
            }
            simple_case::Expr::Case { cond, clauses } => self.compile_case(*cond, clauses),
            simple_case::Expr::Symbol(s) => self.compile_symbol(s),
        }
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
        data: Vec<simple_case::Expr>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};
        let size = (data.len() as u8) + 1;
        let v = self.gensym("v");
        let des = self.gensym("des");

        let mut ret = Vec::new();
        ret.push(Stmt::Assign(v.clone(), Op::Alloc { size }));
        ret.push(Stmt::Assign(des.clone(), Op::Const(descriminant as i32)));
        ret.push(Stmt::Store {
            base: v.clone(),
            offset: 0,
            data: des.clone(),
        });
        for (i, d) in data.into_iter().enumerate() {
            let (stmts, d) = self.compile_expr(d);
            ret.extend(stmts);
            ret.push(Stmt::Store {
                base: v.clone(),
                offset: (i as u8) + 1,
                data: d,
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
    type_db.insert(
        TypeId::new("unit"),
        vec![case::ConstructorType {
            descriminant: 0,
            params: vec![],
        }],
    );

    type_db.insert(
        TypeId::new("bool"),
        vec![
            case::ConstructorType {
                descriminant: 0,
                params: vec![],
            },
            case::ConstructorType {
                descriminant: 1,
                params: vec![],
            },
        ],
    );

    type_db.insert(
        TypeId::new("tuple2"),
        vec![case::ConstructorType {
            descriminant: 0,
            params: vec![TypeId::new("bool"), TypeId::new("bool")],
        }],
    );

    type_db.insert(
        TypeId::new("hoge"),
        vec![
            case::ConstructorType {
                descriminant: 0,
                params: vec![],
            },
            case::ConstructorType {
                descriminant: 1,
                params: vec![TypeId::new("bool")],
            },
            case::ConstructorType {
                descriminant: 2,
                params: vec![
                    TypeId::new("bool"),
                    TypeId::new("bool"),
                    TypeId::new("bool"),
                ],
            },
        ],
    );

    let mut sg = SymbolGenerator::new();

    let m = {
        use case::*;
        use Expr::*;

        let truev = Expr::Inject {
            descriminant: 0,
            data: vec![],
        };
        let unitv = Expr::Inject {
            descriminant: 0,
            data: vec![],
        };

        Case {
            cond: Box::new(Inject {
                descriminant: 2,
                data: vec![truev.clone(), truev.clone(), truev.clone()],
            }),
            ty: TypeId::new("hoge"),
            clauses: vec![
                (
                    Pattern::Constructor(Constructor {
                        descriminant: 0,
                        pattern: vec![],
                    }),
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor(Constructor {
                        descriminant: 1,
                        pattern: vec![Pattern::Variable(sg.gensym("x"))],
                    }),
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor(Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Constructor(Constructor {
                                descriminant: 0,
                                pattern: vec![],
                            }),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Variable(sg.gensym("z")),
                        ],
                    }),
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor(Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Constructor(Constructor {
                                descriminant: 0,
                                pattern: vec![],
                            }),
                            Pattern::Variable(sg.gensym("z")),
                        ],
                    }),
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor(Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Constructor(Constructor {
                                descriminant: 0,
                                pattern: vec![],
                            }),
                        ],
                    }),
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor(Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Variable(sg.gensym("z")),
                        ],
                    }),
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
            data: vec![],
        };
        let truev = Expr::Inject {
            descriminant: 1,
            data: vec![],
        };
        let falsep = Pattern::Constructor(Constructor {
            descriminant: 0,
            pattern: vec![],
        });
        let truep = Pattern::Constructor(Constructor {
            descriminant: 1,
            pattern: vec![],
        });

        fn tuple2p(p1: Pattern, p2: Pattern) -> Pattern {
            Pattern::Constructor(Constructor {
                descriminant: 0,
                pattern: vec![p1, p2],
            })
        }

        Case {
            cond: Box::new(Inject {
                descriminant: 0,
                data: vec![falsev.clone(), falsev.clone()],
            }),
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

    let mut interpreter = CaseInterp::new();
    let v = interpreter.eval(m2.clone());

    println!("evaled to");
    match v {
        Ok(v) => {
            p.pp(&v);
            println!()
        }
        Err(Match) => println!("match failed"),
    }

    let mut compiler = CaseToSimple::new(
        sg.clone(),
        BackTrackPatternCompiler::new(sg.clone(), type_db.clone()),
    );
    let c = compiler.compile(m.clone());
    p.pp(&c);

    let mut compiler = CaseToSwitch::new(sg.clone());
    let s = compiler.compile(c);

    p.pp(&s);

    let mut compiler2 = CaseToSimple::new(
        sg.clone(),
        DecisionTreePatternCompiler::new(sg.clone(), type_db.clone()),
    );
    let c2 = compiler2.compile(m);
    p.pp(&c2);

    let mut compiler = CaseToSwitch::new(sg.clone());
    let s2 = compiler.compile(c2);

    p.pp(&s2);
}
