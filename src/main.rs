mod pp;
use itertools::Itertools;
use pp::{PrettyPrinter, PP};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
#[derive(Debug, Clone)]
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

mod match_ {
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
    pub struct Constructor {
        pub type_id: TypeId,
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
    }

}

mod case {
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

struct MatchToCase {
    type_db: match_::TypeDb,
    symbol_generator: SymbolGenerator,
}

type Stack<T> = Vec<T>;

impl MatchToCase {
    pub fn new(symbol_generator: SymbolGenerator, type_db: match_::TypeDb) -> Self {
        Self {
            type_db,
            symbol_generator,
        }
    }

    pub fn compile(&mut self, match_: match_::Expr) -> case::Expr {
        match match_ {
            match_::Expr::Let { var, expr, body } => self.compile_let(var, *expr, *body),
            match_::Expr::Inject { descriminant, data } => self.compile_inject(descriminant, data),
            match_::Expr::Case { cond, clauses } => self.compile_match(*cond, clauses),
            match_::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_let(&mut self, var: Symbol, expr: match_::Expr, body: match_::Expr) -> case::Expr {
        case::Expr::Let {
            var,
            expr: Box::new(self.compile(expr)),
            body: Box::new(self.compile(body)),
        }
    }

    fn compile_inject(&mut self, descriminant: u8, data: Vec<match_::Expr>) -> case::Expr {
        case::Expr::Inject {
            descriminant,
            data: data.into_iter().map(|d| self.compile(d)).collect(),
        }
    }

    fn compile_match(
        &mut self,
        cond: match_::Expr,
        clauses: Vec<(match_::Pattern, match_::Expr)>,
    ) -> case::Expr {
        let cond = self.compile(cond);
        let clauses = clauses
            .into_iter()
            .map(|(pat, arm)| (vec![pat], self.compile(arm)))
            .collect();
        println!("called");
        let v = self.gensym("v");

        case::Expr::Let {
            expr: Box::new(cond),
            var: v.clone(),
            body: Box::new(self.match_compile(vec![v], clauses, None)),
        }
    }

    fn compile_symbol(&mut self, s: Symbol) -> case::Expr {
        case::Expr::Symbol(s)
    }

    fn match_compile(
        &mut self,
        cond: Stack<Symbol>,
        clauses: Vec<(Stack<match_::Pattern>, case::Expr)>,
        default: Option<case::Expr>,
    ) -> case::Expr {
        // assuming clauses.any(|(patterns, _)| patterns.len() == cond.len())
        if cond.len() == 0 {
            self.match_compile_empty(cond, clauses, default)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_variable())
        {
            self.match_compile_variable(cond, clauses, default)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_constructor())
        {
            self.match_compile_constructor(cond, clauses, default)
        } else {
            self.match_compile_mixture(cond, clauses, default)
        }
    }

    fn match_compile_empty(
        &mut self,
        _: Stack<Symbol>,
        mut clauses: Vec<(Stack<match_::Pattern>, case::Expr)>,
        _: Option<case::Expr>,
    ) -> case::Expr {
        match clauses.len() {
            0 => panic!("non-exhausitive pattern"),
            1 => clauses.remove(0).1,
            _ => panic!("redundant pattern"),
        }
    }

    fn match_compile_variable(
        &mut self,
        mut cond: Stack<Symbol>,
        clauses: Vec<(Stack<match_::Pattern>, case::Expr)>,
        default: Option<case::Expr>,
    ) -> case::Expr {
        let c = cond.pop().unwrap();
        let clauses = clauses
            .into_iter()
            .map(|(mut pat, arm)| {
                let var = pat.pop().unwrap().variable();
                (
                    pat,
                    case::Expr::Let {
                        expr: Box::new(case::Expr::Symbol(c.clone())),
                        var,
                        body: Box::new(arm),
                    },
                )
            })
            .collect();
        self.match_compile(cond, clauses, default)
    }

    fn match_compile_constructor(
        &mut self,
        mut cond: Stack<Symbol>,
        clauses: Vec<(Stack<match_::Pattern>, case::Expr)>,
        default: Option<case::Expr>,
    ) -> case::Expr {
        let c = cond.pop().unwrap();
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.pop().unwrap().constructor();
                (head, clause)
            })
            .collect::<Vec<_>>();
        let type_id = clause_with_heads[0].0.type_id.clone();
        let descriminants = clause_with_heads
            .iter()
            .map(|c| c.0.descriminant)
            .collect::<HashSet<_>>();
        let mut clauses = clause_with_heads
            .into_iter()
            .group_by(|c| c.0.descriminant)
            .into_iter()
            .map(|(descriminant, clause_with_heads)| {
                let arity = self.type_db.arity(&type_id, descriminant).unwrap();
                let tmp_vars = std::iter::repeat_with(|| self.gensym("v"))
                    .take(arity)
                    .collect::<Vec<_>>();
                let clauses = clause_with_heads
                    .into_iter()
                    .map(|(head, (mut pat, arm))| {
                        pat.extend(head.pattern.into_iter().rev());
                        (pat, arm)
                    })
                    .collect();
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_vars.clone().into_iter().rev());
                (
                    case::Pattern::Constructor {
                        descriminant,
                        data: tmp_vars,
                    },
                    self.match_compile(new_cond, clauses, default.clone()),
                )
            })
            .collect();
        if self.is_exhausitive(&type_id, descriminants) {
            if default.is_some() {
                panic!("redundant pattern")
            }
            case::Expr::Case {
                cond: Box::new(case::Expr::Symbol(c.clone())),
                clauses: clauses,
            }
        } else if let Some(default) = default {
            clauses.push((case::Pattern::Variable(self.gensym("_")), default));
            case::Expr::Case {
                cond: Box::new(case::Expr::Symbol(c.clone())),
                clauses: clauses,
            }
        } else {
            panic!("pattern is not exhausitive")
        }
    }

    fn match_compile_mixture(
        &mut self,
        cond: Stack<Symbol>,
        clauses: Vec<(Stack<match_::Pattern>, case::Expr)>,
        default: Option<case::Expr>,
    ) -> case::Expr {
        if clauses[0].0.last().unwrap().is_variable() {
            let pos = clauses
                .iter()
                .position(|(pat, _)| pat.last().unwrap().is_constructor())
                .unwrap();
            let (vars, other) = clauses.split_at(pos);
            let default = self.match_compile(cond.clone(), other.to_vec(), default);
            self.match_compile(cond, vars.to_vec(), Some(default))
        } else {
            let pos = clauses
                .iter()
                .position(|(pat, _)| pat.last().unwrap().is_variable())
                .unwrap();
            let (consts, other) = clauses.split_at(pos);
            let default = self.match_compile(cond.clone(), other.to_vec(), default);
            self.match_compile(cond, consts.to_vec(), Some(default))
        }
    }

    fn is_exhausitive(
        &self,
        type_id: &TypeId,
        descriminansts: impl IntoIterator<Item = u8>,
    ) -> bool {
        self.type_db.constructors(&type_id).unwrap()
            == descriminansts.into_iter().collect::<HashSet<_>>()
    }

    fn gensym(&mut self, hint: impl Into<String>) -> Symbol {
        self.symbol_generator.gensym(hint)
    }
}

struct CaseToSwitch {
    symbol_generator: SymbolGenerator,
}

impl CaseToSwitch {
    pub fn new(symbol_generator: SymbolGenerator) -> Self {
        Self { symbol_generator }
    }

    pub fn compile(&mut self, case: case::Expr) -> switch::Block {
        let (mut stmts, ret) = self.compile_expr(case);
        stmts.push(switch::Stmt::Ret(ret));
        switch::Block(stmts)
    }

    fn compile_expr(&mut self, case: case::Expr) -> (Vec<switch::Stmt>, Symbol) {
        match case {
            case::Expr::Let { var, expr, body } => self.compile_let(var, *expr, *body),
            case::Expr::Inject { descriminant, data } => self.compile_inject(descriminant, data),
            case::Expr::Case { cond, clauses } => self.compile_case(*cond, clauses),
            case::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_let(
        &mut self,
        var: Symbol,
        expr: case::Expr,
        body: case::Expr,
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
        data: Vec<case::Expr>,
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
        cond: case::Expr,
        clauses: Vec<(case::Pattern, case::Expr)>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Block, Op, Stmt};
        let mut default = None;
        let mut switch_clauses = Vec::new();
        let result_sym = self.gensym("switch_result");

        let (mut stmts, switch_cond) = self.compile_expr(cond);

        for (pat, arm) in clauses {
            let (switch_arm, symbol) = self.compile_expr(arm);
            match pat {
                case::Pattern::Variable(s) => {
                    let mut block = vec![Stmt::Assign(s.clone(), Op::Symbol(switch_cond.clone()))];
                    block.extend(switch_arm);
                    block.push(Stmt::Assign(result_sym.clone(), Op::Symbol(symbol)));
                    default = Some(Block(block))
                }
                case::Pattern::Constructor { descriminant, data } => {
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
    let mut sg = SymbolGenerator::new();
    let m = {
        use match_::*;
        use Expr::*;

        Case {
            cond: Box::new(Inject {
                descriminant: 2,
                data: vec![
                    Symbol(sg.gensym("*")),
                    Symbol(sg.gensym("*")),
                    Symbol(sg.gensym("*")),
                ],
            }),
            clauses: vec![
                (
                    Pattern::Constructor(Constructor {
                        type_id: TypeId::new("hoge"),
                        descriminant: 0,
                        pattern: vec![],
                    }),
                    Expr::Symbol(sg.gensym("*")),
                ),
                (
                    Pattern::Constructor(Constructor {
                        type_id: TypeId::new("hoge"),
                        descriminant: 1,
                        pattern: vec![Pattern::Variable(sg.gensym("x"))],
                    }),
                    Expr::Symbol(sg.gensym("*")),
                ),
                (
                    Pattern::Constructor(Constructor {
                        type_id: TypeId::new("hoge"),
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Constructor(Constructor {
                                type_id: TypeId::new("bool"),
                                descriminant: 0,
                                pattern: vec![],
                            }),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Variable(sg.gensym("z")),
                        ],
                    }),
                    Expr::Symbol(sg.gensym("*")),
                ),
                (
                    Pattern::Constructor(Constructor {
                        type_id: TypeId::new("hoge"),
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Constructor(Constructor {
                                type_id: TypeId::new("bool"),
                                descriminant: 0,
                                pattern: vec![],
                            }),
                            Pattern::Variable(sg.gensym("z")),
                        ],
                    }),
                    Expr::Symbol(sg.gensym("*")),
                ),
                (
                    Pattern::Constructor(Constructor {
                        type_id: TypeId::new("hoge"),
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Constructor(Constructor {
                                type_id: TypeId::new("bool"),
                                descriminant: 0,
                                pattern: vec![],
                            }),
                        ],
                    }),
                    Expr::Symbol(sg.gensym("*")),
                ),
                (
                    Pattern::Constructor(Constructor {
                        type_id: TypeId::new("hoge"),
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Variable(sg.gensym("z")),
                        ],
                    }),
                    Expr::Symbol(sg.gensym("*")),
                ),
            ],
        }
    };

    let mut p = PrettyPrinter::new();
    p.pp(&m);

    let mut type_db = match_::TypeDb::new();
    type_db.insert(
        TypeId::new("bool"),
        vec![
            match_::ConstructorType {
                descriminant: 0,
                params: vec![],
            },
            match_::ConstructorType {
                descriminant: 1,
                params: vec![],
            },
        ],
    );
    type_db.insert(
        TypeId::new("hoge"),
        vec![
            match_::ConstructorType {
                descriminant: 0,
                params: vec![],
            },
            match_::ConstructorType {
                descriminant: 1,
                params: vec![TypeId::new("bool")],
            },
            match_::ConstructorType {
                descriminant: 2,
                params: vec![
                    TypeId::new("bool"),
                    TypeId::new("bool"),
                    TypeId::new("bool"),
                ],
            },
        ],
    );
    let mut compiler = MatchToCase::new(sg.clone(), type_db);
    let c = compiler.compile(m);
    p.pp(&c);

    let mut compiler = CaseToSwitch::new(sg);
    let s = compiler.compile(c);

    p.pp(&s);
}
