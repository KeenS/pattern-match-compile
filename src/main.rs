mod pp;
use itertools::Itertools;
use pp::{PrettyPrinter, PP};
#[derive(Debug, Clone)]
pub struct Symbol(String);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId(u32);

impl Symbol {
    pub fn new(s: impl Into<String>) -> Self {
        Symbol(s.into())
    }
}

mod match_ {
    use super::*;

    #[derive(Debug, Clone)]
    pub enum Expr {
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
        Constructor {
            descriminant: u8,
            pattern: Vec<Pattern>,
        },
        Variable(Symbol),
    }

    impl Pattern {
        pub fn is_constructor(&self) -> bool {
            use Pattern::*;
            match self {
                Constructor { .. } => true,
                Variable(_) => false,
            }
        }

        pub fn is_variable(&self) -> bool {
            use Pattern::*;
            match self {
                Constructor { .. } => false,
                Variable(_) => true,
            }
        }
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

struct MatchToCase;

type Stack<T> = Vec<T>;

impl MatchToCase {
    pub fn new() -> Self {
        Self
    }

    pub fn compile(&mut self, match_: match_::Expr) -> case::Expr {
        match match_ {
            match_::Expr::Inject { descriminant, data } => self.compile_inject(descriminant, data),
            match_::Expr::Case { cond, clauses } => self.compile_match(*cond, clauses),
            match_::Expr::Symbol(s) => self.compile_symbol(s),
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
        let v = self.gensym("v");
        case::Expr::Case {
            cond: Box::new(cond),
            clauses: vec![(
                case::Pattern::Variable(v.clone()),
                self.match_compile(vec![v], clauses, None),
            )],
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
                let p = match pat.pop().unwrap() {
                    match_::Pattern::Variable(s) => case::Pattern::Variable(s),
                    _ => unreachable!(),
                };
                (
                    pat,
                    case::Expr::Case {
                        cond: Box::new(case::Expr::Symbol(c.clone())),
                        clauses: vec![(p, arm)],
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
        let clause_groups = clauses
            .into_iter()
            .map(|(mut pat, arm)| {
                let p = pat.pop().unwrap();
                let (des, pattern) = match p {
                    match_::Pattern::Constructor {
                        descriminant,
                        pattern,
                    } => (descriminant, pattern),
                    _ => unreachable!(),
                };
                (des, pattern, pat, arm)
            })
            .group_by(|(des, _, _, _)| *des);
        let clause_groups = clause_groups.into_iter().collect::<Vec<_>>();
        let descriminants = clause_groups
            .iter()
            .map(|(des, _)| *des)
            .collect::<Vec<_>>();
        let mut clauses = clause_groups
            .into_iter()
            .map(|(descriminant, clauses)| {
                let mut npatterns = 0;
                let clauses = clauses
                    .into_iter()
                    .map(|(_, mut patterns, pat, arm)| {
                        npatterns = patterns.len();
                        patterns.extend(pat);
                        (patterns, arm)
                    })
                    .collect();
                let symbols = std::iter::repeat_with(|| self.gensym("v"))
                    .take(npatterns)
                    .collect::<Vec<_>>();
                let pattern = case::Pattern::Constructor {
                    descriminant,
                    data: symbols.clone(),
                };
                let mut new_cond = cond.clone();
                new_cond.extend(symbols.into_iter().rev());
                (
                    pattern,
                    self.match_compile(new_cond, clauses, default.clone()),
                )
            })
            .collect();
        // if descriminants.is_exhausitive() {
        if true {
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
                .position(|(pat, _)| pat[0].is_variable())
                .unwrap();
            let (consts, other) = clauses.split_at(pos);
            let default = self.match_compile(cond.clone(), other.to_vec(), default);
            self.match_compile(cond, consts.to_vec(), Some(default))
        }
    }

    fn gensym(&mut self, hint: impl AsRef<str>) -> Symbol {
        // FIXME
        Symbol::new(hint.as_ref())
    }
}

struct CaseToSwitch;

impl CaseToSwitch {
    pub fn new() -> Self {
        Self
    }

    pub fn compile(&mut self, case: case::Expr) -> switch::Block {
        let (mut stmts, ret) = self.compile_expr(case);
        stmts.push(switch::Stmt::Ret(ret));
        switch::Block(stmts)
    }

    fn compile_expr(&mut self, case: case::Expr) -> (Vec<switch::Stmt>, Symbol) {
        match case {
            case::Expr::Inject { descriminant, data } => self.compile_inject(descriminant, data),
            case::Expr::Case { cond, clauses } => self.compile_case(*cond, clauses),
            case::Expr::Symbol(s) => self.compile_symbol(s),
        }
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

    fn gensym(&mut self, hint: impl AsRef<str>) -> Symbol {
        // FIXME
        Symbol::new(hint.as_ref())
    }
}

fn main() {
    use crate::Symbol as S;

    let m = {
        use match_::*;
        use Expr::*;

        Case {
            cond: Box::new(Inject {
                descriminant: 2,
                data: vec![
                    Symbol(S::new("*")),
                    Symbol(S::new("*")),
                    Symbol(S::new("*")),
                ],
            }),
            clauses: vec![
                (
                    Pattern::Constructor {
                        descriminant: 0,
                        pattern: vec![],
                    },
                    Expr::Symbol(S::new("*")),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 1,
                        pattern: vec![Pattern::Variable(S::new("x"))],
                    },
                    Expr::Symbol(S::new("*")),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Constructor {
                                descriminant: 0,
                                pattern: vec![],
                            },
                            Pattern::Variable(S::new("y")),
                            Pattern::Variable(S::new("z")),
                        ],
                    },
                    Expr::Symbol(S::new("*")),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(S::new("x")),
                            Pattern::Constructor {
                                descriminant: 0,
                                pattern: vec![],
                            },
                            Pattern::Variable(S::new("z")),
                        ],
                    },
                    Expr::Symbol(S::new("*")),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(S::new("x")),
                            Pattern::Variable(S::new("y")),
                            Pattern::Constructor {
                                descriminant: 0,
                                pattern: vec![],
                            },
                        ],
                    },
                    Expr::Symbol(S::new("*")),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        pattern: vec![
                            Pattern::Variable(S::new("x")),
                            Pattern::Variable(S::new("y")),
                            Pattern::Variable(S::new("z")),
                        ],
                    },
                    Expr::Symbol(S::new("*")),
                ),
            ],
        }
    };

    let mut p = PrettyPrinter::new();
    p.pp(&m);

    let mut compiler = MatchToCase::new();
    let c = compiler.compile(m);
    p.pp(&c);

    let mut compiler = CaseToSwitch::new();
    let s = compiler.compile(c);

    p.pp(&s);
}
