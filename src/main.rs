mod pp;
use pp::{PrettyPrinter, PP};
#[derive(Debug, Clone)]
pub struct Symbol(String);

impl Symbol {
    pub fn new(s: impl Into<String>) -> Self {
        Symbol(s.into())
    }
}

pub struct Constant;

mod case {
    //! # Examples
    //! the source
    //!
    //! ```text
    //! case inj <2>(*, *, *, ) of
    //! |<0>() => *
    //! |<1>(x, ) => *
    //! |<2>(x, y, z, ) => *
    //! ```
    //!
    //! can be written as
    //!
    //! ``` rust
    //! Case {
    //!     cond: Box::new(Inject {
    //!         descriminant: 2,
    //!         data: vec![Const(Constant), Const(Constant), Const(Constant)],
    //!     }),
    //!     clauses: vec![
    //!         (
    //!             Pattern::Constructor {
    //!                 descriminant: 0,
    //!                 data: vec![],
    //!             },
    //!             Expr::Const(Constant),
    //!         ),
    //!         (
    //!             Pattern::Constructor {
    //!                 descriminant: 1,
    //!                 data: vec![Symbol::new("x")],
    //!             },
    //!             Expr::Const(Constant),
    //!         ),
    //!         (
    //!             Pattern::Constructor {
    //!                 descriminant: 2,
    //!                 data: vec![Symbol::new("x"), Symbol::new("y"), Symbol::new("z")],
    //!             },
    //!             Expr::Const(Constant),
    //!         ),
    //!     ],
    //! }
    //! ```
    //!

    use super::*;

    pub enum Expr {
        Inject {
            descriminant: u8,
            data: Vec<Expr>,
        },
        Case {
            cond: Box<Expr>,
            clauses: Vec<(Pattern, Expr)>,
        },
        Const(Constant),
    }

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
            case::Expr::Const(c) => self.compile_constant(c),
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
            let (mut stmts, d) = self.compile_expr(d);
            ret.append(&mut stmts);
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
            let (mut switch_arm, symbol) = self.compile_expr(arm);
            match pat {
                case::Pattern::Variable(s) => {
                    let mut block = vec![Stmt::Assign(s.clone(), Op::Symbol(switch_cond.clone()))];
                    block.append(&mut switch_arm);
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
                    block.append(&mut switch_arm);
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

    fn compile_constant(&mut self, _: Constant) -> (Vec<switch::Stmt>, Symbol) {
        let v = self.gensym("v");
        (
            vec![switch::Stmt::Assign(v.clone(), switch::Op::Const(0))],
            v,
        )
    }

    fn gensym(&mut self, hint: impl AsRef<str>) -> Symbol {
        // FIXME
        Symbol::new(hint.as_ref())
    }
}

fn main() {
    let expr = {
        use case::*;
        use Expr::*;

        Case {
            cond: Box::new(Inject {
                descriminant: 2,
                data: vec![Const(Constant), Const(Constant), Const(Constant)],
            }),
            clauses: vec![
                (
                    Pattern::Constructor {
                        descriminant: 0,
                        data: vec![],
                    },
                    Expr::Const(Constant),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 1,
                        data: vec![Symbol::new("x")],
                    },
                    Expr::Const(Constant),
                ),
                (
                    Pattern::Constructor {
                        descriminant: 2,
                        data: vec![Symbol::new("x"), Symbol::new("y"), Symbol::new("z")],
                    },
                    Expr::Const(Constant),
                ),
            ],
        }
    };

    let mut p = PrettyPrinter::new();
    p.pp(&expr);

    let tgt = {
        use crate::Symbol;
        use switch::*;
        use Op::*;
        use Stmt::*;;
        let des = Symbol::new("#des");
        let d1 = Symbol::new("#d1");
        let d2 = Symbol::new("#d2");
        let d3 = Symbol::new("#d3");
        let v1 = Symbol::new("#v1");
        let v2 = Symbol::new("#v2");
        let v3 = Symbol::new("#v3");
        let v4 = Symbol::new("#v4");
        let v5 = Symbol::new("#v5");
        let x1 = Symbol::new("#x1");
        let x2 = Symbol::new("#x2");
        let y = Symbol::new("#y");
        let z = Symbol::new("#z");
        let c0 = Block(vec![Assign(v3.clone(), Const(0)), Ret(v3)]);
        let c1 = Block(vec![
            Assign(
                x1,
                Load {
                    base: v1.clone(),
                    offset: 1,
                },
            ),
            Assign(v4.clone(), Const(0)),
            Ret(v4),
        ]);
        let c2 = Block(vec![
            Assign(
                x2,
                Load {
                    base: v1.clone(),
                    offset: 1,
                },
            ),
            Assign(
                y,
                Load {
                    base: v1.clone(),
                    offset: 2,
                },
            ),
            Assign(
                z,
                Load {
                    base: v1.clone(),
                    offset: 3,
                },
            ),
            Assign(v5.clone(), Const(0)),
            Ret(v5),
        ]);
        let default = Block(vec![Unreachable]);

        Block(vec![
            Assign(v1.clone(), Alloc { size: 4 }),
            Assign(des.clone(), Const(2)),
            Assign(d1.clone(), Const(0)),
            Assign(d2.clone(), Const(0)),
            Assign(d3.clone(), Const(0)),
            Store {
                base: v1.clone(),
                offset: 0,
                data: des.clone(),
            },
            Store {
                base: v1.clone(),
                offset: 1,
                data: d1.clone(),
            },
            Store {
                base: v1.clone(),
                offset: 2,
                data: d2.clone(),
            },
            Store {
                base: v1.clone(),
                offset: 3,
                data: d3.clone(),
            },
            Assign(
                v2.clone(),
                Load {
                    base: v1.clone(),
                    offset: 0,
                },
            ),
            Switch {
                cond: v2,
                targets: vec![(0, c0), (1, c1), (2, c2)],
                default: default,
            },
        ])
    };

    p.pp(&tgt);

    let mut compiler = CaseToSwitch::new();
    let tgt2 = compiler.compile(expr);

    p.pp(&tgt2);
}
