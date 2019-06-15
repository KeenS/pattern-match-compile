use crate::Symbol;

pub struct PrettyPrinter {
    indent: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self { indent: 0 }
    }
}

pub trait PP<T> {
    fn pp(&mut self, t: &T);
}

impl PP<Symbol> for PrettyPrinter {
    fn pp(&mut self, t: &Symbol) {
        print!("{}@{}", t.1, t.0)
    }
}

mod case {
    use super::*;
    use crate::case::*;

    impl PP<Expr> for PrettyPrinter {
        fn pp(&mut self, t: &Expr) {
            use Expr::*;
            match t {
                Inject { descriminant, data } => {
                    print!("inj <{}>(", descriminant);
                    for d in data {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                Case { cond, clauses, .. } => {
                    print!("case ");
                    self.pp(&**cond);
                    print!(" of\n");
                    self.indent += 4;
                    for (pat, arm) in clauses {
                        print!("{: >1$} ", "|", self.indent);
                        self.pp(pat);
                        print!(" => ");
                        self.indent += 4;
                        self.pp(arm);
                        self.indent -= 4;
                        print!("\n");
                    }
                    self.indent -= 4;
                }
                Symbol(s) => self.pp(s),
            }
        }
    }

    impl PP<Value> for PrettyPrinter {
        fn pp(&mut self, t: &Value) {
            let Value { descriminant, data } = t;
            print!("c <{}>(", descriminant);
            for d in data {
                self.pp(d);
                print!(", ");
            }
            print!(")");
        }
    }

    impl PP<Pattern> for PrettyPrinter {
        fn pp(&mut self, t: &Pattern) {
            use Pattern::*;
            match t {
                Constructor(c) => {
                    print!("<{}>(", c.descriminant);
                    for p in &c.pattern {
                        self.pp(p);
                        print!(", ");
                    }
                    print!(")");
                }
                Variable(s) => self.pp(s),
            }
        }
    }

}

mod simple_case {
    use super::*;
    use crate::simple_case::*;

    impl PP<Expr> for PrettyPrinter {
        fn pp(&mut self, t: &Expr) {
            use Expr::*;
            match t {
                Let { var, expr, body } => {
                    print!("let ");
                    self.pp(var);
                    print!(" = ");
                    self.pp(&**expr);
                    print!(" in\n{: >1$}", "", self.indent);
                    self.pp(&**body)
                }
                Inject { descriminant, data } => {
                    print!("inj <{}>(", descriminant);
                    for d in data {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                Case { cond, clauses } => {
                    print!("case ");
                    self.pp(&**cond);
                    print!(" of\n");
                    self.indent += 4;
                    for (pat, arm) in clauses {
                        print!("{: >1$} ", "|", self.indent);
                        self.pp(pat);
                        print!(" => ");
                        self.indent += 4;
                        self.pp(arm);
                        self.indent -= 4;
                        print!("\n");
                    }
                    self.indent -= 4;
                }
                Symbol(s) => self.pp(s),
            }
        }
    }

    impl PP<Pattern> for PrettyPrinter {
        fn pp(&mut self, t: &Pattern) {
            use Pattern::*;
            match t {
                Constructor { descriminant, data } => {
                    print!("<{}>(", descriminant);
                    for s in data {
                        self.pp(s);
                        print!(", ");
                    }
                    print!(")");
                }
                Variable(s) => self.pp(s),
            }
        }
    }

}

mod switch {
    use super::*;
    use crate::switch::*;

    impl PP<Block> for PrettyPrinter {
        fn pp(&mut self, t: &Block) {
            print!("{{\n");
            self.indent += 4;
            for stmt in &t.0 {
                print!("{: >1$}", "", self.indent);
                self.pp(stmt);
                print!(";\n");
            }
            self.indent -= 4;
            print!("{: >1$}", "", self.indent);
            print!("}}\n");
        }
    }

    impl PP<Stmt> for PrettyPrinter {
        fn pp(&mut self, t: &Stmt) {
            use Stmt::*;
            match t {
                Assign(s, op) => {
                    self.pp(s);
                    print!(" := ");
                    self.pp(op);
                }
                Store { base, offset, data } => {
                    print!("store(");
                    self.pp(base);
                    print!(" + {}, ", offset);
                    self.pp(data);
                    print!(")")
                }
                Switch {
                    cond,
                    targets,
                    default,
                } => {
                    print!("switch(");
                    self.pp(cond);
                    print!(") {{\n");
                    self.indent += 4;
                    for (label, block) in targets {
                        print!("{: >1$}", "", self.indent);
                        print!("case {}: ", label);
                        self.pp(block);
                        print!("\n")
                    }
                    print!("{: >1$}", "", self.indent);
                    print!("default: ");
                    self.pp(default);
                    print!("\n");
                    self.indent -= 4;
                    print!("{: >1$}", "", self.indent);
                    print!("}}");
                }
                Unreachable => print!("UNREACHABLE"),
                Ret(s) => {
                    print!("return ");
                    self.pp(s)
                }
            }
        }
    }

    impl PP<Op> for PrettyPrinter {
        fn pp(&mut self, t: &Op) {
            use Op::*;
            match t {
                Alloc { size } => {
                    print!("alloc({})", size);
                }
                Load { base, offset } => {
                    print!("load(");
                    self.pp(base);
                    print!(", {})", offset);
                }
                Symbol(s) => self.pp(s),
                Const(i) => print!("{}", i),
            }
        }
    }

}
