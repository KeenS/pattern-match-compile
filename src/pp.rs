use crate::Symbol;

pub struct PrettyPrinter {
    indent: usize,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self { indent: 0 }
    }

    pub fn indent(&mut self) {
        self.indent += 4;
    }

    pub fn indent_half(&mut self) {
        self.indent += 2;
    }

    pub fn dedent(&mut self) {
        self.indent -= 4;
    }

    pub fn dedent_half(&mut self) {
        self.indent -= 2;
    }
}

pub trait PP<T> {
    fn pp(&mut self, t: &T);
}

impl PP<Symbol> for PrettyPrinter {
    fn pp(&mut self, t: &Symbol) {
        print!("{}@{}", t.0, t.1)
    }
}

mod case {
    use super::*;
    use crate::case::*;

    impl PP<Expr> for PrettyPrinter {
        fn pp(&mut self, t: &Expr) {
            use Expr::*;
            match t {
                Tuple(ts) => {
                    print!("(");
                    for t in ts {
                        self.pp(t);
                        print!(", ");
                    }
                    print!(")");
                }
                Inject { discriminant, data } => {
                    print!("inj <{}>(", discriminant);
                    for d in data.as_ref().map(|d| &**d) {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                Case { cond, clauses, .. } => {
                    print!("case ");
                    self.pp(&**cond);
                    print!(" of\n");
                    self.indent();
                    for (pat, arm) in clauses {
                        print!("{: >1$} ", "|", self.indent);
                        self.pp(pat);
                        print!(" => ");
                        self.indent();
                        self.pp(arm);
                        self.dedent();
                        print!("\n");
                    }
                    self.dedent();
                }
                Symbol(s) => self.pp(s),
            }
        }
    }

    impl PP<Value> for PrettyPrinter {
        fn pp(&mut self, t: &Value) {
            match t {
                Value::Tuple(t) => {
                    print!("(");
                    for d in t {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                Value::Constructor {
                    discriminant,
                    value,
                } => {
                    print!("c <{}>(", discriminant);
                    for d in value.as_ref().map(|d| &**d) {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
            }
        }
    }

    impl PP<Pattern> for PrettyPrinter {
        fn pp(&mut self, t: &Pattern) {
            use Pattern::*;
            match t {
                Tuple(t) => {
                    print!("(");
                    for d in t {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                Constructor {
                    discriminant,
                    pattern,
                } => {
                    print!("<{}>(", discriminant);
                    for p in pattern.as_ref().map(|p| &**p) {
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
                Tuple(t) => {
                    print!("(");
                    for d in t {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                Let { var, expr, body } => {
                    print!("let ");
                    self.pp(var);
                    print!(" = ");
                    self.pp(&**expr);
                    print!(" in\n{: >1$}", "", self.indent);
                    self.pp(&**body)
                }
                Inject { discriminant, data } => {
                    print!("inj <{}>(", discriminant);
                    for d in data.as_ref().map(|d| &**d) {
                        self.pp(d);
                        print!(", ");
                    }
                    print!(")");
                }
                RaiseMatch => {
                    print!("raise Match");
                }
                RaiseFail => {
                    print!("raise Fail");
                }
                HandleFail { expr, handler } => {
                    self.pp(&**expr);
                    print!(" handle Fail =>");
                    self.indent();
                    self.pp(&**handler);
                    self.dedent();
                }
                Case { cond, clauses } => {
                    print!("case ");
                    self.pp(&**cond);
                    print!(" of\n");
                    self.indent();
                    for (pat, arm) in clauses {
                        print!("{: >1$} ", "|", self.indent);
                        self.pp(pat);
                        print!(" => ");
                        self.indent();
                        self.pp(arm);
                        self.dedent();
                        print!("\n");
                    }
                    self.dedent();
                }
                Symbol(s) => self.pp(s),
            }
        }
    }

    impl PP<Pattern> for PrettyPrinter {
        fn pp(&mut self, t: &Pattern) {
            use Pattern::*;
            match t {
                Tuple(t) => {
                    print!("(");
                    for s in t {
                        self.pp(s);
                        print!(", ");
                    }
                    print!(")");
                }
                Constructor { discriminant, data } => {
                    print!("<{}>(", discriminant);
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
            self.indent();
            for stmt in &t.0 {
                self.pp(stmt);
            }
            self.dedent();
            print!("{: >1$}", "", self.indent);
            print!("}}\n");
        }
    }

    impl PP<Stmt> for PrettyPrinter {
        fn pp(&mut self, t: &Stmt) {
            use Stmt::*;
            match t {
                Assign(s, op) => {
                    print!("{: >1$}", "", self.indent);
                    self.pp(s);
                    print!(" := ");
                    self.pp(op);
                    print!(";\n");
                }
                Store { base, offset, data } => {
                    print!("{: >1$}", "", self.indent);
                    print!("store(");
                    self.pp(base);
                    print!(" + {}, ", offset);
                    self.pp(data);
                    print!(")");
                    print!(";\n");
                }
                Switch {
                    cond,
                    targets,
                    default,
                } => {
                    print!("{: >1$}", "", self.indent);
                    print!("switch(");
                    self.pp(cond);
                    print!(") {{\n");
                    self.indent();
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
                    self.dedent();
                    print!("{: >1$}", "", self.indent);
                    print!("}}");
                    print!(";\n");
                }
                Label(label) => {
                    self.dedent_half();
                    print!("{: >1$}", "", self.indent);
                    self.pp(label);
                    print!(":\n");
                    self.indent_half();
                }
                RaiseMatch => {
                    print!("{: >1$}", "", self.indent);
                    print!("sml_raise(SML_EXN_MATCH);\n");
                }
                Goto(label) => {
                    print!("{: >1$}", "", self.indent);
                    print!("goto ");
                    self.pp(label);
                    print!(";\n");
                }
                Unreachable => {
                    print!("{: >1$}", "", self.indent);
                    print!("UNREACHABLE");
                    print!(";\n");
                }
                Ret(s) => {
                    print!("{: >1$}", "", self.indent);
                    print!("return ");
                    self.pp(s);
                    print!(";\n");
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
