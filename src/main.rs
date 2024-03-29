mod pp;
use pp::{PrettyPrinter, PP};
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
/// 名前を表わすデータ型
pub struct Symbol(String, u32);

#[derive(Debug, Clone)]
pub struct SymbolGenerator(Rc<Cell<u32>>);
impl SymbolGenerator {
    pub fn new() -> Self {
        Self(Rc::new(Cell::new(0)))
    }

    /// 名前のヒントを受け取って既存のシンボルと衝突しないシンボルを生成する
    pub fn gensym(&mut self, hint: impl Into<String>) -> Symbol {
        let id = self.0.get();
        self.0.set(id + 1);
        Symbol(hint.into(), id)
    }

    /// n個のシンボルを生成する
    pub fn gennsyms(&mut self, hint: impl Into<String>, n: usize) -> Vec<Symbol> {
        let hint = hint.into();
        std::iter::repeat_with(|| self.gensym(hint.clone()))
            .take(n)
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// 型を表わすデータ型
pub struct TypeId(String);

impl TypeId {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }
}

mod case {
    use super::*;

    /// 式
    #[derive(Debug, Clone)]
    pub enum Expr {
        /// (expr1, expr2, ...)
        Tuple(Vec<Expr>),
        /// inj<discriminant>(data)
        Inject {
            discriminant: u8,
            data: Option<Box<Expr>>,
        },
        /// case cond of pattern1 => expr1 | pattern2 => expr2 ...
        Case {
            cond: Box<Expr>,
            ty: TypeId,
            clauses: Vec<(Pattern, Expr)>,
        },
        /// x
        Symbol(Symbol),
    }

    /// パターン
    #[derive(Debug, Clone)]
    pub enum Pattern {
        /// (pat1, pat2, ...)
        Tuple(Vec<Pattern>),
        /// c<discriminant>(pattern)
        Constructor {
            discriminant: u8,
            pattern: Option<Box<Pattern>>,
        },
        /// x
        Variable(Symbol),
    }

    /// 値
    #[derive(Debug, Clone)]
    pub enum Value {
        /// (value1, value2, ...)
        Tuple(Vec<Value>),
        /// c<discriminant>(value)
        Constructor {
            discriminant: u8,
            value: Option<Box<Value>>,
        },
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum PatternType {
        Tuple,
        Constructor,
        Variable,
    }

    impl Pattern {
        pub fn pattern_type(&self) -> PatternType {
            use PatternType::*;
            match self {
                Pattern::Tuple(_) => Tuple,
                Pattern::Constructor { .. } => Constructor,
                Pattern::Variable(_) => Variable,
            }
        }

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
                    discriminant,
                    pattern,
                } => (discriminant, pattern.map(|p| *p)),
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
        pub discriminant: u8,
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
        /// コンストラクタ
        pub fn new() -> Self {
            Self(HashMap::new())
        }

        /// タプル型の登録
        pub fn register_tuple(&mut self, type_id: TypeId, tys: impl IntoIterator<Item = TypeId>) {
            self.0
                .insert(type_id, Type::Tuple(tys.into_iter().collect()));
        }

        /// 代数的データ型の登録
        pub fn register_adt(
            &mut self,
            type_id: TypeId,
            constructors: impl IntoIterator<Item = Constructor>,
        ) {
            self.0
                .insert(type_id, Type::Adt(constructors.into_iter().collect()));
        }

        /// 参照
        pub fn find(&self, type_id: &TypeId) -> Option<&Type> {
            self.0.get(type_id)
        }

        pub fn param_ty_of(&self, type_id: &TypeId, discriminant: u8) -> Option<TypeId> {
            self.find(type_id)
                .cloned()
                .unwrap()
                .adt()
                .into_iter()
                .find(|c| c.discriminant == discriminant)
                .map(|c| c.param)
                .unwrap()
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
        // どの式かでパターンマッチ
        match expr {
            // タプルならそれぞれの要素を評価したあとにタプルを作る
            Tuple(tuple) => tuple
                .into_iter()
                // case::Expr -> Result<case::Value, Match>
                .map(|e| self.eval(e))
                // 普通に集めると Vec<Result<case::Value, Match>> だが、
                // Rustは注釈を与えればResultとVecを入れ替えられる
                .collect::<Result<Vec<_>, Match>>()
                .map(case::Value::Tuple),
            // `case` 式ならパターンマッチをする（後述）
            Case { cond, clauses, .. } => {
                let cond = self.eval(*cond)?;
                let ret = clauses
                    .into_iter()
                    .map(|(pat, arm)| self.pattern_match(pat, cond.clone()).map(|()| arm))
                    .fold(Err(Fail), |acc, current| acc.or(current));
                match ret {
                    Ok(e) => self.eval(e),
                    Err(Fail) => Err(Match),
                }
            }
            // シンボルはスコープから名前を解決する
            Symbol(s) => Ok(self.resolve(&s)),
            // コンストラクタなら引数があれば評価し、値を作る
            Inject { discriminant, data } => Ok(case::Value::Constructor {
                discriminant,
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
                    discriminant: c1,
                    pattern,
                },
                case::Value::Constructor {
                    discriminant: c2,
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

    /// 式
    #[derive(Debug, Clone)]
    pub enum Expr {
        /// (expr1, expr2, ...)
        Tuple(Vec<Expr>),
        /// let val var = expr in body end
        Let {
            var: Symbol,
            expr: Box<Expr>,
            body: Box<Expr>,
        },
        /// inject data with discriminant
        Inject {
            discriminant: u8,
            data: Option<Box<Expr>>,
        },
        /// case cond of pattern1 => expr1 | pattern2 => expr2 ...
        Case {
            cond: Box<Expr>,
            clauses: Vec<(Pattern, Expr)>,
        },
        /// raise Match;
        RaiseMatch,
        /// raise Fail;
        RaiseFail,
        /// expr handle Fail => handler
        HandleFail { expr: Box<Expr>, handler: Box<Expr> },
        /// x
        Symbol(Symbol),
    }

    /// パターン
    #[derive(Debug, Clone)]
    pub enum Pattern {
        /// (pat1, pat2, ...)
        Tuple(Vec<Symbol>),
        /// constructor of discriminant and its data
        Constructor {
            discriminant: u8,
            data: Option<Symbol>,
        },
        /// x
        Variable(Symbol),
    }
}

mod switch {
    use super::*;

    /// ブロック（文のかたまり）
    pub struct Block(pub Vec<Stmt>);

    /// 文
    pub enum Stmt {
        /// x := op;
        Assign(Symbol, Op),
        /// store(base+offset, data);
        Store {
            base: Symbol,
            offset: u8,
            data: Symbol,
        },
        /// switch(cond) {
        ///   target: {
        ///     block
        ///   }
        ///   ...
        ///   defalut: {
        ///     default
        ///   }
        /// };
        Switch {
            cond: Symbol,
            targets: Vec<(i32, Block)>,
            default: Block,
        },
        /// label:
        Label(Symbol),
        /// sml_raise(SML_EXN_MATCH);
        RaiseMatch,
        /// goto label;
        Goto(Symbol),
        /// UNREACHABLE;
        Unreachable,
        /// return x;
        Ret(Symbol),
    }

    /// 操作。x := の右側に書けるもの。
    pub enum Op {
        /// alloc(size);
        Alloc { size: u8 },
        /// load(base+offset);
        Load { base: Symbol, offset: u8 },
        /// x
        Symbol(Symbol),
        /// 1
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
            case::Expr::Inject { discriminant, data } => {
                self.compile_inject(discriminant, data.map(|d| *d))
            }
            case::Expr::Case { cond, ty, clauses } => self.compile_case(*cond, ty, clauses),
            case::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_tuple(&mut self, data: Vec<case::Expr>) -> simple_case::Expr {
        simple_case::Expr::Tuple(data.into_iter().map(|d| self.compile(d)).collect())
    }

    fn compile_inject(&mut self, discriminant: u8, data: Option<case::Expr>) -> simple_case::Expr {
        simple_case::Expr::Inject {
            discriminant,
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
    ) -> simple_case::Expr {
        // assuming clauses.any(|(patterns, _)| patterns.len() == cond.len())
        if cond.is_empty() {
            self.compile_empty(cond, clauses)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_variable())
        {
            self.compile_variable(cond, clauses)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_tuple())
        {
            self.compile_tuple(cond, clauses)
        } else if clauses
            .iter()
            .all(|(patterns, _)| patterns.last().unwrap().is_constructor())
        {
            self.compile_constructor(cond, clauses)
        } else {
            self.compile_mixture(cond, clauses)
        }
    }

    fn compile_empty(
        &mut self,
        _: Stack<(Symbol, TypeId)>,
        mut clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        clauses.remove(0).1
    }

    fn compile_variable(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        // 条件変数の先頭と、パターン行列の1列目を処理する
        let (sym, _) = cond.pop().unwrap();
        let clauses = clauses
            // 各行の
            .into_iter()
            .map(|(mut pat, arm)| {
                // 1列目にある変数パターンをとりだす
                let var = pat.pop().unwrap().variable();
                // 変数をletにする
                let arm = simple_case::Expr::Let {
                    expr: Box::new(simple_case::Expr::Symbol(sym.clone())),
                    var,
                    body: Box::new(arm),
                };
                (pat, arm)
            })
            .collect();
        // 再帰処理
        self.compile(cond, clauses)
    }

    fn compile_tuple(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let (sym, ty) = cond.pop().unwrap();

        // タプル型の分解
        let param_tys: Vec<TypeId> = self.type_db.find(&ty).cloned().unwrap().tuple();
        // 一時変数の生成
        let tmp_vars = self.symbol_generator.gennsyms("v", param_tys.len());
        // タプルを分解したあとの条件部分の変数。
        let mut new_cond = cond;
        // もとの条件変数スタックと平滑化したタプルの変数をあわせる。
        // スタック構造なので逆順に値を入れていることに注意
        new_cond.extend(tmp_vars.iter().cloned().zip(param_tys).rev());

        let clauses = clauses
            // 各行の
            .into_iter()
            .map(|(mut patterns, arm)| {
                // 1列目にあるタプルパターンをとりだす
                let vars = patterns.pop().unwrap().tuple();
                // 平滑化する
                patterns.extend(vars.into_iter().rev());
                (patterns, arm)
            })
            .collect();
        // 全体は `case` 式になる
        simple_case::Expr::Case {
            cond: Box::new(simple_case::Expr::Symbol(sym)),
            clauses: vec![(
                simple_case::Pattern::Tuple(tmp_vars),
                // 再帰処理
                self.compile(new_cond, clauses),
            )],
        }
    }

    fn compile_constructor(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let (sym, ty) = cond.pop().unwrap();
        // 各節をパターンの先頭とそれ以外に分解。
        // 先頭は全てコンストラクタパターン。
        let clause_with_heads = clauses
            // 各行の
            .into_iter()
            .map(|mut clause| {
                // 1列目にあるコンストラクタパターンをとりだす
                let head = clause.0.pop().unwrap().constructor();
                (head, clause)
            })
            .collect::<Vec<_>>();
        // 先頭のパターンの判別子の集合をとる。
        let discriminants = clause_with_heads
            .iter()
            .map(|c| (c.0).0)
            .collect::<HashSet<_>>();
        // discriminantごとに特殊化する
        let mut clauses = discriminants
            .iter()
            .map(|&discriminant| {
                // パターン行列をdiscriminantで特殊化したものを取得
                let clauses = self.specialize(discriminant, clause_with_heads.iter());
                // Type DBから今パターンマッチしている型の、
                // 対象にしているdiscriminantをもつヴァリアントの、
                // 引数があればその型を取得する
                let param_ty = self.type_db.param_ty_of(&ty, discriminant);
                // 引数があれば一時変数を生成し、条件変数に追加する
                let tmp_var = param_ty.as_ref().map(|_| self.symbol_generator.gensym("v"));
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_var.iter().cloned().zip(param_ty).rev());
                // 返り値はコンストラクタパターンと、それにマッチしたあとの腕の組
                let pat = simple_case::Pattern::Constructor {
                    discriminant,
                    data: tmp_var,
                };
                let arm = self.compile(new_cond, clauses);
                (pat, arm)
            })
            .collect();

        // ヴァリアントが網羅的かどうかで挙動を変える。
        if self.is_exhausitive(&ty, discriminants) {
            // 網羅的ならそのまま `case` 式の生成
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses,
            }
        } else {
            // 網羅的でないならパターンマッチの末尾に `_ => raise Fail` を加える
            clauses.push((
                simple_case::Pattern::Variable(self.symbol_generator.gensym("_")),
                simple_case::Expr::RaiseFail,
            ));

            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses,
            }
        }
    }

    fn compile_mixture(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        // 先頭のパターンと違うものが出現した箇所でパターン行列を分割する
        let head_pattern_type = clauses[0].0.last().unwrap().pattern_type();
        let pos = clauses
            .iter()
            .position(|(pat, _)| pat.last().unwrap().pattern_type() != head_pattern_type)
            .unwrap();
        let (clauses, other) = clauses.split_at(pos);
        // 分割した残りの方をfallbackとし、コンパイルしておく。
        let fallback = self.compile(cond.clone(), other.to_vec());
        // 再帰コンパイル
        let expr = self.compile(cond, clauses.to_vec());

        // e1 handle Fail => e2
        simple_case::Expr::HandleFail {
            expr: Box::new(expr),
            handler: Box::new(fallback),
        }
    }

    fn specialize<'a, 'b>(
        &'a mut self,
        discriminant: u8,
        clause_with_heads: impl Iterator<
            Item = &'b (
                (u8, Option<case::Pattern>),
                (Stack<case::Pattern>, simple_case::Expr),
            ),
        >,
    ) -> Vec<(Stack<case::Pattern>, simple_case::Expr)> {
        // 先頭のパターンが指定された判別子に合致する節をあつめてくる
        clause_with_heads
            .filter(|(head, _)| head.0 == discriminant)
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
        discriminansts: impl IntoIterator<Item = u8>,
    ) -> bool {
        // 判別子の集合がパターンマッチしている型のヴァリアント全ての集合と合致するかで検査
        self.type_db
            .find(&type_id)
            .cloned()
            .unwrap()
            .adt()
            .into_iter()
            .map(|c| c.discriminant)
            .collect::<HashSet<_>>()
            == discriminansts.into_iter().collect::<HashSet<_>>()
    }
}

impl PatternCompiler for BackTrackPatternCompiler {
    fn compile(
        &mut self,
        cond: Vec<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let expr = self.compile(cond, clauses);
        // 拾いきれなかったFail(=パターンの不足)を拾ってMatch例外にする
        simple_case::Expr::HandleFail {
            expr: Box::new(expr),
            handler: Box::new(simple_case::Expr::RaiseMatch),
        }
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
        if clauses.is_empty() {
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
        simple_case::Expr::RaiseMatch
    }

    fn compile_variable(
        &mut self,
        cond: Stack<(Symbol, TypeId)>,
        mut clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        // 先頭行をとりだす
        let (patterns, expr) = clauses.remove(0);
        patterns
            // 全ての列に対して
            .into_iter()
            // 変数をしてとりだして
            .map(|p| p.variable())
            // let val p = c in ... end を作る
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
        mut clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        let pos = self.find_nonvar(&clauses);
        let top = cond.len() - 1;

        // 非変数のパターンをさがして先頭にもってくる
        cond.swap(top, pos);
        for clause in &mut clauses {
            clause.0.swap(top, pos);
        }

        // 先頭にもってきたパターンで場合分け
        if clauses[0].0[top].is_tuple() {
            self.compile_tuple(cond, clauses)
        } else {
            self.compile_constructor(cond, clauses)
        }
    }

    fn compile_tuple(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        // 条件変数の先頭を取り出す
        let (sym, ty) = cond.pop().unwrap();

        // タプルのそれぞれの型を取得
        let param_tys: Vec<TypeId> = self.type_db.find(&ty).cloned().unwrap().tuple();
        let clauses = clauses
            // それぞれの行の
            .into_iter()
            .map(|(mut patterns, mut arm)| {
                // 先頭のパターンに対して、
                let tuple = match patterns.pop().unwrap() {
                    // タプルパターンならその場に展開し、
                    case::Pattern::Tuple(t) => t,
                    // 変数パターンならタプルの要素数と同じ数の `_` を生成する。
                    case::Pattern::Variable(var) => {
                        let pattern = self
                            .symbol_generator
                            .gennsyms("_", param_tys.len())
                            .into_iter()
                            .map(case::Pattern::Variable)
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

        // パターン行列の処理が終わったら条件変数の方にもn個の変数を入れる
        let tmp_vars = self.symbol_generator.gennsyms("v", param_tys.len());
        cond.extend(tmp_vars.iter().cloned().zip(param_tys.clone()).rev());
        // 条件変数に追加したものと同じ変数で `case cond of (v1, v2, ...) => ...` を作る
        simple_case::Expr::Case {
            cond: Box::new(simple_case::Expr::Symbol(sym)),
            clauses: vec![(
                simple_case::Pattern::Tuple(tmp_vars),
                self.compile(cond, clauses),
            )],
        }
    }

    fn compile_constructor(
        &mut self,
        mut cond: Stack<(Symbol, TypeId)>,
        clauses: Vec<(Stack<case::Pattern>, simple_case::Expr)>,
    ) -> simple_case::Expr {
        // 条件変数の先頭を取り出す
        let (sym, ty) = cond.pop().unwrap();

        // 各行の先頭パターンだけ使うことがよくあるので最初に取り出しておく
        let clause_with_heads = clauses
            .into_iter()
            .map(|mut clause| {
                let head = clause.0.pop().unwrap();
                (head, clause)
            })
            .collect::<Vec<_>>();
        // 1. 先頭パターンのコンストラクタの判別子を集めてくる
        let discriminants = clause_with_heads
            .iter()
            .filter_map(|(head, _)| match head {
                case::Pattern::Constructor { discriminant, .. } => Some(*discriminant),
                _ => None,
            })
            .collect::<HashSet<_>>();
        // 2. 判別子毎に特殊化行列を作る
        let mut clauses = discriminants
            .iter()
            .map(|&discriminant| {
                let clauses = self.specialized_patterns(
                    sym.clone(),
                    &ty,
                    discriminant,
                    clause_with_heads.iter(),
                );
                // 本来ならコンストラクタごとに、引数の有無で処理が微妙に変わる。
                // そこを `Option` 型のメソッドで違いを吸収している。
                let param_ty = self.type_db.param_ty_of(&ty, discriminant);
                let tmp_var = param_ty.clone().map(|_| self.symbol_generator.gensym("v"));
                let mut new_cond = cond.clone();
                new_cond.extend(tmp_var.iter().cloned().zip(param_ty).rev());
                let pat = simple_case::Pattern::Constructor {
                    discriminant,
                    data: tmp_var,
                };
                let arm = self.compile(new_cond, clauses);
                (pat, arm)
            })
            .collect();

        // 3. コンストラクタが網羅的でなければデフォルト行列を作る
        if self.is_exhausitive(&ty, discriminants) {
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses,
            }
        } else {
            let default = self.default_patterns(sym.clone(), clause_with_heads.iter());
            clauses.push((
                simple_case::Pattern::Variable(self.symbol_generator.gensym("_")),
                self.compile(cond, default),
            ));
            simple_case::Expr::Case {
                cond: Box::new(simple_case::Expr::Symbol(sym.clone())),
                clauses,
            }
        }
    }

    fn find_nonvar(&mut self, clauses: &[(Stack<case::Pattern>, simple_case::Expr)]) -> usize {
        // ベクトルをスタックの代用としているので先頭から探索するには rpositionを使う
        clauses[0].0.iter().rposition(|p| !p.is_variable()).unwrap()
    }

    fn specialized_patterns<'a, 'b>(
        &'a mut self,
        cond: Symbol,
        type_id: &TypeId,
        discriminant: u8,
        clause_with_heads: impl Iterator<
            Item = &'b (case::Pattern, (Stack<case::Pattern>, simple_case::Expr)),
        >,
    ) -> Vec<(Stack<case::Pattern>, simple_case::Expr)> {
        let param_ty = self.type_db.param_ty_of(&type_id, discriminant);
        clause_with_heads
            .filter_map(|(head, clause)| match head {
                // 判別子が一致するコンストラクタパターンはそのままあつめる
                case::Pattern::Constructor {
                    discriminant: d,
                    pattern,
                } if *d == discriminant => Some((pattern.clone().map(|p| *p), clause.clone())),
                // 変数パターンは無条件で集めるが、
                // 腕やパターンに小細工が必要
                case::Pattern::Variable(var) => {
                    let extra_pattern = if param_ty.is_some() {
                        let var = self.symbol_generator.gensym("_");
                        Some(case::Pattern::Variable(var))
                    } else {
                        None
                    };

                    // pattern => arm を pattern => let val var = c in armに変換
                    let (pat, arm) = clause.clone();
                    let arm = simple_case::Expr::Let {
                        expr: Box::new(simple_case::Expr::Symbol(cond.clone())),
                        var: var.clone(),
                        body: Box::new(arm),
                    };
                    Some((extra_pattern, (pat, arm)))
                }
                // discriminantが一致しないコンストラクタは無視
                _ => None,
            })
            .map(|(extra_pattern, (mut pat, arm))| {
                if let Some(p) = extra_pattern {
                    pat.push(p)
                }
                (pat, arm)
            })
            .collect()
    }

    fn default_patterns<'a, 'b>(
        &'a mut self,
        sym: Symbol,
        clause_with_heads: impl Iterator<
            Item = &'b (case::Pattern, (Stack<case::Pattern>, simple_case::Expr)),
        >,
    ) -> Vec<(Stack<case::Pattern>, simple_case::Expr)> {
        clause_with_heads
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
            .collect()
    }

    fn is_exhausitive(
        &self,
        type_id: &TypeId,
        discriminansts: impl IntoIterator<Item = u8>,
    ) -> bool {
        self.type_db
            .find(&type_id)
            .cloned()
            .unwrap()
            .adt()
            .into_iter()
            .map(|c| c.discriminant)
            .collect::<HashSet<_>>()
            == discriminansts.into_iter().collect::<HashSet<_>>()
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
    local_handler_labels: Vec<Symbol>,
}

impl SimpleToSwitch {
    pub fn new(symbol_generator: SymbolGenerator) -> Self {
        Self {
            symbol_generator,
            local_handler_labels: Vec::new(),
        }
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
            simple_case::Expr::Inject { discriminant, data } => {
                self.compile_inject(discriminant, data.map(|d| *d))
            }
            simple_case::Expr::Case { cond, clauses } => self.compile_case(*cond, clauses),
            simple_case::Expr::RaiseMatch => self.compile_raise_match(),
            simple_case::Expr::RaiseFail => self.compile_raise_fail(),
            simple_case::Expr::HandleFail { expr, handler } => {
                self.compile_handle_fail(*expr, *handler)
            }
            simple_case::Expr::Symbol(s) => self.compile_symbol(s),
        }
    }

    fn compile_tuple(&mut self, es: Vec<simple_case::Expr>) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};
        let mut stmts = vec![];
        let mut symbols = vec![];

        // 各データのコンパイル
        for e in es {
            let (s, sym) = self.compile_expr(e);
            stmts.extend(s);
            symbols.push(sym);
        }

        // タプルの構成
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
        discriminant: u8,
        data: Option<simple_case::Expr>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};
        let mut ret = Vec::new();
        let size = (data.is_some() as u8) + 1;

        // discriminantの部分
        let dis = self.gensym("dis");
        ret.push(Stmt::Assign(dis.clone(), Op::Const(discriminant as i32)));

        // dataの部分
        let mut data_sym = None;
        if let Some(d) = data {
            let (stmts, d) = self.compile_expr(d);
            ret.extend(stmts);
            data_sym = Some(d)
        }

        // injectの構成
        let v = self.gensym("v");
        ret.push(Stmt::Assign(v.clone(), Op::Alloc { size }));
        ret.push(Stmt::Store {
            base: v.clone(),
            offset: 0,
            data: dis.clone(),
        });
        if let Some(sym) = data_sym {
            ret.push(Stmt::Store {
                base: v.clone(),
                offset: 1,
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
        // まずcondをコンパイルする
        let (cond_stmts, cond_symbol) = self.compile_expr(cond);

        // caseには分岐するcase（代数的データ型に対するパターンマッチ）と分岐しないcase（タプルに対するパターンマッチ）がある。
        // それをここで判別する。
        if let simple_case::Pattern::Tuple(_) = clauses[0].0 {
            // タプルパターンのコンパイル
            self.compile_case_tuple(cond_stmts, cond_symbol, clauses)
        } else {
            // 代数的データ型パターンのコンパイル
            self.compile_case_adt(cond_stmts, cond_symbol, clauses)
        }
    }

    fn compile_case_tuple(
        &mut self,
        mut stmts: Vec<switch::Stmt>,
        cond_symbol: Symbol,
        mut clauses: Vec<(simple_case::Pattern, simple_case::Expr)>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};

        assert_eq!(clauses.len(), 1);
        let (pat, arm) = clauses.pop().unwrap();

        let tuple = match pat {
            simple_case::Pattern::Tuple(t) => t,
            // 代数的データ型は別で処理するのでここにはこない
            _ => unreachable!(),
        };
        let load_elements = tuple.into_iter().enumerate().map(|(i, s)| {
            Stmt::Assign(
                s,
                Op::Load {
                    base: cond_symbol.clone(),
                    offset: i as u8,
                },
            )
        });
        stmts.extend(load_elements);

        let (arm_stmts, arm_symbol) = self.compile_expr(arm);
        stmts.extend(arm_stmts);

        (stmts, arm_symbol)
    }

    fn compile_case_adt(
        &mut self,
        mut stmts: Vec<switch::Stmt>,
        cond_symbol: Symbol,
        clauses: Vec<(simple_case::Pattern, simple_case::Expr)>,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Block, Op, Stmt};

        // それぞれの行を、判別子とそれに対応するブロックの組として集める
        let mut switch_clauses = Vec::<(i32, Block)>::new();
        // デフォルト節があれば記録する
        let mut default = None;
        // 返り値のシンボル
        let result_sym = self.gensym("switch_result");

        for (pat, arm) in clauses {
            let (arm_stmts, arm_symbol) = self.compile_expr(arm);
            // コンストラクタ、変数へのパターンマッチそれぞれをここで捌く
            match pat {
                simple_case::Pattern::Constructor { discriminant, data } => {
                    let mut block = vec![];
                    if let Some(sym) = data {
                        let load = Op::Load {
                            base: cond_symbol.clone(),
                            offset: 1,
                        };
                        block.push(Stmt::Assign(sym, load));
                    }
                    block.extend(arm_stmts);
                    block.push(Stmt::Assign(result_sym.clone(), Op::Symbol(arm_symbol)));
                    switch_clauses.push((discriminant as i32, Block(block)))
                }
                simple_case::Pattern::Variable(s) => {
                    let mut block = vec![Stmt::Assign(s.clone(), Op::Symbol(cond_symbol.clone()))];
                    block.extend(arm_stmts);
                    block.push(Stmt::Assign(result_sym.clone(), Op::Symbol(arm_symbol)));
                    // variableは必ずdefault節になる
                    default = Some(Block(block))
                }
                // タプルは別で処理するのでここにはこない
                _ => unreachable!(),
            }
        }

        stmts.push(Stmt::Switch {
            cond: cond_symbol,
            targets: switch_clauses,
            // default節がなければ（=変数パターンがなければ）default節をUNREACHABLEで埋める
            default: default.unwrap_or_else(|| Block(vec![Stmt::Unreachable])),
        });
        (stmts, result_sym)
    }

    fn compile_raise_match(&mut self) -> (Vec<switch::Stmt>, Symbol) {
        use switch::Stmt;
        (
            vec![Stmt::RaiseMatch],
            self.symbol_generator.gensym("undefined"),
        )
    }

    fn compile_raise_fail(&mut self) -> (Vec<switch::Stmt>, Symbol) {
        use switch::Stmt;
        let label = self
            .local_handler_labels
            // スタックの先頭を取り出す
            .last()
            .expect("internal error: Fail will not be handled")
            .clone();

        (
            vec![Stmt::Goto(label)],
            self.symbol_generator.gensym("undefined"),
        )
    }

    fn compile_handle_fail(
        &mut self,
        expr: simple_case::Expr,
        handler: simple_case::Expr,
    ) -> (Vec<switch::Stmt>, Symbol) {
        use switch::{Op, Stmt};

        let catch = self.symbol_generator.gensym("catch");
        let finally = self.symbol_generator.gensym("finally");
        let result_sym = self.symbol_generator.gensym("handle_result");
        // 一旦ラベルをpushしてコンパイルしたあと、popする
        self.local_handler_labels.push(catch.clone());
        let (mut expr_bb, expr_sym) = self.compile_expr(expr);
        self.local_handler_labels.pop();

        let (mut handler_bb, handler_sym) = self.compile_expr(handler);

        //   expr
        //   ...
        //   var := ...
        //   handle_result := var
        //   goto finally
        // catch:
        //   handler
        //   ...
        //   var' := ...
        //   handle_result := var'
        // finally:
        //   ...
        expr_bb.push(Stmt::Assign(result_sym.clone(), Op::Symbol(expr_sym)));
        expr_bb.push(Stmt::Goto(finally.clone()));
        handler_bb.insert(0, Stmt::Label(catch));
        handler_bb.push(Stmt::Assign(result_sym.clone(), Op::Symbol(handler_sym)));
        handler_bb.push(Stmt::Label(finally));

        expr_bb.extend(handler_bb);

        (expr_bb, result_sym)
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
                discriminant: 0,
                param: None,
            },
            case::Constructor {
                discriminant: 1,
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
                discriminant: 0,
                param: None,
            },
            case::Constructor {
                discriminant: 1,
                param: Some(TypeId::new("bool")),
            },
            case::Constructor {
                discriminant: 2,
                param: Some(TypeId::new("tuple3")),
            },
        ],
    );

    type_db.register_tuple(
        TypeId::new("2list"),
        vec![TypeId::new("list"), TypeId::new("list")],
    );

    type_db.register_tuple(
        TypeId::new("cons"),
        vec![TypeId::new("unit"), TypeId::new("list")],
    );

    type_db.register_adt(
        TypeId::new("list"),
        vec![
            // Nil
            case::Constructor {
                discriminant: 0,
                param: None,
            },
            // Cons () * List
            case::Constructor {
                discriminant: 1,
                param: Some(TypeId::new("cons")),
            },
        ],
    );

    let mut sg = SymbolGenerator::new();

    let m = {
        use case::*;
        use Expr::*;

        let truev = Expr::Inject {
            discriminant: 0,
            data: None,
        };
        let unitv = Expr::Inject {
            discriminant: 0,
            data: None,
        };

        Case {
            cond: Box::new(Inject {
                discriminant: 2,
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
                        discriminant: 0,
                        pattern: None,
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        discriminant: 1,
                        pattern: Some(Box::new(Pattern::Variable(sg.gensym("x")))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        discriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Constructor {
                                discriminant: 0,
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
                        discriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Constructor {
                                discriminant: 0,
                                pattern: None,
                            },
                            Pattern::Variable(sg.gensym("z")),
                        ]))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        discriminant: 2,
                        pattern: Some(Box::new(Pattern::Tuple(vec![
                            Pattern::Variable(sg.gensym("x")),
                            Pattern::Variable(sg.gensym("y")),
                            Pattern::Constructor {
                                discriminant: 0,
                                pattern: None,
                            },
                        ]))),
                    },
                    unitv.clone(),
                ),
                (
                    Pattern::Constructor {
                        discriminant: 2,
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

        // `false` （値）のつもり
        let falsev = Expr::Inject {
            discriminant: 1,
            data: None,
        };
        // `true` （値）のつもり
        let truev = Expr::Inject {
            discriminant: 0,
            data: None,
        };
        // `false` （パターン）のつもり
        let falsep = Pattern::Constructor {
            discriminant: 1,
            pattern: None,
        };
        // `true` （パターン）のつもり
        let truep = Pattern::Constructor {
            discriminant: 0,
            pattern: None,
        };

        // 2要素タプルパターンの便利メソッド
        fn tuple2p(p1: Pattern, p2: Pattern) -> Pattern {
            Pattern::Tuple(vec![p1, p2])
        }

        // case (false, false) of
        //     (true, true) => false
        //   | (true, false) => true
        //   | (false, true) => true
        //   | (false, false) => false
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

    let m3 = {
        use case::*;
        use Expr::*;

        // `Nil` （値）のつもり
        let nilv = Expr::Inject {
            discriminant: 0,
            data: None,
        };

        // `Cons` （コンストラクタ）のつもり
        fn consv(arg: Expr) -> Expr {
            Expr::Inject {
                discriminant: 1,
                data: Some(Box::new(arg)),
            }
        }

        // `()` （値）のつもり
        let unitv = Expr::Tuple(vec![]);

        // 2要素タプルパターンの便利メソッド
        fn tuple2p(p1: Pattern, p2: Pattern) -> Pattern {
            Pattern::Tuple(vec![p1, p2])
        }

        // `Nil` （パターン）のつもり
        let nilp = Pattern::Constructor {
            discriminant: 0,
            pattern: None,
        };

        // `Cons` （パターン）のつもり
        fn consp(car: Pattern, cdr: Pattern) -> Pattern {
            Pattern::Constructor {
                discriminant: 1,
                pattern: Some(Box::new(tuple2p(car, cdr))),
            }
        }

        // _ パターン。
        // 衝突しないために毎度シンボルジェネレータを使って生成する
        fn wild(sg: &mut SymbolGenerator) -> Pattern {
            Pattern::Variable(sg.gensym("_"))
        }

        // case (Nil, Nil) of
        //     (Nil, _) => ()
        //   | (_, Nil) => ()
        //   | (Cons(_, _), Cons(_, _)) => ()
        Case {
            cond: Box::new(Tuple(vec![nilv.clone(), nilv.clone()])),
            ty: TypeId::new("2list"),
            clauses: vec![
                (tuple2p(nilp.clone(), wild(&mut sg)), unitv.clone()),
                (tuple2p(wild(&mut sg), nilp.clone()), unitv.clone()),
                (
                    tuple2p(
                        consp(wild(&mut sg), wild(&mut sg)),
                        consp(wild(&mut sg), wild(&mut sg)),
                    ),
                    unitv.clone(),
                ),
            ],
        }
    };

    let expr = m2;

    let mut p = PrettyPrinter::new();
    p.pp(&expr);
    println!("{:#?}", expr);

    println!("");
    println!("evaled to");
    let mut interpreter = CaseInterp::new();
    let v = interpreter.eval(expr.clone());

    match v {
        Ok(v) => {
            p.pp(&v);
            println!();
            println!("{:?}", v);
        }
        Err(Match) => println!("match failed"),
    }

    println!("");
    p.pp(&expr);
    println!("");
    println!("case to simple (backtrack)");
    let mut compiler = CaseToSimple::new(
        sg.clone(),
        BackTrackPatternCompiler::new(sg.clone(), type_db.clone()),
    );
    let c = compiler.compile(expr.clone());
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
    let c2 = compiler2.compile(expr);
    p.pp(&c2);

    println!("simple to switch");
    let mut compiler = SimpleToSwitch::new(sg.clone());
    let s2 = compiler.compile(c2);

    println!("");
    p.pp(&s2);
}
