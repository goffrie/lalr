use crate::config::DefaultConfig;

use super::*;
use std::collections::BTreeMap;

macro_rules! map {
    ($($l: expr => $r: expr),*) => ({
        let mut r = BTreeMap::new();
        $(r.insert($l, $r);)*
        r
    });
}

macro_rules! coll {
    ($($x: expr),*) => (vec!($($x),*).into_iter().collect());
    () => (None.into_iter().collect());
}

fn rhs<T, N, A>(syms: Vec<Symbol<T, N>>, act: A) -> Rhs<T, N, A> {
    Rhs { syms, act }
}

fn grammar() -> Grammar<&'static str, &'static str, ()> {
    Grammar {
        rules: map![
            "S" => vec![
                rhs(vec![Nonterminal("N")], ()),
            ],
            "N" => vec![
                rhs(vec![Nonterminal("V"), Terminal("="), Nonterminal("E")], ()),
                rhs(vec![Nonterminal("E")], ()),
            ],
            "E" => vec![
                rhs(vec![Nonterminal("V")], ()),
            ],
            "V" => vec![
                rhs(vec![Terminal("x")], ()),
                rhs(vec![Terminal("*"), Nonterminal("E")], ()),
            ]
        ],
        start: "S",
    }
}

static S: &'static &str = &"S";
static N: &'static &str = &"N";
static E: &'static &str = &"E";
static V: &'static &str = &"V";
static X: &'static &str = &"x";
static STAR: &'static &str = &"*";
static EQ: &'static &str = &"=";

#[test]
fn test_first_sets() {
    let g = grammar();
    assert_eq!(
        g.first_sets(),
        map! {
            S => (coll![X, STAR], false),
            N => (coll![X, STAR], false),
            E => (coll![X, STAR], false),
            V => (coll![X, STAR], false)
        }
    );
}

#[test]
fn test_follow_sets() {
    let g = grammar();
    assert_eq!(
        g.follow_sets(g.first_sets()),
        map! {
            S => (coll![], true),
            N => (coll![], true),
            E => (coll![EQ], true),
            V => (coll![EQ], true)
        }
    );
}

#[test]
fn test_extended_grammar() {
    let g = grammar();
    let state_machine = g.lr0_state_machine();
    let extended = state_machine.extended_grammar();
    assert_eq!(extended.start.1, S);
    // Too difficult to test the actual output so just do a sanity check
    assert_eq!(extended.rules.len(), 8);
    let total: usize = extended.rules.values().map(|rhss| rhss.len()).sum();
    assert_eq!(total, 12);
}

#[test]
fn test_lalr1() {
    let g = grammar();
    let c = DefaultConfig::new();
    // FIXME: write a test
    assert!(g.lalr1(&c).is_ok());
}

#[test]
fn test_lalr1_no_conflict() {
    // This grammar was taken from https://github.com/jsinger67/parol/blob/main/examples/list_lr/list.par
    let g = Grammar {
        rules: map![
            "List" => vec![
                rhs(vec![Nonterminal("ListOpt")], ()),
            ],
            "ListOpt" => vec![
                rhs(vec![Nonterminal("Items")], ()),
                rhs(vec![], ()),
            ],
            "Items" => vec![
                rhs(vec![Nonterminal("Num"), Nonterminal("ItemsList")], ()),
            ],
            "ItemsList" => vec![
                rhs(vec![Nonterminal("ItemsList"), Terminal(","), Nonterminal("Num")], ()),
                rhs(vec![], ()),
            ],
            "Num" => vec![
                rhs(vec![Terminal("/0|[1-9][0-9]*/")], ()),
            ]
        ],
        start: "List",
    };
    let pt_expected = LR1ParseTable::<&str, &str, ()> {
        states: vec![
            LR1State {
                eof: Some(LRAction::Reduce(
                    &"ListOpt",
                    &g.rules.get("ListOpt").unwrap()[1],
                )),
                lookahead: map! {
                    &"/0|[1-9][0-9]*/" => LRAction::Shift(1)
                },
                goto: map! {
                    &"Items" => 2,
                    &"ListOpt" => 3,
                    &"Num" => 4
                },
            },
            LR1State {
                eof: Some(LRAction::Reduce(&"Num", &g.rules.get("Num").unwrap()[0])),
                lookahead: map! {
                    &"," => LRAction::Reduce(
                        &"Num",
                        &g.rules.get("Num").unwrap()[0]
                    )
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: Some(LRAction::Reduce(
                    &"ListOpt",
                    &g.rules.get("ListOpt").unwrap()[0],
                )),
                lookahead: BTreeMap::new(),
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: Some(LRAction::Accept),
                lookahead: BTreeMap::new(),
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: Some(LRAction::Reduce(
                    &"ItemsList",
                    &g.rules.get("ItemsList").unwrap()[1],
                )),
                lookahead: map! {
                    &"," => LRAction::Reduce(
                        &"ItemsList",
                        &g.rules.get("ItemsList").unwrap()[1],
                    )
                },
                goto: map! {
                    &"ItemsList" => 5
                },
            },
            LR1State {
                eof: Some(LRAction::Reduce(
                    &"Items",
                    &g.rules.get("Items").unwrap()[0],
                )),
                lookahead: map! {
                    &"," => LRAction::Shift(6)
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: None,
                lookahead: map! {
                    &"/0|[1-9][0-9]*/" => LRAction::Shift(1)
                },
                goto: map! {
                    &"Num" => 7
                },
            },
            LR1State {
                eof: Some(LRAction::Reduce(
                    &"ItemsList",
                    &g.rules.get("ItemsList").unwrap()[0],
                )),
                lookahead: map! {
                    &"," => LRAction::Reduce(
                        &"ItemsList",
                        &g.rules.get("ItemsList").unwrap()[0],
                    )
                },
                goto: BTreeMap::new(),
            },
        ],
    };

    if let Ok(pt) = g.lalr1(&DefaultConfig::new()) {
        assert_eq!(pt, pt_expected);
    } else {
        panic!("Expected a parse table");
    }
}

struct TestConfig<'a> {
    calls: RefCell<Vec<LR1ResolvedConflict<'a, &'a str, &'a str, ()>>>,
}

impl<'a> TestConfig<'a> {
    fn new() -> Self {
        TestConfig {
            calls: RefCell::new(vec![]),
        }
    }
}

impl<'a> Config<'a, &'a str, &'a str, ()> for TestConfig<'a> {
    fn resolve_shift_reduce_conflict_in_favor_of_shift(&self) -> bool {
        true
    }

    fn warn_on_resolved_conflicts(&self) -> bool {
        true
    }

    fn on_resolved_conflict(&self, conflict: LR1ResolvedConflict<'a, &'a str, &'a str, ()>) {
        println!("\nResolved conflict: {:?}", conflict);
        self.calls.borrow_mut().push(conflict);
    }
}

#[test]
fn test_lalr1_resolved_shift_reduce_conflict() {
    // The grammar resemples this example
    // Conflict
    //     : Stmt
    //     ;
    // Stmt
    //     : 'if' '(' cond ')' Stmt
    //     | 'if' '(' cond ')' Stmt 'else'^ Stmt
    //     |
    //     ;
    // cond: 'true'
    //     | 'false'
    //     ;
    // The grammar is ambigous, but the shift-reduce conflict can be resolved by shifting.
    // The shift-reduce conflict in this grammar arises from the ambiguity of the `else` clause in
    // an `if` statement. This is often referred to as the "dangling else" problem.
    // Here's the issue: when you have nested `if` statements, it's unclear to which `if` an `else`
    // belongs. For example, consider the following code:

    // ```
    // if (Cond) if (Cond) Stmt else Stmt
    // ```

    // This could be interpreted in two ways:

    // 1. As `if (Cond) { if (Cond) Stmt else Stmt }` (where the `else` belongs to the inner `if`)
    // 2. As `if (Cond) { if (Cond) Stmt } else Stmt` (where the `else` belongs to the outer `if`)

    // In the first interpretation, we "reduce" when we see the `else` (i.e., we finish the inner
    // `if` statement). In the second interpretation, we "shift" (i.e., we consider the `else` as
    // part of the outer `if` statement).

    // This is a shift-reduce conflict. The parser doesn't know whether to shift (add the `else` to
    // the stack for further processing) or reduce (use the `else` to complete a statement and
    // remove symbols from the stack).

    // Most parser generators, including YACC and Bison, resolve this conflict by shifting, which
    // corresponds to the first interpretation (the `else` belongs to the inner `if`). This is
    // generally what we want in programming languages -- it's how C, C++, Java, and most other
    // languages handle this ambiguity.
    let g = Grammar {
        rules: map![
            "Conflict" => vec![
                rhs(vec![Nonterminal("Stmt")], ()),
            ],
            "Stmt" => vec![
                rhs(vec![
                    Terminal("if"),
                    Terminal("("),
                    Nonterminal("Cond"),
                    Terminal(")"),
                    Nonterminal("Stmt")], ()),
                rhs(vec![
                    Terminal("if"),
                    Terminal("("),
                    Nonterminal("Cond"),
                    Terminal(")"),
                    Nonterminal("Stmt"),
                    Terminal("else"),
                    Nonterminal("Stmt")], ()),
                rhs(vec![], ()),
            ],
            "Cond" => vec![
                rhs(vec![Terminal("true")], ()),
                rhs(vec![Terminal("false")], ()),
            ]
        ],
        start: "Conflict",
    };

    let pt_expected = LR1ParseTable::<&str, &str, ()> {
        states: vec![
            LR1State {
                eof: Some(LRAction::Reduce(&"Stmt", &g.rules.get("Stmt").unwrap()[2])),
                lookahead: map! {
                    &"if" => LRAction::Shift(1)
                },
                goto: map! {
                    &"Stmt" => 2
                },
            },
            LR1State {
                eof: None,
                lookahead: map! {
                    &"(" => LRAction::Shift(3)
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: Some(LRAction::Accept),
                lookahead: BTreeMap::new(),
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: None,
                lookahead: map! {
                    &"false" => LRAction::Shift(4),
                    &"true" => LRAction::Shift(5)
                },
                goto: map! {
                    &"Cond" => 6
                },
            },
            LR1State {
                eof: None,
                lookahead: map! {
                    &")" => LRAction::Reduce(
                        &"Cond",
                        &g.rules.get("Cond").unwrap()[1],
                    )
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: None,
                lookahead: map! {
                    &")" => LRAction::Reduce(
                        &"Cond",
                        &g.rules.get("Cond").unwrap()[0],
                    )
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: None,
                lookahead: map! {
                    &")" => LRAction::Shift(7)
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: Some(LRAction::Reduce(&"Stmt", &g.rules.get("Stmt").unwrap()[2])),
                lookahead: map! {
                    &"else" => LRAction::Reduce(
                        &"Stmt",
                        &g.rules.get("Stmt").unwrap()[2]
                    ),
                    &"if" => LRAction::Shift(1)
                },
                goto: map! {
                    &"Stmt" => 8
                },
            },
            LR1State {
                eof: Some(LRAction::Reduce(&"Stmt", &g.rules.get("Stmt").unwrap()[0])),
                lookahead: map! {
                    &"else" => LRAction::Shift(9)
                },
                goto: BTreeMap::new(),
            },
            LR1State {
                eof: Some(LRAction::Reduce(&"Stmt", &g.rules.get("Stmt").unwrap()[2])),
                lookahead: map! {
                    &"else" => LRAction::Reduce(
                        &"Stmt",
                        &g.rules.get("Stmt").unwrap()[2]
                    ),
                    &"if" => LRAction::Shift(1)
                },
                goto: map! {
                    &"Stmt" => 10
                },
            },
            LR1State {
                eof: Some(LRAction::Reduce(&"Stmt", &g.rules.get("Stmt").unwrap()[1])),
                lookahead: map! {
                    &"else" => LRAction::Reduce(
                        &"Stmt",
                        &g.rules.get("Stmt").unwrap()[1]
                    )
                },
                goto: BTreeMap::new(),
            },
        ],
    };

    let config = TestConfig::new();
    if let Ok(pt) = g.lalr1(&config) {
        assert_eq!(pt, pt_expected);
        // The conflict is actually resolved twice
        // I'm not sure why it's resolved twice, maybe this is a problem in the implementation
        assert_eq!(config.calls.borrow().len(), 2);
    } else {
        panic!("Expected a parse table");
    }
}
