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
    // FIXME: write a test
    assert!(g.lalr1(|_, _| true, |_, _| 0).is_ok());
}
