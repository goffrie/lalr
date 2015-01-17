#![allow(unused)]
extern crate lalr;
use lalr::*;
use std::collections::{btree_map, BTreeSet, BTreeMap, RingBuf};

macro_rules! map {
    ($($l: expr => $r: expr),*) => ({
        let mut r = BTreeMap::new();
        $(r.insert($l, $r);)*
        r
    })
}

fn rhs<T, N, A>(syms: Vec<Symbol<T, N>>, act: A) -> Rhs<T, N, A> {
    Rhs {
        syms: syms,
        act: act,
    }
}

fn main() {
    let g = Grammar {
        rules: map![
            "S" => vec![
                rhs(vec![Nonterminal("N")], ()),
            ],
            "N" => vec![
                rhs(vec![Nonterminal("V"),
                        Terminal("="),
                        Nonterminal("E")], ()),
                rhs(vec![Nonterminal("E")], ()),
            ],
            "E" => vec![
                rhs(vec![Nonterminal("V")], ()),
            ],
            "V" => vec![
                rhs(vec![Terminal("x")], ()),
                rhs(vec![Terminal("*"),
                        Nonterminal("E")], ()),
            ]
        ],
        start: "S"
    };
    println!("{:?}", g);
    let machine = g.lr0_state_machine();
    machine.print();
    let ag = machine.augmented_grammar();
    println!("{:?}", ag);
    println!("");
    println!("{:?}", g.follow_sets(g.first_sets()));
    println!("");
    println!("{:?}", ag.follow_sets(ag.first_sets()));
    println!("");
    println!("{:?}", g.lalr1());
}
