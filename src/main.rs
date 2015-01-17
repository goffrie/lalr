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

fn main() {
    let g = Grammar {
        rules: map![
            "N" => vec![
                vec![Nonterminal("V"),
                     Terminal("="),
                     Nonterminal("E")],
                vec![Nonterminal("E")],
            ],
            "E" => vec![
                vec![Nonterminal("V")]
            ],
            "V" => vec![
                vec![Terminal("x")],
                vec![Terminal("*"),
                     Nonterminal("E")]
            ]
        ],
        start: "N"
    };
    println!("{:?}", g);
    let machine = g.lr0_state_machine("S");
    machine.print();
    let ag = machine.augmented_grammar("S");
    println!("{:?}", ag);
    println!("");
    println!("{:?}", g.follow_sets(g.first_sets()));
    println!("");
    println!("{:?}", ag.follow_sets(ag.first_sets()));
}
