#![allow(unstable)]

use std::collections::{BTreeSet, BTreeMap, RingBuf};
use std::fmt::{self, Show};
use std::cell::{RefCell};
pub use Symbol::*;

// "T" = terminal, and "N" = nonterminal.

#[derive(Ord,PartialOrd,Eq,PartialEq,Clone)]
pub enum Symbol<T, N> {
    Terminal(T),
    Nonterminal(N),
}

impl<T: Show, N: Show> Show for Symbol<T, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Terminal(ref t) => t.fmt(f),
            Nonterminal(ref n) => n.fmt(f),
        }
    }
}

#[derive(Ord,PartialOrd,Eq,PartialEq,Show,Clone)]
pub struct Item<T, N> {
    pub lhs: N,
    pub rhs: Vec<Symbol<T, N>>,
    pub pos: usize,
}

#[derive(Ord,PartialOrd,Eq,PartialEq,Show,Clone)]
pub struct ItemSet<T, N> {
    pub items: BTreeSet<Item<T, N>>,
}

#[derive(Ord,PartialOrd,Eq,PartialEq,Show,Clone)]
pub struct Rule<T, N> {
    pub lhs: N,
    pub rhs: Vec<Symbol<T, N>>,
}

#[derive(Show)]
pub struct Grammar<T, N> {
    pub rules: BTreeMap<N, Vec<Vec<Symbol<T, N>>>>,
    pub start: N,
}

#[derive(Show)]
pub struct LR0StateMachine<T, N> {
    pub states: Vec<(ItemSet<T, N>, BTreeMap<Symbol<T, N>, usize>)>,
}

impl<T: Ord + Clone, N: Ord + Clone> Grammar<T, N> where T: Show, N: Show {
    pub fn lr0_state_machine(&self, fake_start: N) -> LR0StateMachine<T, N> {
        struct S<T, N> {
            states: Vec<(ItemSet<T, N>, BTreeMap<Symbol<T, N>, usize>)>,
            item_sets: BTreeMap<ItemSet<T, N>, usize>,
            nubs: BTreeMap<ItemSet<T, N>, usize>,
        }
        impl<T: Ord + Clone, N: Ord + Clone> S<T, N> {
            fn item_set(&mut self, item_set: ItemSet<T, N>) -> usize {
                if let Some(&ix) = self.item_sets.get(&item_set) {
                    return ix;
                }
                let ix = self.states.len();
                self.item_sets.insert(item_set.clone(), ix);
                self.states.push((item_set, BTreeMap::new()));
                ix
            }
            fn complete_nub(&mut self, grammar: &Grammar<T, N>, nub: ItemSet<T, N>) -> usize {
                if let Some(&ix) = self.nubs.get(&nub) {
                    return ix;
                }
                let mut completed = nub.items.clone();
                let mut to_add: RingBuf<_> = nub.items.iter().cloned().collect();
                while let Some(item) = to_add.pop_front() {
                    if let Some(&Nonterminal(ref n)) = item.rhs.get(item.pos) {
                        if let Some(rules) = grammar.rules.get(n) {
                            for rhs in rules.iter() {
                                let new_item = Item {
                                    lhs: n.clone(),
                                    rhs: rhs.clone(),
                                    pos: 0,
                                };
                                if !completed.contains(&new_item) {
                                    to_add.push_back(new_item.clone());
                                    completed.insert(new_item);
                                }
                            }
                        }
                    }
                }
                let ix = self.item_set(ItemSet { items: completed });
                self.nubs.insert(nub, ix);
                ix
            }
        }
        fn advance<T: Clone, N: Clone>(i: &Item<T, N>) -> Option<(&Symbol<T, N>, Item<T, N>)> {
            i.rhs.get(i.pos).map(|&: s| {
                (s, Item {
                    lhs: i.lhs.clone(),
                    rhs: i.rhs.clone(),
                    pos: i.pos + 1,
                })
            })
        }
        let mut state = S {
            states: vec![],
            item_sets: BTreeMap::new(),
            nubs: BTreeMap::new(),
        };
        let mut finished = 0;
        state.complete_nub(self, ItemSet { items: { let mut r = BTreeSet::new(); r.insert(Item {
            lhs: fake_start.clone(),
            rhs: vec![Nonterminal(self.start.clone())],
            pos: 0,
        }); r } });
        while finished < state.states.len() {
            let mut next_nubs = BTreeMap::new();
            for item in state.states[finished].0.items.iter() {
                if let Some((sym, next)) = advance(item) {
                    next_nubs.entry(sym.clone()).get().unwrap_or_else(|v| v.insert(BTreeSet::new())).insert(next);
                }
            }
            for (sym, items) in next_nubs.into_iter() {
                let ix = state.complete_nub(self, ItemSet { items: items });
                state.states[finished].1.insert(sym, ix);
            }
            finished += 1;
        }
        LR0StateMachine { states: state.states }
    }

    // returns a map containing:
    // nonterminal => (first set, nullable)
    pub fn first_sets(&self) -> BTreeMap<N, (BTreeSet<T>, bool)> {
        let mut r = BTreeMap::new();
        for (lhs, _) in self.rules.iter() {
            r.insert(lhs.clone(), RefCell::new((BTreeSet::new(), false)));
        }
        loop {
            let mut changed = false;

            // the `zip` is okay because `self.rules` and `r` have the same order
            for ((lhs, rhses), (_, cell)) in self.rules.iter().zip(r.iter()) {
                let mut cell = cell.borrow_mut();
                'outer: for rhs in rhses.iter() {
                    for sym in rhs.iter() {
                        match *sym {
                            Terminal(ref t) => {
                                if cell.0.insert(t.clone()) {
                                    changed = true;
                                }
                                continue 'outer;
                            }
                            Nonterminal(ref n) => if n == lhs {
                                // refers to `lhs`; no need to add own set elements
                                if !cell.1 {
                                    continue 'outer;
                                }
                            } else {
                                let them = r.get(n).unwrap().borrow();
                                for t in them.0.iter() {
                                    if cell.0.insert(t.clone()) {
                                        changed = true;
                                    }
                                }
                                if !them.1 {
                                    // stop if it's not nullable
                                    continue 'outer;
                                }
                            },
                        }
                    }
                    if !cell.1 {
                        // if we got here, then we must be nullable
                        cell.1 = true;
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
        r.into_iter().map(|(k, v)| (k, v.into_inner())).collect()
    }

    // returns a map containing:
    // nonterminal => (follow set, follow set contains EOF)
    pub fn follow_sets(&self, first: BTreeMap<N, (BTreeSet<T>, bool)>) -> BTreeMap<N, (BTreeSet<T>, bool)> {
        let mut r = BTreeMap::new();
        for (lhs, _) in self.rules.iter() {
            r.insert(lhs.clone(), (BTreeSet::new(), *lhs == self.start));
        }
        loop {
            let mut changed = false;
            for (lhs, rhses) in self.rules.iter() {
                for rhs in rhses.iter() {
                    let mut follow = r.get(lhs).unwrap().clone();
                    for sym in rhs.iter().rev() {
                        match *sym {
                            Terminal(ref t) => {
                                follow.0.clear();
                                follow.1 = false;
                                follow.0.insert(t.clone());
                            }
                            Nonterminal(ref n) => {
                                let s = r.get_mut(n).unwrap();
                                for t in follow.0.iter() {
                                    if s.0.insert(t.clone()) {
                                        changed = true;
                                    }
                                }
                                if !s.1 && follow.1 {
                                    s.1 = true;
                                    changed = true;
                                }
                                let &(ref f, nullable) = first.get(n).unwrap();
                                if !nullable {
                                    follow.0.clear();
                                    follow.1 = false;
                                }
                                follow.0.extend(f.iter().cloned());
                            }
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }
        r
    }
}

impl<T: Ord + Clone, N: Ord + Clone> LR0StateMachine<T, N> {
    pub fn augmented_grammar(&self, fake_start: N) -> Grammar<T, (usize, N)> {
        let mut r = BTreeMap::new();
        for (ix, &(ref iset, _)) in self.states.iter().enumerate() {
            for item in iset.items.iter() {
                if item.pos == 0 {
                    let new_lhs = (ix, item.lhs.clone());
                    let new_rhs = item.rhs.iter().scan(ix, |st, sym| {
                        let old_st = *st;
                        *st = *self.states[old_st].1.get(sym).unwrap();
                        Some(match *sym {
                            Terminal(ref t) => Terminal(t.clone()),
                            Nonterminal(ref n) => Nonterminal((old_st, n.clone())),
                        })
                    }).collect();
                    r.entry(new_lhs).get().unwrap_or_else(|v| v.insert(vec![])).push(new_rhs);
                }
            }
        }
        Grammar {
            rules: r,
            start: (0, fake_start),
        }
    }
}

impl<T: Show, N: Show> LR0StateMachine<T, N> {
    pub fn print(&self) {
        println!(r#"
digraph G {{
    node [
        shape="box",
        style="rounded",
        penwidth=1,
        width=2.0
    ];"#);
        for (i, &(ref iset, ref trans)) in self.states.iter().enumerate() {
            print!("s{}[label=<", i);
            for item in iset.items.iter() {
                print!("{:?} →", item.lhs);
                for j in 0..item.pos {
                    print!(" {:?}", item.rhs[j]);
                }
                print!(" •");
                for j in item.pos..item.rhs.len() {
                    print!(" {:?}", item.rhs[j]);
                }
                print!("<br />\n");
            }
            println!(">]");
            for (sym, &target) in trans.iter() {
                println!("s{} -> s{} [label=<{:?}>]", i, target, sym);
            }
        }
        println!("}}");
    }
}
