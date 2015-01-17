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

#[derive(Show,Clone)]
pub struct Rhs<T, N, A> {
    pub syms: Vec<Symbol<T, N>>,
    pub act: A,
}

impl<T: PartialEq, N: PartialEq, A> PartialEq for Rhs<T, N, A> {
    fn eq(&self, other: &Self) -> bool {
        self.syms == other.syms
    }
}

impl<T: Eq, N: Eq, A> Eq for Rhs<T, N, A> { }

impl<T: PartialOrd, N: PartialOrd, A> PartialOrd for Rhs<T, N, A> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.syms.partial_cmp(&other.syms)
    }
}

impl<T: Ord, N: Ord, A> Ord for Rhs<T, N, A> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.syms.cmp(&other.syms)
    }
}

#[derive(Ord,PartialOrd,Eq,PartialEq,Show)]
pub struct Item<'a, T: 'a, N: 'a, A: 'a> {
    pub lhs: &'a N,
    pub rhs: &'a Rhs<T, N, A>,
    pub pos: usize,
}

impl<'a, T, N, A> Clone for Item<'a, T, N, A> {
    fn clone(&self) -> Item<'a, T, N, A> {
        Item { lhs: self.lhs, rhs: self.rhs, pos: self.pos }
    }
}

#[derive(Ord,PartialOrd,Eq,PartialEq,Show)]
pub struct ItemSet<'a, T: 'a, N: 'a, A: 'a> {
    pub items: BTreeSet<Item<'a, T, N, A>>,
}

impl<'a, T, N, A> Clone for ItemSet<'a, T, N, A> {
    fn clone(&self) -> ItemSet<'a, T, N, A> {
        ItemSet { items: self.items.clone() }
    }
}

#[derive(Show)]
pub struct Grammar<T, N, A> {
    pub rules: BTreeMap<N, Vec<Rhs<T, N, A>>>,
    // `start` must have exactly one rule of the form "`start` -> N", for some nonterminal N,
    // and not be referred to elsewhere in the grammar.
    pub start: N,
}

#[derive(Show)]
pub struct LR0StateMachine<'a, T: 'a, N: 'a, A: 'a> {
    pub states: Vec<(ItemSet<'a, T, N, A>, BTreeMap<&'a Symbol<T, N>, usize>)>,
    pub start: &'a N,
}

// FIXME: A doesn't actually have to be Ord
impl<T: Ord, N: Ord, A: Ord> Grammar<T, N, A> where T: Show, N: Show {
    // Creates the LR(0) state machine for a grammar.
    pub fn lr0_state_machine<'a>(&'a self) -> LR0StateMachine<'a, T, N, A> {
        struct S<'a, T: 'a, N: 'a, A: 'a> {
            states: Vec<(ItemSet<'a, T, N, A>, BTreeMap<&'a Symbol<T, N>, usize>)>,
            item_sets: BTreeMap<ItemSet<'a, T, N, A>, usize>,
            nubs: BTreeMap<ItemSet<'a, T, N, A>, usize>,
        }
        impl<'a, T: Ord, N: Ord, A: Ord> S<'a, T, N, A> {
            fn item_set(&mut self, item_set: ItemSet<'a, T, N, A>) -> usize {
                if let Some(&ix) = self.item_sets.get(&item_set) {
                    return ix;
                }
                let ix = self.states.len();
                self.item_sets.insert(item_set.clone(), ix);
                self.states.push((item_set, BTreeMap::new()));
                ix
            }
            fn complete_nub(&mut self, grammar: &'a Grammar<T, N, A>, nub: ItemSet<'a, T, N, A>) -> usize {
                if let Some(&ix) = self.nubs.get(&nub) {
                    return ix;
                }
                let mut completed: BTreeSet<_> = nub.items.clone();
                let mut to_add: RingBuf<_> = nub.items.iter().cloned().collect();
                while let Some(item) = to_add.pop_front() {
                    if let Some(&Nonterminal(ref n)) = item.rhs.syms.get(item.pos) {
                        if let Some(rules) = grammar.rules.get(n) {
                            for rhs in rules.iter() {
                                let new_item = Item {
                                    lhs: n,
                                    rhs: rhs,
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
        fn advance<'a, 'b, T, N, A>(i: &'b Item<'a, T, N, A>) -> Option<(&'a Symbol<T, N>, Item<'a, T, N, A>)> {
            i.rhs.syms.get(i.pos).map(|s| {
                (s, Item {
                    lhs: i.lhs,
                    rhs: i.rhs,
                    pos: i.pos + 1,
                })
            })
        }
        let mut state: S<'a, T, N, A> = S {
            states: vec![],
            item_sets: BTreeMap::new(),
            nubs: BTreeMap::new(),
        };
        let mut finished = 0;
        state.complete_nub(self, ItemSet { items: { let mut r = BTreeSet::new(); r.insert(Item {
            lhs: &self.start,
            rhs: &self.rules.get(&self.start).unwrap()[0],
            pos: 0,
        }); r } });
        while finished < state.states.len() {
            let mut next_nubs = BTreeMap::new();
            for item in state.states[finished].0.items.iter() {
                if let Some((sym, next)) = advance(item) {
                    next_nubs.entry(sym).get().unwrap_or_else(|v| v.insert(BTreeSet::new())).insert(next);
                }
            }
            for (sym, items) in next_nubs.into_iter() {
                let ix = state.complete_nub(self, ItemSet { items: items });
                state.states[finished].1.insert(sym, ix);
            }
            finished += 1;
        }
        LR0StateMachine {
            states: state.states,
            start: &self.start,
        }
    }

    // returns a map containing:
    // nonterminal => (first set, nullable)
    pub fn first_sets(&self) -> BTreeMap<&N, (BTreeSet<&T>, bool)> {
        let mut r = BTreeMap::new();
        for (lhs, _) in self.rules.iter() {
            r.insert(lhs, RefCell::new((BTreeSet::new(), false)));
        }
        loop {
            let mut changed = false;

            // the `zip` is okay because `self.rules` and `r` have the same order
            for ((lhs, rhses), (_, cell)) in self.rules.iter().zip(r.iter()) {
                let mut cell = cell.borrow_mut();
                'outer: for rhs in rhses.iter() {
                    for sym in rhs.syms.iter() {
                        match *sym {
                            Terminal(ref t) => {
                                if cell.0.insert(t) {
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
                                for &t in them.0.iter() {
                                    if cell.0.insert(t) {
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
    pub fn follow_sets<'a>(&'a self, first: BTreeMap<&'a N, (BTreeSet<&'a T>, bool)>) -> BTreeMap<&'a N, (BTreeSet<&'a T>, bool)> {
        let mut r = BTreeMap::new();
        for (lhs, _) in self.rules.iter() {
            r.insert(lhs, (BTreeSet::new(), *lhs == self.start));
        }
        loop {
            let mut changed = false;
            for (lhs, rhses) in self.rules.iter() {
                for rhs in rhses.iter() {
                    let mut follow = r.get(lhs).unwrap().clone();
                    for sym in rhs.syms.iter().rev() {
                        match *sym {
                            Terminal(ref t) => {
                                follow.0.clear();
                                follow.1 = false;
                                follow.0.insert(t);
                            }
                            Nonterminal(ref n) => {
                                let s = r.get_mut(n).unwrap();
                                for &t in follow.0.iter() {
                                    if s.0.insert(t) {
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
                                follow.0.extend(f.iter().map(|x| *x));
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

impl<'a, T: Ord + Clone, N: Ord + Clone, A: Clone> LR0StateMachine<'a, T, N, A> {
    pub fn augmented_grammar(&self) -> Grammar<T, (usize, N), A> {
        let mut r = BTreeMap::new();
        for (ix, &(ref iset, _)) in self.states.iter().enumerate() {
            for item in iset.items.iter() {
                if item.pos == 0 {
                    let new_lhs = (ix, item.lhs.clone());
                    let new_rhs = Rhs {
                        syms: item.rhs.syms.iter().scan(ix, |st, sym| {
                            let old_st = *st;
                            *st = *self.states[old_st].1.get(sym).unwrap();
                            Some(match *sym {
                                Terminal(ref t) => Terminal(t.clone()),
                                Nonterminal(ref n) => Nonterminal((old_st, n.clone())),
                            })
                        }).collect(),
                        act: item.rhs.act.clone(),
                    };
                    r.entry(new_lhs).get().unwrap_or_else(|v| v.insert(vec![])).push(new_rhs);
                }
            }
        }
        Grammar {
            rules: r,
            start: (0, self.start.clone()),
        }
    }
}

impl<'a, T: Show, N: Show, A> LR0StateMachine<'a, T, N, A> {
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
                    print!(" {:?}", item.rhs.syms[j]);
                }
                print!(" •");
                for j in item.pos..item.rhs.syms.len() {
                    print!(" {:?}", item.rhs.syms[j]);
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
