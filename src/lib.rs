#![allow(unstable)]

use std::collections::{btree_map, BTreeSet, BTreeMap, RingBuf};
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

#[derive(Show)]
pub enum LRAction<'a, T: 'a, N: 'a, A: 'a> {
    Reduce(&'a N, &'a Rhs<T, N, A>),
    Shift(usize),
    Accept,
}

#[derive(Show)]
pub struct LR1State<'a, T: 'a, N: 'a, A: 'a> {
    pub eof: Option<LRAction<'a, T, N, A>>,
    pub lookahead: BTreeMap<&'a T, LRAction<'a, T, N, A>>,
    pub goto: BTreeMap<&'a N, usize>,
}

#[derive(Show)]
pub struct LR1ParseTable<'a, T: 'a, N: 'a, A: 'a> {
    pub states: Vec<LR1State<'a, T, N, A>>,
}

// FIXME: A doesn't actually have to be Ord
impl<T: Ord, N: Ord, A: Ord> Grammar<T, N, A> {
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

    pub fn lalr1<'a>(&'a self) -> LR1ParseTable<'a, T, N, A>
        where T: Show, N: Show, Rhs<T, N, A>: Show {
        let state_machine = self.lr0_state_machine();
        let augmented = state_machine.augmented_grammar();
        let first_sets = augmented.first_sets();
        let follow_sets = augmented.follow_sets(first_sets);
        let mut r = LR1ParseTable {
            states: state_machine.states.iter().map(|_| LR1State {
                eof: None,
                lookahead: BTreeMap::new(),
                goto: BTreeMap::new(),
            }).collect(),
        };

        // add shifts
        for (i, &(_, ref trans)) in state_machine.states.iter().enumerate() {
            for (&sym, &target) in trans.iter() {
                match *sym {
                    Terminal(ref t) => {
                        let z = r.states[i].lookahead.insert(t, LRAction::Shift(target));
                        // can't have conflicts yet
                        debug_assert!(z.is_none());
                    }
                    Nonterminal(ref n) => {
                        let z = r.states[i].goto.insert(n, target);
                        debug_assert!(z.is_none());
                    }
                }
            }
        }

        // add reductions
        for ((&(start_state, lhs), rhss),
             (&&(s2, l2), &(ref follow, eof)))
            in augmented.rules.iter().zip(follow_sets.iter()) {

            debug_assert_eq!(start_state, s2);
            debug_assert_eq!(lhs, l2);

            for &Rhs { syms: _, act: (end_state, rhs) }
                in rhss.iter() {

                for &&t in follow.iter() {
                    match r.states[end_state].lookahead.entry(t) {
                        btree_map::Entry::Vacant(v) => {
                            v.insert(LRAction::Reduce(lhs, rhs));
                        }
                        btree_map::Entry::Occupied(v) => {
                            match *v.get() {
                                LRAction::Reduce(l, r) if l == lhs
                                    && r as *const Rhs<T, N, A>
                                       == rhs as *const Rhs<T, N, A> => {
                                    // The cells match, so there's no conflict.
                                }
                                LRAction::Reduce(l, r) => {
                                    // Otherwise, we have a reduce/reduce conflict.
                                    // FIXME: report errors properly
                                    panic!("reduce-reduce conflict in state {}, token {:?}: {:?} -> {:?} vs {:?} -> {:?}", end_state, t, l, r, lhs, rhs);
                                }
                                LRAction::Shift(st) => {
                                    panic!("shift-reduce conflict in state {}, token {:?}: state {} vs {:?} -> {:?}", end_state, t, st, lhs, rhs);
                                }
                                LRAction::Accept => {
                                    unreachable!();
                                }
                            }
                        }
                    }
                }

                if eof {
                    let state = &mut r.states[end_state];
                    if *lhs == self.start {
                        match state.eof {
                            Some(_) => unreachable!(),
                            _ => ()
                        }
                        state.eof = Some(LRAction::Accept);
                    } else {
                        match state.eof {
                            Some(LRAction::Reduce(l, r)) if l == lhs
                                && r as *const Rhs<T, N, A>
                                   == rhs as *const Rhs<T, N, A> => {
                                // no problem
                            }
                            Some(LRAction::Reduce(l, r)) => {
                                panic!("reduce-reduce conflict in state {}, on EOF: {:?} -> {:?} vs {:?} -> {:?}", end_state, l, r, lhs, rhs);
                            }
                            Some(LRAction::Shift(st)) => {
                                panic!("shift-reduce conflict in state {}, on EOF: state {} vs {:?} -> {:?}", end_state, st, lhs, rhs);
                            }
                            Some(LRAction::Accept) => {
                                unreachable!();
                            }
                            None => {
                                state.eof = Some(LRAction::Reduce(lhs, rhs));
                            }
                        }
                    }
                }

            }
        }

        r
    }
}

impl<'a, T: Ord, N: Ord, A> LR0StateMachine<'a, T, N, A> {
    pub fn augmented_grammar(&self) -> Grammar<&'a T, (usize, &'a N), (usize, &'a Rhs<T, N, A>)> {
        let mut r = BTreeMap::new();
        for (ix, &(ref iset, _)) in self.states.iter().enumerate() {
            for item in iset.items.iter() {
                if item.pos == 0 {
                    let new_lhs = (ix, item.lhs);
                    let mut state = ix;
                    let new_rhs = Rhs {
                        syms: item.rhs.syms.iter().map(|sym| {
                            let old_st = state;
                            state = *self.states[old_st].1.get(sym).unwrap();
                            match *sym {
                                Terminal(ref t) => Terminal(t),
                                Nonterminal(ref n) => Nonterminal((old_st, n)),
                            }
                        }).collect(),
                        act: (state, item.rhs),
                    };
                    r.entry(new_lhs).get().unwrap_or_else(|v| v.insert(vec![])).push(new_rhs);
                }
            }
        }
        Grammar {
            rules: r,
            start: (0, self.start),
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
