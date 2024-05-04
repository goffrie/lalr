//! This crate provides data structures for context-free grammars and LR(1) state machines, and an
//! algorithm to convert a context-free grammar into an LR(1) state machine by the LALR(1)
//! construction.
//!
//! To use this crate, you should create a [`Grammar`](struct.Grammar.html) and call
//! [`lalr1`](struct.Grammar.html#method.lalr1). Then you can use the
//! [`LR1ParseTable`](struct.LR1ParseTable.html) to create your own parser.

#![deny(missing_docs)]

pub mod config;
pub use config::Config;

use std::cell::RefCell;
use std::cmp;
use std::collections::{btree_map, BTreeMap, BTreeSet, VecDeque};
use std::fmt::{self, Debug, Display};
pub use Symbol::*;

#[cfg(test)]
mod tests;

// "T" = terminal, and "N" = nonterminal.

/// A symbol in a context-free grammar.
#[derive(Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum Symbol<T, N> {
    /// A terminal symbol.
    Terminal(T),
    /// A nonterminal symbol.
    Nonterminal(N),
}

impl<T: Display, N: Display> Display for Symbol<T, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Terminal(ref t) => t.fmt(f),
            Nonterminal(ref n) => n.fmt(f),
        }
    }
}

impl<T: Debug, N: Debug> Debug for Symbol<T, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Terminal(ref t) => t.fmt(f),
            Nonterminal(ref n) => n.fmt(f),
        }
    }
}
macro_rules! item {
    ($x: item) => {
        $x
    };
}
// Derive the {Partial,}{Eq,Ord} traits, based on the tuple implementations,
// for the given fields.
macro_rules! comparators {
    ($t: ident($($p: tt)+) ($($s: ident),+) ($($f: ident),+)) => {
        item!(impl<$($p)+> PartialEq for $t<$($p)+> where $($s: PartialEq),+ {
            fn eq(&self, other: &Self) -> bool {
                ($(self.$f),+) == ($(other.$f),+)
            }
        });
        item!(impl<$($p)+> Eq for $t<$($p)+> where $($s: Eq),+ {
        });
        item!(impl<$($p)+> PartialOrd for $t<$($p)+> where $($s: PartialOrd),+ {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                ($(self.$f),+).partial_cmp(&($(other.$f),+))
            }
        });
        item!(impl<$($p)+> Ord for $t<$($p)+> where $($s: Ord),+ {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                ($(self.$f),+).cmp(&($(other.$f),+))
            }
        });
    };
}

/// The right-hand side of a rule in a context-free grammar.
#[derive(Debug, Clone)]
pub struct Rhs<T, N, A> {
    /// The symbols in the right-hand side of the rule.
    pub syms: Vec<Symbol<T, N>>,
    /// An action associated with this rule.
    /// This can be any data you want to attach to a rule.
    ///
    /// This field is ignored by the `Eq` and `Ord` instances.
    pub act: A,
}
comparators!(Rhs(T, N, A)(T, N)(syms));

/// An item in an LR(0) state machine, consisting of a rule with a distinguished current position.
#[derive(Debug)]
pub struct Item<'a, T: 'a, N: 'a, A: 'a> {
    /// The left-hand side of the rule.
    pub lhs: &'a N,
    /// The right-hand side of the rule.
    pub rhs: &'a Rhs<T, N, A>,
    /// The current position in the rule.
    pub pos: usize,
}
comparators!(Item('a, T, N, A) (T, N) (lhs, rhs, pos));

impl<'a, T, N, A> Clone for Item<'a, T, N, A> {
    fn clone(&self) -> Item<'a, T, N, A> {
        Item {
            lhs: self.lhs,
            rhs: self.rhs,
            pos: self.pos,
        }
    }
}

/// A set of `Item`s, forming a state in an LR(0) state machine.
#[derive(Debug)]
pub struct ItemSet<'a, T: 'a, N: 'a, A: 'a> {
    /// The items in the set.
    pub items: BTreeSet<Item<'a, T, N, A>>,
}
comparators!(ItemSet('a, T, N, A) (T, N) (items));

impl<'a, T, N, A> Clone for ItemSet<'a, T, N, A> {
    fn clone(&self) -> ItemSet<'a, T, N, A> {
        ItemSet {
            items: self.items.clone(),
        }
    }
}

/// A context-free grammar.
#[derive(Debug)]
pub struct Grammar<T, N, A> {
    /// The rules for each nonterminal.
    pub rules: BTreeMap<N, Vec<Rhs<T, N, A>>>,
    /// The starting state.
    /// There must be exactly one rule of the form "`start` -> N", for some nonterminal N.
    /// `start` must not be referred to elsewhere in the grammar.
    pub start: N,
}

type LR0State<'a, T, N, A> = (ItemSet<'a, T, N, A>, BTreeMap<&'a Symbol<T, N>, usize>);

/// An LR(0) state machine.
#[derive(Debug)]
pub struct LR0StateMachine<'a, T: 'a, N: 'a, A: 'a> {
    /// A vector of states, each of which consists of an item set and a set of transitions.
    ///
    /// State 0 is the starting state.
    pub states: Vec<LR0State<'a, T, N, A>>,
    /// The starting state of the associated context-free grammar.
    pub start: &'a N,
}

/// An action in an LR(1) parse table.
#[derive(Debug, Eq, PartialEq)]
pub enum LRAction<'a, T: 'a, N: 'a, A: 'a> {
    /// Reduce by the given rule.
    Reduce(&'a N, &'a Rhs<T, N, A>),
    /// Shift, moving to the given state.
    Shift(usize),
    /// Accept, ending the parse.
    Accept,
}

/// A state in an LR(1) parse table.
#[derive(Debug, PartialEq, Eq)]
pub struct LR1State<'a, T: 'a, N: 'a, A: 'a> {
    /// The action if the lookahead is EOF.
    pub eof: Option<LRAction<'a, T, N, A>>,
    /// The actions for each non-EOF lookahead.
    pub lookahead: BTreeMap<&'a T, LRAction<'a, T, N, A>>,
    /// The state to jump to when shifting a nonterminal (because of a reduce rule).
    pub goto: BTreeMap<&'a N, usize>,
}

/// An LR(1) parse table.
#[derive(Debug, PartialEq, Eq)]
pub struct LR1ParseTable<'a, T: 'a, N: 'a, A: 'a> {
    /// The states of the parse table.
    pub states: Vec<LR1State<'a, T, N, A>>,
}

/// A conflict detected while trying to construct an LR(1) parse table.
#[derive(Debug)]
pub enum LR1Conflict<'a, T: 'a, N: 'a, A: 'a> {
    /// A reduce-reduce conflict.
    ReduceReduce {
        /// The LR(0) state in which the conflict occurs.
        state: ItemSet<'a, T, N, A>,
        /// The token leading to the conflict, or `None` if the token is EOF.
        token: Option<&'a T>,
        /// The first conflicting rule.
        r1: (&'a N, &'a Rhs<T, N, A>),
        /// The second conflicting rule.
        r2: (&'a N, &'a Rhs<T, N, A>),
    },
    /// A shift-reduce conflict.
    ShiftReduce {
        /// The LR(0) state in which the conflict appears.
        state: ItemSet<'a, T, N, A>,
        /// The token leading to the conflict, or `None` if the token is EOF.
        token: Option<&'a T>,
        /// The reduce rule involved in the conflict.
        rule: (&'a N, &'a Rhs<T, N, A>),
    },
}

impl<T: Ord, N: Ord, A> Grammar<T, N, A> {
    /// Create the LR(0) state machine for a grammar.
    pub fn lr0_state_machine<'a>(&'a self) -> LR0StateMachine<'a, T, N, A> {
        struct S<'a, T: 'a, N: 'a, A: 'a> {
            states: Vec<LR0State<'a, T, N, A>>,
            item_sets: BTreeMap<ItemSet<'a, T, N, A>, usize>,
            nubs: BTreeMap<ItemSet<'a, T, N, A>, usize>,
        }
        impl<'a, T: Ord, N: Ord, A> S<'a, T, N, A> {
            fn item_set(&mut self, item_set: ItemSet<'a, T, N, A>) -> usize {
                if let Some(&ix) = self.item_sets.get(&item_set) {
                    return ix;
                }
                let ix = self.states.len();
                self.item_sets.insert(item_set.clone(), ix);
                self.states.push((item_set, BTreeMap::new()));
                ix
            }
            fn complete_nub(
                &mut self,
                grammar: &'a Grammar<T, N, A>,
                nub: ItemSet<'a, T, N, A>,
            ) -> usize {
                if let Some(&ix) = self.nubs.get(&nub) {
                    return ix;
                }
                let mut completed: BTreeSet<_> = nub.items.clone();
                let mut to_add: VecDeque<_> = nub.items.iter().cloned().collect();
                while let Some(item) = to_add.pop_front() {
                    if let Some(Nonterminal(ref n)) = item.rhs.syms.get(item.pos) {
                        if let Some(rules) = grammar.rules.get(n) {
                            for rhs in rules.iter() {
                                let new_item = Item {
                                    lhs: n,
                                    rhs,
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
        type SymbolItem<'a, T, N, A> = (&'a Symbol<T, N>, Item<'a, T, N, A>);

        fn advance<'a, T, N, A>(i: &Item<'a, T, N, A>) -> Option<SymbolItem<'a, T, N, A>> {
            i.rhs.syms.get(i.pos).map(|s| {
                (
                    s,
                    Item {
                        lhs: i.lhs,
                        rhs: i.rhs,
                        pos: i.pos + 1,
                    },
                )
            })
        }
        let mut state: S<'a, T, N, A> = S {
            states: vec![],
            item_sets: BTreeMap::new(),
            nubs: BTreeMap::new(),
        };
        let mut finished = 0;
        state.complete_nub(
            self,
            ItemSet {
                items: {
                    let mut r = BTreeSet::new();
                    r.insert(Item {
                        lhs: &self.start,
                        rhs: &self.rules.get(&self.start).unwrap()[0],
                        pos: 0,
                    });
                    r
                },
            },
        );
        while finished < state.states.len() {
            let mut next_nubs = BTreeMap::new();
            for item in state.states[finished].0.items.iter() {
                if let Some((sym, next)) = advance(item) {
                    next_nubs.entry(sym).or_insert(BTreeSet::new()).insert(next);
                }
            }
            for (sym, items) in next_nubs.into_iter() {
                let ix = state.complete_nub(self, ItemSet { items });
                state.states[finished].1.insert(sym, ix);
            }
            finished += 1;
        }
        LR0StateMachine {
            states: state.states,
            start: &self.start,
        }
    }

    /// Compute the FIRST sets of the grammar.
    /// Returns a map from nonterminal to (first set, nullable).
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
                            Nonterminal(ref n) => {
                                if n == lhs {
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
                                }
                            }
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

    /// Compute the FOLLOW sets of the grammar.
    /// Returns a map mapping from nonterminal to (follow set, whether follow set contains EOF)
    pub fn follow_sets<'a>(
        &'a self,
        first: BTreeMap<&'a N, (BTreeSet<&'a T>, bool)>,
    ) -> BTreeMap<&'a N, (BTreeSet<&'a T>, bool)> {
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
                                follow.0.extend(f.iter().copied());
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

    /// Try to create an LALR(1) parse table out of the grammar.
    ///
    /// You can tweak the behaviour of the parser in two ways:
    ///
    /// - `reduce_on` is a predicate, allowing you to control certain reduce rules based on the
    ///   lookahead token. This function takes two parameters: the rule, given by its right-hand
    ///   side, and the lookahead token (or `None` for EOF). You can use this to resolve
    ///   shift-reduce conflicts. For example, you can solve the "dangling else" problem by
    ///   forbidding the reduce action on an `else` token.
    /// - `priority_of` allows you to resolve reduce-reduce conflicts, by giving reduce rules
    ///   different "priorities". This takes the same parameters as `reduce_on`, so you can vary
    ///   the priority based on the lookahead token. If there would be a reduce-reduce conflict
    ///   between rules, but they have different priority, the one with higher priority is used.
    pub fn lalr1(
        &self,
        config: &impl Config<T, N, A>,
    ) -> Result<LR1ParseTable<'_, T, N, A>, LR1Conflict<'_, T, N, A>> {
        let state_machine = self.lr0_state_machine();
        let extended = state_machine.extended_grammar();
        let first_sets = extended.first_sets();
        let follow_sets = extended.follow_sets(first_sets);
        let mut r = LR1ParseTable {
            states: state_machine
                .states
                .iter()
                .map(|_| LR1State {
                    eof: None,
                    lookahead: BTreeMap::new(),
                    goto: BTreeMap::new(),
                })
                .collect(),
        };

        // add shifts
        for (i, (_, trans)) in state_machine.states.iter().enumerate() {
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
        for ((&(start_state, lhs), rhss), (&&(s2, l2), &(ref follow, eof))) in
            extended.rules.iter().zip(follow_sets.iter())
        {
            debug_assert_eq!(start_state, s2);
            debug_assert!(lhs == l2);

            for &Rhs {
                syms: _,
                act: (end_state, rhs),
            } in rhss.iter()
            {
                for &&t in follow.iter().filter(|&&&t| config.reduce_on(rhs, Some(t))) {
                    match r.states[end_state].lookahead.entry(t) {
                        btree_map::Entry::Vacant(v) => {
                            v.insert(LRAction::Reduce(lhs, rhs));
                        }
                        btree_map::Entry::Occupied(mut v) => {
                            match *v.get_mut() {
                                LRAction::Reduce(l, r) if l == lhs && std::ptr::eq(r, rhs) => {
                                    // The cells match, so there's no conflict.
                                }
                                LRAction::Reduce(ref mut l, ref mut r) => {
                                    match config
                                        .priority_of(r, Some(t))
                                        .cmp(&config.priority_of(rhs, Some(t)))
                                    {
                                        cmp::Ordering::Greater => {
                                            // `r` overrides `rhs` - do nothing.
                                        }
                                        cmp::Ordering::Less => {
                                            // `rhs` overrides `r`.
                                            *l = lhs;
                                            *r = rhs;
                                        }
                                        cmp::Ordering::Equal => {
                                            // Otherwise, we have a reduce/reduce conflict.
                                            return Err(LR1Conflict::ReduceReduce {
                                                state: state_machine.states[end_state].0.clone(),
                                                token: Some(t),
                                                r1: (*l, *r),
                                                r2: (lhs, rhs),
                                            });
                                        }
                                    }
                                }
                                LRAction::Shift(_) => {
                                    return Err(LR1Conflict::ShiftReduce {
                                        state: state_machine.states[end_state].0.clone(),
                                        token: Some(t),
                                        rule: (lhs, rhs),
                                    });
                                }
                                LRAction::Accept => {
                                    unreachable!();
                                }
                            }
                        }
                    }
                }

                if eof && config.reduce_on(rhs, None) {
                    let state = &mut r.states[end_state];
                    if *lhs == self.start {
                        if state.eof.is_some() {
                            unreachable!()
                        }
                        state.eof = Some(LRAction::Accept);
                    } else {
                        match state.eof {
                            Some(LRAction::Reduce(l, r)) if l == lhs && std::ptr::eq(r, rhs) => {
                                // no problem
                            }
                            Some(LRAction::Reduce(ref mut l, ref mut r)) => {
                                match config
                                    .priority_of(r, None)
                                    .cmp(&config.priority_of(rhs, None))
                                {
                                    cmp::Ordering::Greater => {
                                        // `r` overrides `rhs` - do nothing.
                                    }
                                    cmp::Ordering::Less => {
                                        // `rhs` overrides `r`.
                                        *l = lhs;
                                        *r = rhs;
                                    }
                                    cmp::Ordering::Equal => {
                                        // We have a reduce/reduce conflict.
                                        return Err(LR1Conflict::ReduceReduce {
                                            state: state_machine.states[end_state].0.clone(),
                                            token: None,
                                            r1: (*l, *r),
                                            r2: (lhs, rhs),
                                        });
                                    }
                                }
                            }
                            Some(LRAction::Shift(_)) => {
                                return Err(LR1Conflict::ShiftReduce {
                                    state: state_machine.states[end_state].0.clone(),
                                    token: None,
                                    rule: (lhs, rhs),
                                });
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

        Ok(r)
    }
}

type StateNonterminal<'a, N> = (usize, &'a N);
type StateRhs<'a, T, N, A> = (usize, &'a Rhs<T, N, A>);
type StateRule<'a, T, N, A> = Rhs<&'a T, StateNonterminal<'a, N>, StateRhs<'a, T, N, A>>;
type ExtendedRules<'a, T, N, A> = BTreeMap<StateNonterminal<'a, N>, Vec<StateRule<'a, T, N, A>>>;
type ExtendedGrammar<'a, T, N, A> = Grammar<&'a T, StateNonterminal<'a, N>, StateRhs<'a, T, N, A>>;

impl<'a, T: Ord, N: Ord, A> LR0StateMachine<'a, T, N, A> {
    /// Create an LALR(1) extended grammar, as described
    /// [here](https://web.archive.org/web/20211216015406/https://web.cs.dal.ca/~sjackson/lalr1.html).
    pub fn extended_grammar(&self) -> ExtendedGrammar<'a, T, N, A> {
        let mut r: ExtendedRules<'a, T, N, A> = BTreeMap::new();
        for (ix, (iset, _)) in self.states.iter().enumerate() {
            for item in iset.items.iter() {
                if item.pos == 0 {
                    let new_lhs = (ix, item.lhs);
                    let mut state = ix;
                    let new_rhs = Rhs {
                        syms: item
                            .rhs
                            .syms
                            .iter()
                            .map(|sym| {
                                let old_st = state;
                                state = *self.states[old_st].1.get(sym).unwrap();
                                match *sym {
                                    Terminal(ref t) => Terminal(t),
                                    Nonterminal(ref n) => {
                                        let nt = (old_st, n);
                                        if let btree_map::Entry::Vacant(view) = r.entry(nt) {
                                            view.insert(vec![]);
                                        }
                                        Nonterminal(nt)
                                    }
                                }
                            })
                            .collect(),
                        act: (state, item.rhs),
                    };
                    r.entry(new_lhs).or_default().push(new_rhs);
                }
            }
        }
        Grammar {
            rules: r,
            start: (0, self.start),
        }
    }
}

impl<'a, T: Debug, N: Debug, A> LR0StateMachine<'a, T, N, A> {
    /// Print the state machine in graphviz format.
    pub fn print(&self) {
        println!(
            r#"
digraph G {{
    node [
        shape="box",
        style="rounded",
        penwidth=1,
        width=2.0
    ];"#
        );
        for (i, (iset, trans)) in self.states.iter().enumerate() {
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
                println!("<br />");
            }
            println!(">]");
            for (sym, &target) in trans.iter() {
                println!("s{} -> s{} [label=<{:?}>]", i, target, sym);
            }
        }
        println!("}}");
    }
}
