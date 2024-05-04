//! This module provides a trait for the configuration of the parse table generation process.
//!
//! The user can implement this trait to provide a custom configuration.
//! The default configuration is provided by the default implementation of this trait.
//!
use crate::Rhs;

/// The trait for configuration.
pub trait Config<T, N, A> {
    /// `resolve_shift_reduse_conflict_in_favor_of_shift` returns true if shift should be resolved
    /// in favor of shift.
    /// To mimic a behavior of Yacc, this method should return true.
    /// See, https://www.gnu.org/savannah-checkouts/gnu/bison/manual/html_node/Shift_002fReduce.html
    /// and https://www.ibm.com/docs/en/zos/2.2.0?topic=ambiguities-rules-help-remove
    /// for more information.
    ///
    /// If this method returns false, shift-reduce conflict is not resolved and the parse table
    /// generation will fail with an error.
    /// This is the default behavior of this create.
    /// It also means that the parser will only accept pure LALR(1) grammars.
    ///
    /// If this method returns true, shift-reduce conflict is resolved in favor of shift and the
    /// parser will accept a wider range of grammars.
    /// Also, the parse table generation will return warnings for shift-reduce conflicts.
    fn resolve_shift_reduse_conflict_in_favor_of_shift(&self) -> bool {
        false
    }

    /// `reduce_on` is a predicate, allowing you to control certain reduce rules based on the
    /// lookahead token. This function takes two parameters: the rule, given by its right-hand
    /// side, and the lookahead token (or `None` for EOF). You can use this to resolve
    /// shift-reduce conflicts. For example, you can solve the "dangling else" problem by
    /// forbidding the reduce action on an `else` token.
    fn reduce_on(&self, _rhs: &Rhs<T, N, A>, _lookahead: Option<&T>) -> bool {
        true
    }

    /// `priority_of` allows you to resolve reduce-reduce conflicts, by giving reduce rules
    /// different "priorities". This takes the same parameters as `reduce_on`, so you can vary
    /// the priority based on the lookahead token. If there would be a reduce-reduce conflict
    /// between rules, but they have different priority, the one with higher priority is used.
    fn priority_of(&self, _rhs: &Rhs<T, N, A>, _lookahead: Option<&T>) -> i32 {
        0
    }
}

/// The default configuration.
pub struct DefaultConfig<T, N, A> {
    _phantom: std::marker::PhantomData<(T, N, A)>,
}

/// The implementation of the default configuration.
impl<T, N, A> DefaultConfig<T, N, A> {
    /// Create a new default configuration.
    pub fn new() -> Self {
        DefaultConfig {
            _phantom: std::marker::PhantomData,
        }
    }
}

/// The default implementation of the configuration.
impl<T, N, A> Default for DefaultConfig<T, N, A> {
    fn default() -> Self {
        DefaultConfig::new()
    }
}

impl<T, N, A> Config<T, N, A> for DefaultConfig<T, N, A> {}
