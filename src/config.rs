//! This module provides a trait for the configuration of the parse table generation process.
//!
//! The user can implement this trait to provide a custom configuration.
//! The default configuration is provided by the default implementation of this trait.
//!
use crate::{ItemSet, LR1Conflict, LR1ResolvedConflict, LRConflictResolution, Rhs};

/// The trait for configuration.
pub trait Config<'a, T, N, A> {
    /// `resolve_shift_reduse_conflict_in_favor_of_shift` returns true if shift should be resolved
    /// in favor of shift.
    /// To mimic a behavior of Bison/Yacc, this method should return true.
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
    /// resulting parser will accept a wider range of grammars.
    /// Also, the parse table generation will return warnings for shift-reduce conflicts.
    fn resolve_shift_reduce_conflict_in_favor_of_shift(&self) -> bool {
        false
    }

    /// `warn_on_resolved_conflicts` returns `true` if a warnings should be emitted when
    /// reduce-reduce or shift-reduce conflicts are resolved.
    /// A reduce-reduce conflict is resolved by selecting the rule with the highest priority. This
    /// means that the methode `priority_of` should provide meaningfull values to resolve the
    /// conflicts in a deterministic way.
    /// A shift-reduce conflict is resolved by selecting the shift action. This can only be the case
    /// when `resolve_shift_reduse_conflict_in_favor_of_shift` returns true.
    ///
    /// Warnings are emmitted by calling the returned function with the resolved conflict.
    /// This configures the handling of warnings and gives the client full control over the way
    /// warnings are emitted.
    ///
    /// If this method returns `false`, no warnings are emitted when a conflict is resolved.
    /// This is the default behavior of this crate.
    fn warn_on_resolved_conflicts(&self) -> bool {
        false
    }

    /// `on_resolved_conflict` is called when a reduce-reduce or shift-reduce conflict is resolved.
    /// This method is called only when `warn_on_resolved_conflicts` returns `true`.
    fn on_resolved_conflict(&self, _conflict: LR1ResolvedConflict<'a, T, N, A>) {}

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
pub struct DefaultConfig<'a, T, N, A> {
    _phantom: std::marker::PhantomData<(T, N, A)>,
    _phantom2: std::marker::PhantomData<&'a ()>,
}

/// The implementation of the default configuration.
impl<'a, T, N, A> DefaultConfig<'a, T, N, A> {
    /// Create a new default configuration.
    pub fn new() -> Self {
        DefaultConfig {
            _phantom: std::marker::PhantomData,
            _phantom2: std::marker::PhantomData,
        }
    }
}

/// The default implementation of the configuration.
impl<'a, T, N, A> Default for DefaultConfig<'a, T, N, A> {
    fn default() -> Self {
        DefaultConfig::new()
    }
}

/// The implementation of the configuration trait for the default configuration.
impl<'a, T, N, A> Config<'a, T, N, A> for DefaultConfig<'a, T, N, A> {}

// -----------------------------------------------------------------------------------------------
// ConflictWarner
// -----------------------------------------------------------------------------------------------

/// A helper for issuing conflict warnings.
///
/// Holds a reference to the configuration.
///
/// The configuration is used to determine if and where warnings should be issued.
/// The conflict warner is used by the parse table generator to issue warnings for resolved
/// conflicts.
pub(crate) struct ConflictWarner<'a, T, N, A> {
    config: &'a dyn Config<'a, T, N, A>,
}

impl<'a, T, N, A> ConflictWarner<'a, T, N, A> {
    /// Create a new conflict warner.
    pub fn new(config: &'a dyn Config<'a, T, N, A>) -> Self {
        ConflictWarner { config }
    }

    /// Issue a warning for a resolved reduce-reduce conflict.
    /// The lifetime 'b is used to ensure that the state is not borrowed for longer than necessary.
    pub fn warn_shift_reduce<'b>(
        &self,
        state: &'b ItemSet<'a, T, N, A>,
        token: Option<&'a T>,
        rule: (&'a N, &'a Rhs<T, N, A>),
    ) where
        'a: 'b,
    {
        if self.config.warn_on_resolved_conflicts() {
            self.config.on_resolved_conflict(LR1ResolvedConflict {
                conflict: LR1Conflict::ShiftReduce {
                    state: state.clone(),
                    token,
                    rule,
                },
                applied_resolution: LRConflictResolution::ShiftOverReduce,
            });
        }
    }

    /// Issue a warning for a resolved reduce-reduce conflict.
    /// The lifetime 'b is used to ensure that the state is not borrowed for longer than necessary.
    pub fn warn_reduce_reduce<'b>(
        &self,
        state: &'b ItemSet<'a, T, N, A>,
        token: Option<&'a T>,
        r1: (&'a N, &'a Rhs<T, N, A>),
        r2: (&'a N, &'a Rhs<T, N, A>),
        applied_resolution: LRConflictResolution,
    ) where
        'a: 'b,
    {
        if self.config.warn_on_resolved_conflicts() {
            self.config.on_resolved_conflict(LR1ResolvedConflict {
                conflict: LR1Conflict::ReduceReduce {
                    state: state.clone(),
                    token,
                    r1,
                    r2,
                },
                applied_resolution,
            });
        }
    }
}
