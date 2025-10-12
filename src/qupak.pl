:- module(qupak,[
    (~=)/2,
    (~=)/3,
    (=~)/2,
    (=~)/3,
    pattern_match/2,
    pattern_match_t/3,
    match/2,
    op(500, fy, ?),
    op(1105, xfy, '|'),
    op(700, xfx, ~=),
    op(700, xfx, =~),
    op(950, xfx, ~>)
]).

:- use_module(library(reif)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).

% === Helper predicates ===

and(true, true, true).
and(true, false, false).
and(false, true, false).
and(false, false, false).

% === Pattern matching operators ===

:- meta_predicate(=~(?,:)).
:- meta_predicate(=~(?,:,?)).
:- meta_predicate(~=(:,?)).
:- meta_predicate(~=(:,?,?)).

=~(Value, Pattern, T) :-
    pattern_match_t(Pattern, Value, T).
=~(Value, Pattern) :-
    pattern_match_t(Pattern, Value, true).

~=(Pattern, Value, T) :-
    pattern_match_t(Pattern, Value, T).
~=(Pattern, Value) :-
    pattern_match_t(Pattern, Value, true).

% === Pattern matching predicates ===

:- meta_predicate(pattern_match(:,?)).
:- meta_predicate(pattern_match_t(:,?,?)).

%% pattern_match(Pattern, Value).
%
% See pattern_match_t.
pattern_match(Pattern, Value) :-
    pattern_match_t(Pattern, Value, true).

%% pattern_match_t(Pattern, Value, T).
%
% A predicate to check if a value matches a certain pattern.
% The pattern is an specially constructed term that describes
% a pattern so that this can be monotonic.
pattern_match_t(Module:Pattern, Value, T) :-
    % TODO: Error handling of pattern arity.
    Pattern =.. [PatternKind|PatternArgs],
    inner_pattern_match(PatternKind, PatternArgs, Value, Module, T).

% Ignore
inner_pattern_match(*, [], _, _, true).
% Variable (bind)
inner_pattern_match(-, [Variable], Value, _, true) :- Variable = Value.
% Variable (reif)
inner_pattern_match(?, [Variable], Value, _, T) :- =(Variable, Value, T).
% Ground
inner_pattern_match(+, [Ground], Value, _, T) :-
    % TODO: Error handling
    ground(Ground),
    =(Ground, Value, T).
% Composite
inner_pattern_match(\, [Composite], Value, Module, T) :-
    Composite =.. [Functor|Args],
    length(Args, Arity),
    functor(Value, ValueFunctor, ValueArity),
    if_(
        Functor = ValueFunctor,
        if_(
            Arity = ValueArity,
            (
                Value =.. [_|ValueArgs],
                pattern_match_arguments(Args, ValueArgs, Module, true, T)
            ),
            T = false
        ),
        T = false
    ).
% All bind
inner_pattern_match(<<, [Variable, Pattern], Value, Module, T) :-
    Variable = Value,
    pattern_match_t(Module:Pattern, Value, T).
% Guards
inner_pattern_match('|', [Pattern, Guard_1], Value, Module, T) :-
    if_(
        pattern_match_t(Module:Pattern, Value),
        call(Module:Guard_1, T),
        T = false
    ).

pattern_match_arguments([], [], _, T, T).
pattern_match_arguments([Pattern|Patterns], [Arg|Args], Module, T0, T) :-
    pattern_match_t(Module:Pattern, Arg, T1),
    and(T0, T1, T2),
    pattern_match_arguments(Patterns, Args, Module, T2, T).

% === match/2 predicate ===

:- meta_predicate(match(?,:)).

% Ugly, but works?
match(Term, Module:Arms) :-
    match_run(Arms, Module, Term).

match_run([], _, _) :- false.
match_run([Pattern ~> Goal_0|Arms], Module, Term) :-
    if_(
        Term =~ Pattern,
        call(Module:Goal_0),
        match_run(Arms, Module, Term)
    ).
