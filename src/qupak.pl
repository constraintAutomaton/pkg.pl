% Qupak: Pattern Matching for library(reif)
%
% This library provides predicates to do pattern matching in a way that complements and
% expands on library(reif).
%
% Tested on Scryer Prolog 0.10.

% === Pattern matching ===
%
% The core predicate of this library is pattern_match_t/3. It has a non-reified version
% pattern_match/2, along with predicate aliases pattern_match_rev/2-3 (value first order).
%
% The pattern consists of a term that is a clean representation of what you want to match.
% You always need to specify how a term will be matched with a wrapping functor.

% --- ground: Ground term matching ---
%
% Matches a ground term. Doesn't need to be atomic.
%
% Examples:
%   ?- pattern_match_rev(a(1,2,3), ground(a(1,2,3))).
%      true.
%   ?- pattern_match_rev(a(1,2,3), ground(a(1,2,3)), T).
%      T = true.

% --- bind: Bind variable ---
%
% Binds a variable. This does normal unification, not reified unification like check.
%
% Examples:
%   ?- pattern_match_rev(a(1,2,3), bind(X)).
%      X = a(1,2,3).
%   ?- pattern_match_rev(a(1,2,3), bind(X), T).
%      X = a(1,2,3), T = true.

% --- check: Reified unification ---
%
% Does reified unification. If used in a non-reified predicate, it's basically the same
% thing as bind, but creates a spurious choice point. When used in a reified
% predicate, does the unification like =/3, exploring the dif/2 case on backtracking.
%
% Examples:
%   ?- pattern_match_rev(a(1,2,3), check(X)).
%      X = a(1,2,3)
%   ;  false.
%   ?- pattern_match_rev(a(1,2,3), check(X), T).
%      X = a(1,2,3), T = true
%   ;  T = false, dif:dif(X,a(1,2,3)).

% --- any: Wildcard ---
%
% Matches anything, and ignores it.
%
% Examples:
%   ?- pattern_match_rev(a(1,2,3), any).
%      true.
%   ?- pattern_match_rev(a(1,2,3), any, T).
%      T = true.

% --- composite: Compound term ---
%
% Matches a compound term, applying patterns recursively. If the compound term is ground,
% you can use ground instead.
%
% Examples:
%   ?- pattern_match_rev(a(1,2,3,4), composite(a(ground(1), bind(A), check(B), any))).
%      A = 2, B = 3
%   ;  false.
%   ?- pattern_match_rev(a(1,2,3,4), composite(a(ground(1), bind(A), check(B), any)), T).
%      A = 2, B = 3, T = true
%   ;  A = 2, T = false, dif:dif(B,3).

% --- bind_all: All binding ---
%
% Binds the whole pattern to a variable. It can be used at any nesting level, not just
% at the top level.
%
% Examples:
%   ?- pattern_match_rev(a(1,2,3,4), bind_all(All, composite(a(ground(1), bind(A), check(B), any)))).
%      All = a(1,2,3,4), A = 2, B = 3
%   ;  false.
%   ?- pattern_match_rev(a(1,2,3,4), bind_all(All, composite(a(ground(1), bind(A), check(B), any))), T).
%      All = a(1,2,3,4), A = 2, B = 3, T = true
%   ;  All = a(1,2,3,4), A = 2, T = false, dif:dif(B,3).

% --- guard: Guards ---
%
% Runs a reified predicate after the bindings to decide if the pattern matches.
%
% Examples:
%   ?- use_module(library(clpz)).
%      true.
%   ?- pattern_match_rev(a(50), guard(composite(a(bind(N))), clpz_t(N #< 25))).
%      false.
%   ?- pattern_match_rev(a(10), guard(composite(a(bind(N))), clpz_t(N #< 25))).
%      N = 10.
%   ?- pattern_match_rev(a(50), guard(composite(a(bind(N))), clpz_t(N #< 25)), T).
%      N = 50, T = false.
%   ?- pattern_match_rev(a(10), guard(composite(a(bind(N))), clpz_t(N #< 25)), T).
%      N = 10, T = true.

% === match/2: Reified pattern matching switch ===
%
% The match/2 predicate implements something like a switch/match statement on top of
% if_/3 and the pattern matching predicates from this library. The arms are matched in
% order, and the first one to succeed is the one taken.
%
% When a check pattern backtracks and tries the dif/2 case, it will test the next
% arms in the match/2. The dif/2 constraint will be available for all the arms
% from there on.
%
% Examples:
%   example_match(Term, Out) :-
%       match(Term, [
%           arm(ground(a), (Out = 1)),
%           arm(composite(b(bind(N))), (Out = N)),
%           arm(composite(c(check(N))), (Out = N)),
%           arm(any, (Out = wildcard))
%       ]).
%
%   ?- example_match(a, Out).
%      Out = 1.
%   ?- example_match(b(10), Out).
%      Out = 10.
%   ?- example_match(c(10), Out).
%      Out = 10
%   ;  Out = wildcard, dif:dif(_A,10).

% === Migration from operator version ===
%
% Old operator syntax -> New predicate syntax:
%   +Term              -> ground(Term)
%   -Var               -> bind(Var)
%   ?Var               -> check(Var)
%   *                  -> any
%   \Structure         -> composite(Structure)
%   All << Pattern     -> bind_all(All, Pattern)
%   Pattern | Guard    -> guard(Pattern, Guard)
%   Pattern ~> Goal    -> arm(Pattern, Goal)
%   Value =~ Pattern   -> pattern_match_rev(Value, Pattern)
%   Pattern ~= Value   -> pattern_match(Pattern, Value)

:- use_module(library(reif)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).

% === Helper predicates ===

and(true, true, true).
and(true, false, false).
and(false, true, false).
and(false, false, false).

% === Pattern matching predicates ===

:- meta_predicate(pattern_match_rev(?,:)).
:- meta_predicate(pattern_match_rev(?,:,?)).
:- meta_predicate(pattern_match(:,?)).
:- meta_predicate(pattern_match(:,?,?)).
:- meta_predicate(pattern_match_t(:,?,?)).

%% pattern_match_rev(Value, Pattern) - emulates Value =~ Pattern
pattern_match_rev(Value, Pattern, T) :-
    pattern_match_t(Pattern, Value, T).
pattern_match_rev(Value, Pattern) :-
    pattern_match_t(Pattern, Value, true).

%% pattern_match(Pattern, Value) - emulates Pattern ~= Value
pattern_match(Pattern, Value, T) :-
    pattern_match_t(Pattern, Value, T).
pattern_match(Pattern, Value) :-
    pattern_match_t(Pattern, Value, true).

%% pattern_match_t(Pattern, Value, T).
%
% A predicate to check if a value matches a certain pattern.
% The pattern is a specially constructed term that describes
% a pattern so that this can be monotonic.
pattern_match_t(Module:Pattern, Value, T) :-
    % TODO: Error handling of pattern arity.
    Pattern =.. [PatternKind|PatternArgs],
    inner_pattern_match(PatternKind, PatternArgs, Value, Module, T).

% Pattern constructors - emulate the symbol operators
% any - emulates *
inner_pattern_match(any, [], _, _, true).

% bind - emulates -
inner_pattern_match(bind, [Variable], Value, _, true) :-
    Variable = Value.

% check - emulates ?
inner_pattern_match(check, [Variable], Value, _, T) :-
    =(Variable, Value, T).

% ground - emulates +
inner_pattern_match(ground, [Ground], Value, _, T) :-
    % TODO: Error handling
    ground(Ground),
    =(Ground, Value, T).

% composite - emulates \
inner_pattern_match(composite, [Composite], Value, Module, T) :-
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

% bind_all - emulates
inner_pattern_match(bind_all, [Variable, Pattern], Value, Module, T) :-
    Variable = Value,
    pattern_match_t(Module:Pattern, Value, T).

% guard - emulates | in patterns
inner_pattern_match(guard, [Pattern, Guard_1], Value, Module, T) :-
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

%% match(Term, Arms)
% Pattern matching with multiple arms.
% Arms should be a list of arm(Pattern, Goal) terms
match(Term, Module:Arms) :-
    match_run(Arms, Module, Term).

match_run([], _, _) :- false.
match_run([arm(Pattern, Goal_0)|Arms], Module, Term) :-
    if_(
        pattern_match_rev(Term, Pattern),
        call(Module:Goal_0),
        match_run(Arms, Module, Term)
    ).
