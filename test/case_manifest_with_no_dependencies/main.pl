:- use_module(pkg).
:- use_module('../util_packages/scryer_libs/packages/testing/testing.pl').
:- use_module(library(os)).

main :-
    shell("rm -rf scryer_libs && rm -f manifest-lock.pl"),
    run_tests,
    shell("rm -rf scryer_libs && rm -f manifest-lock.pl"), 
    halt.

test("test if no dependencies are installed", (pkg_install(X), X == [])).
