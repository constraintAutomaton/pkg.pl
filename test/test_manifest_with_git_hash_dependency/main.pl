:- use_module(pkg).
:- use_module('../util_packages/scryer_libs/packages/testing/testing.pl').
:- use_module(library(pio)).
:- use_module(library(format)).

main :-
    run_tests.

test("the package report is valid", (
        pkg_install(X),
        Expected = [
            validate_dependency(dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", hash("d19fefc1d7907f6675e181601bb9b8b94561b441"))))-success,
            install_dependency(dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", hash("d19fefc1d7907f6675e181601bb9b8b94561b441"))))-success
        ],
        (
            
            X == Expected
            ;
            (
                current_output(Out),
                phrase_to_stream(("expected: \n\n", portray_clause_(Expected), "\nbut got: \n\n", portray_clause_(X)), Out),
                false
            )
        )
    )
).