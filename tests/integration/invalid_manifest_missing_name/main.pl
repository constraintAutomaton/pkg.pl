:- use_module(bakage).
:- use_module('../../utils/testing.pl').
:- use_module('../../utils/assert.pl').
:- use_module(library(pio)).
:- use_module(library(format)).

main :-
    run_tests.

test("the package report is valid", (
        pkg_install(X),
        Expected = [
        validate_manifest_name-error("the 'name' of the package is not defined"),
        validate_manifest_main_file-success,
        validate_manifest_license-success,
        validate_dependencies-success
        ],
        test_eq(X, Expected)
    )
).
