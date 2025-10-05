:- use_module(bakage).
:- use_module('../../utils/testing.pl').
:- use_module('../../utils/assert.pl').
:- use_module('../../utils/assert.pl').
:- use_module(library(ordsets)).
:- use_module(library(pio)).
:- use_module(library(format)).

main :-
    run_tests.
  
test("the package report is valid", (
        pkg_install(X),
        list_to_ord_set([
            lock_dependency(dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git")))-success,
            
            validate_dependency(dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git")))-success,
            validate_dependency(dependency("test_branch", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", branch("branch"))))-success,
            validate_dependency(dependency("test_tag", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", tag("tag"))))-success,
            validate_dependency(dependency("test_hash", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", hash("d19fefc1d7907f6675e181601bb9b8b94561b441"))))-success,
            validate_dependency(dependency("test_local", path("./local_package")))-success,

            install_dependency(dependency("test", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git")))-success,
            install_locked_dependency(dependency("test_branch",git("https://github.com/constraintAutomaton/test-prolog-package-manager.git",hash("cac8452114d5cd8aa75c6acbc6180a7d60bd24a6"))))-success,
            install_locked_dependency(dependency("test_tag",git("https://github.com/constraintAutomaton/test-prolog-package-manager.git",hash("41941e2af5b265d2cc98f04a05a7e652bd370d60"))))-success,
            install_locked_dependency(dependency("test_hash", git("https://github.com/constraintAutomaton/test-prolog-package-manager.git", hash("d19fefc1d7907f6675e181601bb9b8b94561b441"))))-success,
            install_dependency(dependency("test_local", path("./local_package")))-success
            ], Expected),
        list_to_ord_set(X, X_Set),
        test_eq(X_Set, Expected)
    )
).