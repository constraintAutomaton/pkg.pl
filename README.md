# pkg.pl: An experimental package manager for Scryer Prolog

This project is intended to be a testing ground for how to do a package manager
for Scryer Prolog. This is still really rough (currently it has trivially
exploitable arbitrary remote code execution) and is not intended for general
use yet. If you want to contribute or have any ideas or questions feel free to
get in touch and create issues, pull requests and discussions.

## How to use

A package is a directory with a `scryer-manifest.pl`, the current schema is something like:

```prolog
name("name_of_the_package").
% Optional. The file that will be imported when this package is used.
main_file("main.pl")
% Optional
dependencies([
    % A git url to clone
    git("https://github.com/bakaq/testing.pl.git")
]).
```

Copy the `pkg.pl` file into your project. It is both the dependency manager and
the package loader. Use `scryer-prolog pkg.pl -g pkg_install,halt` to download
the dependencies to a `scryer_libs` directory (it doesn't handle transitive
dependencies yet). You can then import packages in your code as follows:

```prolog
% Loads the package loader
:- use_module(pkg).

% Loads a package. the argument should be an atom equal to the name of the
% dependency package specified in the `name/1` field of its manifest.
:- use_module(pkg(testing)).

% You can then use the predicates exported by the main file of the dependency
% in the rest of the program.

main :-
    % `run_tests/0` is exported by `pkg(testing)`
    run_tests,
    halt.

test("Example test", (true)).
test("Another test", (false)).
```
