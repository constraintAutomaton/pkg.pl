:- use_module('../../../../../bakage.pl').
:- use_module(pkg(testing)).
:- use_module('../my_module.pl').

test("greet succeeds", (
    greet('World')
)).
