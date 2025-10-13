:- module(my_module, [greet/1]).

greet(Name) :-
    write('Hello, '),
    write(Name),
    write('!'), nl.
