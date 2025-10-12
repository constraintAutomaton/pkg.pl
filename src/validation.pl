:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module('qupak.pl').
:- use_module(library(debug)).


is_list([]).
is_list([_|L]) :-
    is_list(L).

is_list_t(L, true):-
    is_list(L),!.

is_list_t(_, false).

% A valid manifest
valid_manifest(Manifest, Report) :-
    has_valid_name(Manifest, ValidName),
    has_valid_optional_main_file(Manifest, ValidMainFile),
    has_license(Manifest, ValidLicense),
    has_optional_dependencies(Manifest, ValidDendencies),


has_a_field(S, FieldName, FieldTypePred, FieldTypeName, Optional, Result):-
    if_(S=[],
        if_(Optional=true,
            Result = success,
            (
                append(["the '", FieldName, "' of the package is not defined" ], E),
                Result=error(E)
            )
        ),
        (
            length(S, L),
            S = [Field|_],
            call(FieldTypePred, Field, R),
            if_(R=true,
                match(L,[
                    (+1) ~> Result=success,
                    (*) ~> (
                        append(["the package has multiple '",FieldName, "'"  ], E),
                        Result=error(E)
                    )
                ]),
                (
                  append(["the field '",FieldName, "' does not have the type '",FieldTypeName,"'"], E),
                  Result=error(E)
                )
            )

        )
    ).

has_valid_name(Manifest, Result):-
    findall(N, member(name(N), Manifest), S),
    has_a_field(S, "name", is_list_t, "list", false, Result).

has_valid_optional_main_file(Manifest, Result) :-
    findall(N, member(main_file(N), Manifest), S),
    has_a_field(S, "main_file", is_list_t, "list", true, Result).

has_license(Manifest, Result) :-
    findall(N, member(license(N), Manifest), S),
    has_a_field(S, "license", is_list_t, "list", false, Result).

has_optional_dependencies(Manifest, Result):-
    findall(N, member(dependencies(N), Manifest), Deps),
    has_a_field(Deps, "dependencies", is_list_t, "list", true, Result).

% A valid dependency
valid_dependencies([]) --> [].

valid_dependencies([dependency(Name, path(Path))| Ds]) --> {
    if_(
        (memberd_t(';', Name)
        ; memberd_t('|', Name)
        ; memberd_t(';', Path)
        ; memberd_t('|', Path)
        ),
        (
            Error = "the name and the path of the dependency should not contain an \";\" or an \"|\" caracter",
            M = validate_dependency(dependency(Name, path(Path)))-error(Error),
            user_message_malformed_dependency(dependency(Name, path(Path)), Error)
        ),
        M = validate_dependency(dependency(Name, path(Path)))-success
    )
    },
    [M],
    valid_dependencies(Ds).

valid_dependencies([dependency(Name, git(Url))| Ds]) --> {
    if_(
        (memberd_t(';', Name)
            ; memberd_t('|', Name)
            ; memberd_t(';', Url)
            ; memberd_t('|', Url)
        ),
        (
            Error = "the name of the dependency and the url should not contain an \";\" or an \"|\" caracter",
            M = validate_dependency(dependency(Name, git(Url)))-error(Error),
            user_message_malformed_dependency(dependency(Name, git(Url)), Error)
        ),
         M = validate_dependency(dependency(Name, git(Url)))-success
    )
    },
    [M],
    valid_dependencies(Ds).

valid_dependencies([dependency(Name, git(Url, branch(Branch)))| Ds]) --> {
    if_(
        (memberd_t(';', Name)
        ; memberd_t('|', Name)
        ; memberd_t(';', Url)
        ; memberd_t('|', Url)
        ; memberd_t(';', Branch)
        ; memberd_t('|', Branch)),
        (
            Error = "the name, the url and the branch of dependency should not contain an \";\" or an \"|\" caracter",
            M = validate_dependency(dependency(Name, git(Url, branch(Branch))))-error(Error),
            user_message_malformed_dependency(dependency(Name, git(Url, branch(Branch))), Error)
        ),(
            M = validate_dependency(dependency(Name, git(Url, branch(Branch))))-success
        )
    )
    },
    [M],
    valid_dependencies(Ds).

valid_dependencies([dependency(Name, git(Url, tag(Tag)))|Ds]) --> {
    if_(
        (memberd_t(';', Name)
        ; memberd_t('|', Name)
        ; memberd_t(';', Url)
        ; memberd_t('|', Url)
        ; memberd_t(';', Tag)
        ; memberd_t('|', Tag)),
        (
            Error = "the name, the url and the tag of dependency should not contain an \";\" or an \"|\" caracter",
            M = validate_dependency(dependency(Name, git(Url, tag(Tag))))-error(Error),
            user_message_malformed_dependency(dependency(Name, git(Url, tag(Tag))), Error)
        ),
        M = validate_dependency(dependency(Name, git(Url, tag(Tag))))-success
    )
    },
    [M],
    valid_dependencies(Ds).

valid_dependencies([dependency(Name, git(Url, hash(Hash)))|Ds]) --> {
    if_(
        (memberd_t(';', Name)
        ; memberd_t('|', Name)
        ; memberd_t(';', Url)
        ; memberd_t('|', Url)
        ; memberd_t(';', Hash)
        ; memberd_t('|', Hash)),
        (
            Error = "the name, the url and the hash of dependency should not contain an \";\" or an \"|\" caracter",
            M = validate_dependency(dependency(Name, git(Url, hash(Hash))))-error(Error),
            user_message_malformed_dependency(dependency(Name, git(Url, hash(Hash))), Error)
        ),
        M = validate_dependency(dependency(Name, git(Url, hash(Hash))))-success
    )
    },
    [M],
    valid_dependencies(Ds).

all_dependencies_valid_t([], true).
all_dependencies_valid_t([validate_dependency(_)-success| Vs], T) :-  all_dependencies_valid_t(Vs, T).
all_dependencies_valid_t([validate_dependency(_)-error(_)| _], false).
