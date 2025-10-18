:- use_module(library(lists)).
:- use_module(library(reif)).
:- use_module(library(debug)).
:- use_module(library(files)).

is_list([]).
is_list([_|L]) :-
    is_list(L).

is_list_t(L, true):-
    is_list(L),!.

is_list_t(_, false).

file_exists_t(P, true):- file_exists(P),!.
file_exists_t(_, false).

is_license_t(license(name(N)), true):- is_list_t(N, true),!.
is_license_t(license(name(_)), false).

is_license_t(license(name(N), path(P)), true):- is_list_t(N, true), is_list_t(P, true), file_exists_t(P, true).
is_license_t(license(name(N), path(P)), false):- is_list_t(N, true), is_list_t(P, true), file_exists_t(P, false).
is_license_t(license(name(N), path(P)), false):- is_list_t(N, false), is_list_t(P, false).
is_license_t(license(name(N), path(P)), false):- is_list_t(N, true), is_list_t(P, false).
is_license_t(license(name(N), path(P)), false):- is_list_t(N, false), is_list_t(P, true).

% A valid manifest
valid_manifest_t(Manifest, Report, Valid) :-
    has_valid_name(Manifest, ValidName),
    has_valid_optional_main_file(Manifest, ValidMainFile),
    has_license(Manifest, ValidLicense),
    has_optional_dependencies(Manifest, ValidDendencies),
    if_((ValidName=success, ValidMainFile=success, ValidLicense=success, ValidDendencies=success),
        if_(memberd_t(dependencies(Deps), Manifest),
            (
                phrase(valid_dependencies(Deps), DepsReport),
                if_(all_dependencies_valid_t(DepsReport),
                    (
                        Report = [validate_manifest-success],
                        Valid=true
                    ),
                    (
                        Report = DepsReport,
                        Valid=false
                    )
                )
            ),
            (
                Report = [validate_manifest-success],
                Valid = true
            )

        ),
        (
            Report = [
                validate_manifest_name-ValidName,
                validate_manifest_main_file-ValidMainFile,
                validate_manifest_license-ValidLicense,
                validate_dependencies-ValidDendencies
                ],
            Valid = false
        )

    ).


has_a_field(S, FieldName, FieldTypePred, FieldTypeName, Optional, Result):-
    if_(S=[],
        if_(Optional = true,
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
            if_(R = true,
                if_(L = 1,
                    Result=success,
                    (
                        append(["the package has multiple '",FieldName, "'"  ], E),
                        Result=error(E)
                    )
                ),
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
    findall(license(name(N1)), member(license(name(N1)), Manifest), S1),
    findall(license(name(N2), path(P)), member(license(name(N2), path(P)), Manifest), S2),
    length(S1, L1),
    length(S2, L2),
    if_((L1=0, L2=0),
        Result=error("the license does not exist or respect the format 'license(Name)' or 'license(name(Name), path(Path))'"),
        if_((L1=1, L2=0),
            has_a_field(S1, "license", is_license_t, "license", false, Result),
            if_((L1=0, L2=1),
                has_a_field(S2, "license", is_license_t, "license with a valid path", false, Result),
                Result=error("the package has multiple 'license'")
            )
        )
    ).

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
