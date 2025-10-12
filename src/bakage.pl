/*bin/sh -c true

set -eu

type scryer-prolog > /dev/null 2> /dev/null \
    && exec scryer-prolog -f -g "bakage:run" "$0" -- "$@"

echo "No known supported Prolog implementation available in PATH."
echo "Try to install Scryer Prolog."
exit 1
#*/

/* SPDX-License-Identifier: Unlicense */

:- module(bakage, [pkg_install/1]).

:- use_module(library(os)).
:- use_module(library(pio)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(reif)).
:- use_module(library(iso_ext)).
:- use_module(library(debug)).
% === Dev import start ===
:- use_module('cli.pl').
:- use_module('script.pl').
% === Dev import end ===

% Cleanly pass arguments to a script through environment variables
run_script_with_args(ScriptName, Args, Success) :-
    maplist(define_script_arg, Args),
    append(["sh scryer_libs/scripts/", ScriptName, ".sh"], Script),
    (
        shell(Script) ->
            Success = true
        ;   Success = false
    ),
    maplist(undefine_script_arg, Args).

define_script_arg(Arg-Value) :- setenv(Arg, Value).
undefine_script_arg(Arg-_) :- unsetenv(Arg).

scryer_path(ScryerPath) :-
    (   getenv("SCRYER_PATH", ScryerPath) ->
        true
    ;   ScryerPath = "scryer_libs"
    ).

% the message sent to the user when a dependency is malformed
user_message_malformed_dependency(D, Error):-
    current_output(Out),
    phrase_to_stream((portray_clause_(D), "is malformed: ", Error, "\n"), Out).

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


% A prolog file knowledge base represented as a list of terms
prolog_kb_list(Stream) --> {read(Stream, Term), dif(Term, end_of_file)}, [Term], prolog_kb_list(Stream).
prolog_kb_list(Stream) --> {read(Stream, Term), Term == end_of_file}, [].

parse_manifest(Filename, Manifest) :-
    setup_call_cleanup(
        open(Filename, read, Stream),
        once(phrase(prolog_kb_list(Stream), Manifest)),
        close(Stream)
    ).

% pkg depedencies associated with the corresponding physical module
user:term_expansion((:- use_module(pkg(Package))), (:- use_module(PackageMainFile))) :-
    atom_chars(Package, PackageChars),
    scryer_path(ScryerPath),
    append([ScryerPath, "/packages/", PackageChars], PackagePath),
    append([PackagePath, "/", "scryer-manifest.pl"], ManifestPath),
    parse_manifest(ManifestPath, Manifest),
    member(main_file(MainFile), Manifest),
    append([PackagePath, "/", MainFile], PackageMainFileChars),
    atom_chars(PackageMainFile, PackageMainFileChars).

% This creates the directory structure we want
ensure_scryer_libs :-
    (   directory_exists("scryer_libs") ->
        true
    ;   make_directory_path("scryer_libs")
    ),
    (   directory_exists("scryer_libs/packages") ->
        true
    ;   make_directory_path("scryer_libs/packages")
    ),
    (   directory_exists("scryer_libs/scripts") ->
        true
    ;   make_directory_path("scryer_libs/scripts"),
        ensure_scripts
    ),
    (   directory_exists("scryer_libs/temp") ->
        true
    ;   make_directory_path("scryer_libs/temp")
    ).

% Installs helper scripts
ensure_scripts :-
    findall(ScriptName-ScriptString, script_string(ScriptName, ScriptString), Scripts),
    maplist(ensure_script, Scripts).

ensure_script(Name-String) :-
    append(["scryer_libs/scripts/", Name, ".sh"], Path),
    phrase_to_file(String, Path).


% Predicate to install the dependencies
pkg_install(Report) :-
        parse_manifest("scryer-manifest.pl", Manifest),
        ensure_scryer_libs,
        setenv("SHELL", "/bin/sh"),
        setenv("GIT_ADVICE", "0"),
        directory_files("scryer_libs/packages", Installed_Packages),
        (member(dependencies(Deps), Manifest) ->
            (
                phrase(valid_dependencies(Deps), Validation_Report),
                if_(all_dependencies_valid_t(Validation_Report),
                    call_cleanup(
                        (
                        logical_plan(Plan, Deps, Installed_Packages),
                        installation_execution(Plan, Installation_Report),
                        append(Validation_Report, Installation_Report, Report)
                        ),
                        delete_directory("scryer_libs/temp")
                    ),
                    (
                        Report = Validation_Report
                    )
                )
            );  Report = []
        ).

% A logical plan to install the dependencies
logical_plan(Plan, Ds, Installed_Packages) :-
    phrase(fetch_plan(Ds, Installed_Packages), Plan).

% A logical plan to fetch the dependencies
fetch_plan([], _) --> [].
fetch_plan([D|Ds], Installed_Packages) -->
    {fetch_step(D, Installation_Step, Installed_Packages)},
    [Installation_Step],
    fetch_plan(Ds, Installed_Packages).


% A step of a logical plan to fetch the dependencies
fetch_step(dependency(Name, DependencyTerm), Step, Installed_Packages) :-
    if_(memberd_t(Name, Installed_Packages),
        Step = do_nothing(dependency(Name, DependencyTerm)),
        Step = install_dependency(dependency(Name, DependencyTerm))
    ).

% Execute the physical installation of the dependencies
installation_execution(Plan, Results):-
    ensure_dependencies(Plan, Success),
    if_(Success = false,
        phrase(fail_installation(Plan), Results),
        true
    ),
    parse_install_report(Result_Report),
    phrase(installation_report(Plan, Result_Report), Results).

% All dependency installation failed
fail_installation([]) --> [].
fail_installation([P|Ps]) --> [P-error("installation script failed")], fail_installation(Ps).


% Parse the report of the installation of the dependencies
parse_install_report(Result_List) :-
    setup_call_cleanup(
        open("scryer_libs/temp/install_resp.pl", read, Stream),
        once(phrase(prolog_kb_list(Stream), Result_List)),
        (
            close(Stream),
            ( file_exists("scryer_libs/temp/install_resp.pl")->
                delete_file("scryer_libs/temp/install_resp.pl")
            ; true
            )
        )
    ).

% The installation report of the dependencies
installation_report([], _) --> [].
installation_report([P|Ps], Result_Report) -->
    { report_installation_step(P, Result_Report, R) },
    [R],
    installation_report(Ps, Result_Report).

% The result of a logical step
report_installation_step(do_nothing(dependency(Name, DependencyTerm)), _, do_nothing(dependency(Name, DependencyTerm))-success).


report_installation_step(install_dependency(dependency(Name, DependencyTerm)), ResultMessages, install_dependency(dependency(Name, DependencyTerm))-Message):-
    memberchk(result(Name, Message), ResultMessages).

% Execute the logical plan
ensure_dependencies(Logical_Plan, Success) :-
    phrase(physical_plan(Logical_Plan), Physical_Plan),
    Args = [
        "DEPENDENCIES_STRING"-Physical_Plan
    ],
    run_script_with_args("ensure_dependencies", Args, Success).


% Create a physical plan in shell script
physical_plan([]) --> [].
physical_plan([P|Ps]) --> physical_plan_([P|Ps]).

physical_plan_([P]) --> {
    physical_plan_step(P, El)
    },
    El.

physical_plan_([P|Ps]) --> {
    physical_plan_step(P, El)
    },
    El,
    "|",
    physical_plan_(Ps).

% Create a step for the shell script physical plan
physical_plan_step(do_nothing(dependency(Name, D)) , El) :-
    write_term_to_chars(D, [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=do_nothing"], El).

physical_plan_step(install_dependency(dependency(Name, git(Url))) ,El):-
    write_term_to_chars(git(Url), [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=git_default;git_url=", Url], El).

physical_plan_step(install_dependency(dependency(Name, git(Url,branch(Branch)))) ,El):-
    write_term_to_chars(git(Url,branch(Branch)), [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=git_branch;git_url=", Url, ";git_branch=", Branch], El).

physical_plan_step(install_dependency(dependency(Name, git(Url,tag(Tag)))) ,El):-
    write_term_to_chars(git(Url,tag(Tag)), [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=git_tag;git_url=", Url, ";git_tag=", Tag], El).

physical_plan_step(install_dependency(dependency(Name, git(Url,hash(Hash)))) ,El):-
    write_term_to_chars(git(Url,hash(Hash)), [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=git_hash;git_url=", Url, ";git_hash=", Hash], El).

physical_plan_step(install_dependency(dependency(Name, path(Path))) ,El):-
    write_term_to_chars(path(Path), [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=path;dependency_path=", Path], El).
