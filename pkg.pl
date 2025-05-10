/* SPDX-License-Identifier: Unlicense */

:- module(pkg, [pkg_install/0]).

:- use_module(library(os)).
:- use_module(library(pio)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(debug)).

scryer_path(ScryerPath) :-
    (   getenv("SCRYER_PATH", ScryerPath) ->
        true
    ;   ScryerPath = "scryer_libs"
    ).

parse_manifest(Filename, Manifest) :-
    open(Filename, read, Stream),
    parse_manifest_(Stream, Manifest),
    close(Stream).

parse_manifest_(Stream, Manifest) :-
    read(Stream, Term),
    (   Term == end_of_file ->
        Manifest = []
    ;   Manifest = [Term|Ms],
        parse_manifest_(Stream, Ms)
    ).

user:term_expansion((:- use_module(pkg(Package))), UsePackage) :-
    atom_chars(Package, PackageChars),
    scryer_path(ScryerPath),
    append([ScryerPath, "/", PackageChars], PackagePath),
    append([PackagePath, "/", "scryer-manifest.pl"], ManifestPath),
    parse_manifest(ManifestPath, Manifest),
    member(main_file(MainFile), Manifest),
    append([PackagePath, "/", MainFile], PackageMainFileChars),
    atom_chars(PackageMainFile, PackageMainFileChars),
    UsePackage = (
        :- use_module(PackageMainFile)
    ).

pkg_install :-
    parse_manifest("scryer-manifest.pl", Manifest),
    (   directory_exists("scryer_libs") ->
        true
    ;   make_directory_path("scryer_libs")
    ),
    (   file_exists("manifest-lock.pl") ->
        parse_manifest("manifest-lock.pl", LockFile),
        member(lock_dependencies(LockDepsMat),LockFile)
    ;   LockDepsMat = []
    ),
    setenv("SHELL", "/bin/sh"),
    setenv("GIT_ADVICE", "0"),
    (   member(dependencies(Deps), Manifest) ->
        ensure_dependencies(Deps, LockDepsMat),
        ensure_lock_dependencies(Deps, LockDeps)
    ;   true
    ),
    materialize_lock_file(LockDeps).

materialize_lock_file(LockDeps) :-
    open('manifest-lock.pl', write, Stream),
    write(Stream, '% WARNING: This file is auto-generated. Do NOT modify it manually.\n\n'),
    write_term(Stream, lock_dependencies(LockDeps), [double_quotes(true)]),
    write(Stream, '.\n'),
    close(Stream).

lock_dependency_with_name([_|Ls], dependency(Name, _), Dep) :-
    lock_dependency_with_name(Ls, dependency(Name, _), Dep).

lock_dependency_with_name([dependency(Name, X)|_], dependency(Name, _), dependency(Name, X)).

ensure_dependencies([], _).
ensure_dependencies([D|Ds], Ls) :-
    (   lock_dependency_with_name(Ls, D, L) ->
        ensure_dependency(L)
    ;   ensure_dependency(D)
    ),
    ensure_dependencies(Ds, Ls).    

ensure_lock_dependencies([], []).

ensure_lock_dependencies([D|Ds], [L|Ls]) :-
    ensure_lock_dependency(D, L),
    ensure_lock_dependencies(Ds, Ls).

ensure_lock_dependency(dependency(PkgName, X), LockedDependencyTerm) :-
    atom_chars(PkgName, Name),
    append(["scryer_libs/", Name], Path),
    lock_dependency(dependency(PkgName, X), Path, LockedDependencyTerm).

ensure_dependency(dependency(PkgName, DependencyTerm)) :-
    current_output(Out),
    phrase_to_stream(("Ensuring is installed: ", portray_clause_(DependencyTerm)), Out),
    shell("rm --recursive --force scryer_libs/tmp"),
    % Hell yeah, injection attack!
    dependency_aq_command(DependencyTerm, Command),
    shell(Command),
    atom_chars(PkgName, Name),
    append(
        [
            "rm --recursive --force scryer_libs/", Name,
            "; mv scryer_libs/tmp scryer_libs/", Name
        ],
        Command2
    ),
    shell(Command2).

dependency_aq_command(git(X), Command) :- git_command(git(X), Command).
dependency_aq_command(git(X, Y), Command) :- git_command(git(X, Y), Command).
dependency_aq_command(path(X), Command) :- path_command(path(X), Command).

git_command(git(Url), Command) :-
    Segments = ["git clone --quiet --depth 1 --single-branch ", Url, " scryer_libs/tmp"],
    append(Segments, Command).

git_command(git(Url, branch(Branch)), Command) :-
    Segments = [
        "git clone --quiet --depth 1 --single-branch --branch ",
        Branch, " ", Url,
        " scryer_libs/tmp"
    ],
    append(Segments, Command).

git_command(git(Url, tag(Tag)), Command) :-
    git_command(git(Url, branch(Tag)), Command).

git_command(git(Url, hash(Hash)), Command) :-
    CloneCommand = ["git clone --quiet --depth 1 --single-branch ", Url, " scryer_libs/tmp "],
    GetHashCommitCommand = [" && cd scryer_libs/tmp  >/dev/null && git fetch --quiet --depth 1 origin ", Hash, " && git checkout --quiet ", Hash],
    append(CloneCommand, GetHashCommitCommand,  Segments), 
    append(Segments, Command).

path_command(path(Path), Command) :-
    Segments = ["ln -rs ", Path, " scryer_libs/tmp"],
    append(Segments, Command).

lock_dependency(dependency(Name, git(Url)), Path, LockDependencyTerm) :-
    append(["cd ", Path, " && git rev-parse HEAD"], Command),
    run_command(Command, temp_result(Hash)),
    LockDependencyTerm=dependency(Name, git(Url, hash(Hash))).

lock_dependency(dependency(Name, git(Url, tag(_))), Path, LockDependencyTerm) :-
    lock_dependency(dependency(Name, git(Url)), Path, LockDependencyTerm).

lock_dependency(dependency(Name, git(Url, branch(_))), Path, LockDependencyTerm) :-
    lock_dependency(dependency(Name, git(Url)), Path, LockDependencyTerm).

lock_dependency(dependency(Name, git(Url, hash(Hash))), _, dependency(Name, git(Url, hash(Hash)))).

lock_dependency(dependency(Name, path(Path)), _, dependency(Name, path(Path))).

/**
 A hack to get the result of a shell command.
*/
run_command(Command, Output) :-
    append(["echo \"temp_result(\\\"$(",Command, ")\\\").\" > temp"], Command2),
    shell(Command2),
    open('temp', read, Stream),
    read(Stream, Output),
    close(Stream),
    shell("rm temp").