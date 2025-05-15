/* SPDX-License-Identifier: Unlicense */

:- module(pkg, [pkg_install/0]).

:- use_module(library(os)).
:- use_module(library(pio)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(debug)).

dependency_directory_name("scryer_libs").
manifest_file_name("scryer-manifest.pl").
lock_file_name("manifest-lock.pl").

% Cleanly pass arguments to a script through environment variables
run_script_with_args(ScriptName, Args) :-
    maplist(define_script_arg, Args),
    append(["sh scryer_libs/scripts/", ScriptName, ".sh"], Script),
    shell(Script),
    maplist(undefine_script_arg, Args).

define_script_arg(Arg-Value) :- setenv(Arg, Value).
undefine_script_arg(Arg-_) :- unsetenv(Arg).

scryer_path(ScryerPath) :-
    (   getenv("SCRYER_PATH", ScryerPath) ->
        true
    ;   dependency_directory_name(ScryerPath)
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
    append([ScryerPath, "/packages/", PackageChars], PackagePath),
    append([PackagePath, "/", "scryer-manifest.pl"], ManifestPath),
    parse_manifest(ManifestPath, Manifest),
    member(main_file(MainFile), Manifest),
    append([PackagePath, "/", MainFile], PackageMainFileChars),
    atom_chars(PackageMainFile, PackageMainFileChars),
    UsePackage = (
        :- use_module(PackageMainFile)
    ).

% This creates the directory structure we want
ensure_scryer_libs :-
    (   directory_exists("scryer_libs") ->
        true
    ;   make_directory_path(DependencyDirectoryName)
    ),
    * (   file_exists(LockFileFileName) ->
        parse_manifest(LockFileFileName, LockFile),
        member(lock_dependencies(LockDepsMat),LockFile)
    ;   LockDepsMat = []
    ),
    (   directory_exists("scryer_libs/packages") ->
        true
    ;   make_directory_path("scryer_libs/packages")
    ),
    (   directory_exists("scryer_libs/scripts") ->
        true
    ;   make_directory_path("scryer_libs/scripts"),
        ensure_scripts
    ).

% Installs helper scripts
ensure_scripts :-
    findall(ScriptName-ScriptString, script_string(ScriptName, ScriptString), Scripts),
    maplist(ensure_script, Scripts).

ensure_script(Name-String) :-
    append(["scryer_libs/scripts/", Name, ".sh"], Path),
    phrase_to_file(String, Path).

pkg_install :-
    parse_manifest("scryer-manifest.pl", Manifest),
    ensure_scryer_libs,
    setenv("SHELL", "/bin/sh"),
    setenv("GIT_ADVICE", "0"),
    (   member(dependencies(Deps), Manifest) ->
        %ensure_lock_dependencies(Deps, LockDeps),
        maplist(ensure_dependency, Deps)
    ;   true
    ).

materialize_lock_file(LockDeps) :-
    lock_file_name(LockFileFileName),
    open(LockFileFileName, write, Stream),
    write(Stream, '% WARNING: This file is auto-generated. Do NOT modify it manually.\n\n'),
    write_term(Stream, lock_dependencies(LockDeps), [double_quotes(true)]),
    write(Stream, '.\n'),
    close(Stream).

dependency(_, _, _).
lock_dependencies(_).

lock_dependency_with_name([dependency(Name, X, _)|_], dependency(Name, _), dependency(Name, X)) :- !.

lock_dependency_with_name([_|Ls], dependency(Name, _), Dep) :-
    lock_dependency_with_name(Ls, dependency(Name, _), Dep). 

ensure_lock_dependencies([], []).

ensure_lock_dependencies([D|Ds], [L|Ls]) :-
    ensure_lock_dependency(D, L),
    ensure_lock_dependencies(Ds, Ls).

ensure_lock_dependency(dependency(PkgName, X), LockedDependencyTerm) :-
    lock_dependency(dependency(PkgName, X), LockedDependencyTerm).

ensure_dependency(dependency(Name, DependencyTerm)) :-
    write_term_to_chars(DependencyTerm, [quoted(true), double_quotes(true)], DependencyTermChars),
    CommonArgs = [
        "DEPENDENCY_NAME"-Name,
        "DEPENDENCY_TERM"-DependencyTermChars
    ],
    ensure_dependency_extra_args(DependencyTerm, ExtraArgs),
    append(CommonArgs, ExtraArgs, Args),
    run_script_with_args("ensure_dependency", Args).

ensure_dependency_extra_args(git(Url), [
    "DEPENDENCY_KIND"-"git_default",
    "GIT_URL"-Url
]).
ensure_dependency_extra_args(git(Url,branch(Branch)), [
    "DEPENDENCY_KIND"-"git_branch",
    "GIT_URL"-Url,
    "GIT_BRANCH"-Branch
]).
ensure_dependency_extra_args(git(Url,tag(Tag)), [
    "DEPENDENCY_KIND"-"git_tag",
    "GIT_URL"-Url,
    "GIT_TAG"-Tag
]).
ensure_dependency_extra_args(git(Url,hash(Hash)), [
    "DEPENDENCY_KIND"-"git_hash",
    "GIT_URL"-Url,
    "GIT_HASH"-Hash
]).
ensure_dependency_extra_args(path(Path), [
    "DEPENDENCY_KIND"-"path",
    "DEPENDENCY_PATH"-Path
]).

lock_dependency(dependency(PkgName, git(Url)), LockDependencyTerm) :-
    dependency_directory_name(DF),
    atom_chars(PkgName, Name),
    append(["cd ", DF,"/", Name, " && git rev-parse HEAD"], GitHashCmd),
    append(["find ",DF,"/", Name, " -type f -print0 | sort -z | xargs -0 sha1sum | sha1sum | awk '{print $1}'"], IntegrityHashCmd),
    run_command(GitHashCmd, temp_result(Hash)),
    run_command(IntegrityHashCmd, temp_result(IntegrityHash)),
    LockDependencyTerm=dependency(PkgName, git(Url, hash(Hash)), IntegrityHash).

lock_dependency(dependency(Name, git(Url, tag(_))), LockDependencyTerm) :-
    lock_dependency(dependency(Name, git(Url)), LockDependencyTerm).

lock_dependency(dependency(Name, git(Url, branch(_))), LockDependencyTerm) :-
    lock_dependency(dependency(Name, git(Url)), LockDependencyTerm).

lock_dependency(dependency(PkgName, git(Url, hash(Hash))), LockDependencyTerm):-
    dependency_directory_name(DF),
    atom_chars(PkgName, Name),
    append(["find ",DF,"/", Name, " -type f -print0 | sort -z | xargs -0 sha1sum | sha1sum | awk '{print $1}'"], IntegrityHashCmd),
    run_command(IntegrityHashCmd, temp_result(IntegrityHash)),
    LockDependencyTerm=dependency(PkgName, git(Url, hash(Hash)), IntegrityHash).

lock_dependency(dependency(PkgName, path(Path)), LockDependencyTerm) :-
    dependency_directory_name(DF),
    atom_chars(PkgName, Name),
    append(["find ",DF,"/", Name, " -type f -print0 | sort -z | xargs -0 sha1sum | sha1sum | awk '{print $1}'"], IntegrityHashCmd),
    run_command(IntegrityHashCmd, temp_result(IntegrityHash)),
    LockDependencyTerm=dependency(Name, path(Path), IntegrityHash).

/**
 A hack to get the result of a shell command.
 Maybe current_output could fix this
*/
run_command(Command, Output) :-
    append(["echo \"temp_result(\\\"$(",Command, ")\\\").\" > temp"], Command2),
    shell(Command2),
    open('temp', read, Stream),
    read(Stream, Output),
    close(Stream),
    shell("rm temp").

% === Generated code start ===
script_string("ensure_dependency", "#!/bin/sh\nset -eu\n\necho \"Ensuring is installed: ${DEPENDENCY_TERM}\"\n\nrm --recursive --force scryer_libs/tmp-package\n\nrelocate_tmp() {\n    rm --recursive --force \"scryer_libs/packages/${DEPENDENCY_NAME}\"\n    mv scryer_libs/tmp-package \"scryer_libs/packages/${DEPENDENCY_NAME}\"\n}\n\ncase \"${DEPENDENCY_KIND}\" in\n    git_default)\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            \"${GIT_URL}\" \\\n            scryer_libs/tmp-package\n        relocate_tmp\n        ;;\n    git_branch)\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            --branch \"${GIT_BRANCH}\" \\\n            \"${GIT_URL}\" \\\n            scryer_libs/tmp-package\n        relocate_tmp\n        ;;\n    git_tag)\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            --branch \"${GIT_TAG}\" \\\n            \"${GIT_URL}\" \\\n            scryer_libs/tmp-package\n        relocate_tmp\n        ;;\n    git_hash)\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            \"${GIT_URL}\" \\\n            scryer_libs/tmp-package\n        git -C scryer_libs/tmp-package fetch \\\n            --quiet \\\n            --depth 1 \\\n            origin \"${GIT_HASH}\"\n        git -C scryer_libs/tmp-package switch \\\n            --quiet \\\n            --detach \\\n            \"${GIT_HASH}\"\n        relocate_tmp\n        ;;\n    path)\n        ln -rsf \"${DEPENDENCY_PATH}\" \"scryer_libs/packages/${DEPENDENCY_NAME}\"\n        ;;\n    *)\n        echo \"Unknown dependency kind\"\n        exit 1\n        ;;\nesac\n").
% === Generated code end ===

