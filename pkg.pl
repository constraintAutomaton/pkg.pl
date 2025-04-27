/* SPDX-License-Identifier: Unlicense */

:- module(pkg, [pkg_install/0]).

:- use_module(library(os)).
:- use_module(library(files)).
:- use_module(library(lists)).
:- use_module(library(format)).

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
    setenv("SHELL", "/bin/sh"),
    (   member(dependencies(Deps), Manifest) ->
        ensure_dependencies(Deps)
    ;   true
    ).

ensure_dependencies([]).
ensure_dependencies([D|Ds]) :-
    ensure_dependency(D),
    ensure_dependencies(Ds).

ensure_dependency(git(Url)) :-
    shell("rm -rf scryer_libs/tmp"),
    % Hell yeah, injection attack!
    append(["git clone --depth 1 ", Url, " scryer_libs/tmp"], Command),
    shell(Command),
    parse_manifest("scryer_libs/tmp/scryer-manifest.pl", Manifest),
    member(name(Name), Manifest),
    append(["rm -rf scryer_libs/", Name, "; mv scryer_libs/tmp scryer_libs/", Name], Command2),
    shell(Command2).
