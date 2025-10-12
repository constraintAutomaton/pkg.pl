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
:- use_module('validation.pl').

run :-
    (
        catch(
            main,
            Error,
            (
                portray_clause(Error),
                halt(1)
            )
        ),
        halt
    ;   phrase_to_stream("main predicate failed", user_output),
        halt(1)
    ).

main :-
    collect_relevant_env_vars(EnvVars),
    argv(Args),
    parse_args(Args, ParsedArgs),
    resolve_flags_and_task(ParsedArgs, EnvVars, GlobalFlags, Task),
    set_global_state(GlobalFlags),
    do_task(Task, GlobalFlags).

parse_args(Args, ParsedArgs) :-
    phrase(chunked_args(ChunkedArgs), Args),
    parse_chunked_args(ChunkedArgs, parsed_args([], []), ParsedArgs).

parse_chunked_args([], ParsedArgs, ParsedArgs).
parse_chunked_args([CA|CAs], ParsedArgs0, ParsedArgs) :-
    parse_chunked_arg(CA, ParsedArgs0, ParsedArgs1),
    parse_chunked_args(CAs, ParsedArgs1, ParsedArgs).

parse_chunked_arg(flag(Flag), parsed_args(Command, Flags0), parsed_args(Command, Flags)) :-
    options_upsert(Flag, Flags0, Flags).

parse_chunked_arg(command(Cmd), parsed_args(Command0, Flags), parsed_args(Command, Flags)) :-
    append(Command0, [Cmd], Command).

options_upsert(Option, Options0, Options) :-
    functor(Option, Functor, Arity),
    functor(Pattern, Functor, Arity),
    (   append([Before, [Pattern], After], Options0) ->
        append([Before, [Option], After], Options)
    ;   append(Options0, [Option], Options)
    ).

chunked_args([]) --> [].
chunked_args([CA|CAs]) --> chunked_arg(CA), chunked_args(CAs).

chunked_arg(flag(help)) --> ( ["-h"] ; ["--help"] ).
chunked_arg(flag(color(ColorPolicy))) -->
    ["--color"],
    [ColorPolicy0],
    { phrase(color_policy(ColorPolicy), ColorPolicy0) }.
chunked_arg(flag(color(ColorPolicy))) -->
    [ColorPolicy0],
    { phrase(("--color=", color_policy(ColorPolicy)), ColorPolicy0) }.
chunked_arg(command(install)) --> ["install"].

color_policy(auto) --> "auto".
color_policy(always) --> "always".
color_policy(never) --> "never".

collect_relevant_env_vars(EnvVars) :-
    (   getenv("NO_COLOR", NC) ->
        NoColor = NC
    ;   NoColor = unset
    ),
    (   getenv("CLICOLOR_FORCE", CCF) ->
        CliColorForce = CCF
    ;   CliColorForce = unset
    ),
    EnvVars = [
        no_color(NoColor),
        cli_color_force(CliColorForce)
    ].

resolve_flags_and_task(ParsedArgs, EnvVars, GlobalFlags, Task) :-
    ParsedArgs = parsed_args(Command, Flags),
    resolve_cli_color(EnvVars, Flags, CliColor),
    GlobalFlags = [
        cli_color(CliColor)
    ],
    resolve_task(Command, Flags, Task).

resolve_cli_color(EnvVars, Flags, CliColor) :-
    member(no_color(NoColor), EnvVars),
    member(cli_color_force(CliColorForce), EnvVars),
    (   member(color(ColorPolicy), Flags) ->
        true
    ;   ColorPolicy = auto
    ),
    CliColor0 = on,
    (   NoColor \= unset, NoColor \= "" ->
        CliColor1 = off
    ;   CliColor1 = CliColor0
    ),
    (   CliColorForce \= unset, CliColorForce \= "" ->
        CliColor2 = on
    ;   CliColor2 = CliColor1
    ),
    (   ColorPolicy == always ->
        CliColor = on
    ;   ColorPolicy == never ->
        CliColor = off
    ;   CliColor = CliColor2
    ).

resolve_task([], _, help(root)).
resolve_task([Command|Subcommands], Flags, Task) :-
    resolve_command_task(Command, Subcommands, Flags, Task).

resolve_command_task(install, [], Flags, Task) :-
    (   member(help, Flags) ->
        Task = help(install)
    ;   Task = install
    ).

set_global_state(GlobalFlags) :-
    (   member(cli_color(CliColor), GlobalFlags) ->
        assertz(cli_color(CliColor))
    ;   true
    ).

do_task(help(CommandPath), _) :-
    phrase_to_stream(help_text(CommandPath), user_output).

do_task(install, _) :-
    pkg_install(_).

% Uses global color configuration
ansi(Color) -->
    {
        cli_color(CliColor),
        phrase(ansi(CliColor, Color), Code)
    },
    Code.

ansi(off, _) --> "".
ansi(on, Color) --> ansi_(Color).

% Raw colors
ansi_(reset) --> "\x1b\[0m".
ansi_(bold) --> "\x1b\[1m".
ansi_(red) --> "\x1b\[31m".
ansi_(green) --> "\x1b\[32m".
ansi_(yellow) --> "\x1b\[33m".
ansi_(blue) --> "\x1b\[34m".
ansi_(magenta) --> "\x1b\[35m".
ansi_(cyan) --> "\x1b\[36m".
ansi_(white) --> "\x1b\[37m".

% Color aliases
ansi_(help_header) --> ansi_(yellow), ansi_(bold).
ansi_(help_option) --> ansi_(green), ansi_(bold).

% Uses global color configuration
color_text(Color, Text) -->
    {
        cli_color(CliColor),
        phrase(color_text(CliColor, Color, Text), ColorText)
    },
    ColorText.

% Accepts arbitrary grammar rule bodies
color_text(CliColor, Color, Text) -->
    { phrase(Text, Text1) },
    ansi(CliColor, Color),
    Text1,
    ansi(CliColor, reset).

help_text(CommandPath) -->
    {
        help_info(CommandPath, Description, Usage, Options, Commands)
    },
    help_description(Description),
    help_usage(Usage),
    help_option_table("Options", Options),
    help_option_table("Commands", Commands).

help_info(
    root,
    "Bakage: an experimental package manager for Prolog",
    "bakage [OPTIONS] [COMMAND]",
    [
        flag("color", none, "When to use color. Valid options: auto, always, never"),
        flag("help", "h", "Print help")
    ],
    [
        %command("init", "Create a new project in the current directory"),
        command("install", "Installs the dependencies of the current package")
        %command("new", "Create a new project in a new directory"),
        %command("run", "Runs the current package")
    ]
).
help_info(
    install,
    "Install package dependencies",
    "bakage install [OPTIONS]",
    [
        flag("color", none, "When to use color. Valid options: auto, always, never"),
        flag("help", "h", "Print help")
    ],
    []
).

options_width(Options, OptionWidth) :-
    options_width(Options, 0, OptionWidth0),
    OptionWidth is 2 + OptionWidth0 + 2.

options_width([], OptionWidth, OptionWidth).
options_width([Option|Options], OptionWidth0, OptionWidth) :-
    option_width(Option, Width),
    OptionWidth1 is max(OptionWidth0, Width),
    options_width(Options, OptionWidth1, OptionWidth).

option_width(flag(Long, _, _), Width) :-
    length(Long, Width0),
    Width is 4 + Width0.
option_width(command(Name, _), Width) :-
    length(Name, Width).

with_tail_newline([]) --> "\n".
with_tail_newline([H|T]) -->
    {
        Text = [H|T],
        phrase((..., [Last]), Text),
        if_(
            Last = '\n',
            WithNewline = Text,
            phrase((Text, "\n"), WithNewline)
        )
    },
    WithNewline.

help_description(Description) -->
    { phrase(Description, Description1) },
    with_tail_newline(Description1).

help_usage(Usage) -->
    "\n",
    color_text(help_header, "Usage:"),
    " ",
    color_text(help_option, Usage),
    "\n".

help_option_table(_, []) --> "".
help_option_table(Name, Options) -->
    { Options = [_|_] },
    "\n",
    color_text(help_header, (Name, ":")), "\n",
    { options_width(Options, OptionWidth) },
    help_option_table_(Options, OptionWidth).

help_option_table_([], _) --> "".
help_option_table_([Option|Options], OptionWidth) -->
    help_option_table_line(Option, OptionWidth),
    help_option_table_(Options, OptionWidth).

n_spaces(N) -->
    {
        length(Spaces, N),
        append(Spaces, _, [' '|Spaces])
    },
    Spaces.

help_option_table_line(command(Name, Description), OptionWidth) -->
    {
        length(Name, NameLen),
        PaddingLen is OptionWidth - NameLen - 2
    },
    n_spaces(2), color_text(help_option, Name), n_spaces(PaddingLen),
    with_tail_newline(Description).

help_option_table_line(flag(Long, Short, Description), OptionWidth) -->
    {
        length(Long, LongLen),
        PaddingLen is OptionWidth - LongLen - 6,
        if_(
            Short = none,
            phrase(n_spaces(4), ShortDesc),
            phrase(
                (color_text(help_option, ("-", Short)), ", "),
                ShortDesc
            )
        )
    },
    n_spaces(2),
    ShortDesc,
    color_text(help_option, ("--", Long)),
    n_spaces(PaddingLen),
    with_tail_newline(Description).

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

% === Generated code start ===
script_string("ensure_dependencies", "#!/bin/sh\nset -u\n\n# Fail instead of prompting for password in git commands.\nexport GIT_TERMINAL_PROMPT=0\n\nwrite_result() {\n    flock scryer_libs/temp/install_resp.pl.lock -c \\\n        \"printf \'result(\\\"%s\\\", %s).\\n\' \\\"$1\\\" \\\"$2\\\" >> scryer_libs/temp/install_resp.pl\"\n}\n\nwrite_success() {\n    write_result \"$1\" \"success\"\n}\n\nwrite_error() {\n    escaped_error=$(printf \'%s\' \"$2\" | sed -e \'s/\\\\/\\\\\\\\/g\' -e \'s/\"/\\\\\"/g\')\n    escaped_error=$(printf \'%s\' \"$escaped_error\" | tr \'\\r\\n\' \'\\\\n\')\n    escaped_error=$(printf \'%s\' \"$escaped_error\" | sed \'s/\xa0\/ /g\')\n    write_result \"$1\" \"error(\\\\\\\"$escaped_error\\\\\\\")\"\n}\n\ninstall_git_default() {\n    dependency_name=$1\n    git_url=$2\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        write_success \"${dependency_name}\"\n    else\n        write_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_git_branch() {\n    dependency_name=$1\n    git_url=$2\n    git_branch=$3\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            --branch \"${git_branch}\" \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        write_success \"${dependency_name}\"\n    else\n        write_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_git_tag() {\n    dependency_name=$1\n    git_url=$2\n    git_tag=$3\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            --branch \"${git_tag}\" \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        write_success \"${dependency_name}\"\n    else\n        write_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_git_hash() {\n    dependency_name=$1\n    git_url=$2\n    git_hash=$3\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        fetch_error=$(\n            git -C \"scryer_libs/packages/${dependency_name}\" fetch \\\n                --quiet \\\n                --depth 1 \\\n                origin \"${git_hash}\" 2>&1 1>/dev/null\n        )\n        switch_error=$(\n            git -C \"scryer_libs/packages/${dependency_name}\" switch \\\n                --quiet \\\n                --detach \\\n                \"${git_hash}\" 2>&1 1>/dev/null\n        )\n        combined_error=\"${fetch_error}; ${switch_error}\"\n\n        if [ -z \"$fetch_error\" ] && [ -z \"$switch_error\" ]; then\n            write_success \"${dependency_name}\"\n        else\n            write_error \"${dependency_name}\" \"$combined_error\"\n        fi\n    else\n        write_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_path() {\n    dependency_name=$1\n    dependency_path=$2\n\n    if [ -d \"${dependency_path}\" ]; then\n        error_output=$(ln -rsf \"${dependency_path}\" \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null)\n\n        if [ -z \"$error_output\" ]; then\n            write_success \"${dependency_name}\"\n        else\n            write_error \"${dependency_name}\" \"$error_output\"\n        fi\n    else\n        write_error \"${dependency_name}\" \"${dependency_path} does not exist\"\n    fi\n}\n\nOLD_IFS=$IFS\nIFS=\'|\'\nset -- $DEPENDENCIES_STRING\nIFS=$OLD_IFS\n\ntouch scryer_libs/temp/install_resp.pl\n\nfor dependency in \"$@\"; do\n    unset dependency_term dependency_kind dependency_name git_url git_branch git_tag git_hash dependency_path\n\n    IFS=\';\'\n    set -- $dependency\n    IFS=$OLD_IFS\n\n    while [ \"$#\" -gt 0 ]; do\n        field=$1\n        shift\n\n        key=$(printf \"%s\" \"$field\" | cut -d= -f1)\n        value=$(printf \"%s\" \"$field\" | cut -d= -f2-)\n\n        case \"$key\" in\n        dependency_term) dependency_term=$value ;;\n        dependency_kind) dependency_kind=$value ;;\n        dependency_name) dependency_name=$value ;;\n        git_url) git_url=$value ;;\n        git_branch) git_branch=$value ;;\n        git_tag) git_tag=$value ;;\n        git_hash) git_hash=$value ;;\n        dependency_path) dependency_path=$value ;;\n        esac\n    done\n\n    printf \"Ensuring is installed: %s\\n\" \"${dependency_term}\"\n\n    case \"${dependency_kind}\" in\n    do_nothing) ;;\n\n    git_default)\n        install_git_default \"${dependency_name}\" \"${git_url}\" &\n        ;;\n    git_branch)\n        install_git_branch \"${dependency_name}\" \"${git_url}\" \"${git_branch}\" &\n        ;;\n    git_tag)\n        install_git_tag \"${dependency_name}\" \"${git_url}\" \"${git_tag}\" &\n        ;;\n    git_hash)\n        install_git_hash \"${dependency_name}\" \"${git_url}\" \"${git_hash}\" &\n        ;;\n    path)\n        install_path \"${dependency_name}\" \"${dependency_path}\" &\n        ;;\n    *)\n        printf \"Unknown dependency kind: %s\\n\" \"${dependency_kind}\"\n        write_error \"${dependency_name}\" \"Unknown dependency kind: ${dependency_kind}\"\n        ;;\n    esac\ndone\n\nwait\n\nrm -f scryer_libs/temp/install_resp.pl.lock\n").
% === Generated code end ===
