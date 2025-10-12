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
