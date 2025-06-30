/* SPDX-License-Identifier: Unlicense */

:- module(pkg, [pkg_install/1]).

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

locked_dependencies(Ls):- 
    file_exists("manifest-lock.pl") -> 
        parse_manifest("manifest-lock.pl", LockFile),
        member(lock_dependencies(Ls), LockFile)
        ;   Ls = [].
                
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
                        locked_dependencies(Locked_Dependencies),
                        logical_plan(Plan, Deps, Installed_Packages, Locked_Dependencies),
                        installation_execution(Plan, Installation_Report),
                        lock_dependencies_result(Plan),
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

lock_dependencies_result(Plan) :-
    parse_lock_report(Result),
    phrase(lock_report(Plan, Result_Report), Results).
    
% A logical plan to install the dependencies
logical_plan(Plan, Ds, Installed_Packages, Locked_Dependencies) :-
    phrase(fetch_plan(Ds, Installed_Packages, Locked_Dependencies), Plan).

% A logical plan to fetch the dependencies
fetch_plan([], _, _) --> [].
fetch_plan([D|Ds], Installed_Packages, Locked_Dependencies) --> 
    {fetch_step(D, Installation_Step, Installed_Packages, Locked_Dependencies)},
    [Installation_Step],
    fetch_plan(Ds, Installed_Packages, Locked_Dependencies).

% A step of a logical plan to fetch the dependencies
fetch_step(dependency(Name, DependencyTerm), Step, Installed_Packages, Locked_Dependencies) :-
    if_(memberd_t(Name, Installed_Packages),
        Step = do_nothing(dependency(Name, DependencyTerm), do_nothing),
        (
            if_(memberd_t(dependency(Name, X), Locked_Dependencies),
                Step = install_locked_dependency(dependency(Name, X), do_nothing),
                Step = install_dependency(dependency(Name, DependencyTerm), lock)
            )
        )
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


parse_report(Result_List, File) :-
    setup_call_cleanup(
        open(File, read, Stream),
        once(phrase(prolog_kb_list(Stream), Result_List)),
        (
            close(Stream),
            ( file_exists(File)->
                delete_file(File)
            ; true
            )
        )
    ).

% Parse the report of the installation of the dependencies
parse_install_report(Result_List) :- parse_report(Result_List, "scryer_libs/temp/install_resp.pl").

parse_lock_report(Result_List) :- parse_report(Result_List, "scryer_libs/temp/lock_resp.pl").

% The installation report of the dependencies
installation_report([], _) --> [].
installation_report([P|Ps], Result_Report) -->
    { report_installation_step(P, Result_Report, R) },
    [R],
    installation_report(Ps, Result_Report).

% The result of a logical step
report_installation_step(do_nothing(dependency(Name, DependencyTerm), _), _, do_nothing(dependency(Name, DependencyTerm))-success).


report_installation_step(install_locked_dependency(D, _), ResultMessages, install_locked_dependency(D)-Message):-
    report_installation_step(install_dependency(D, _), ResultMessages, _-Message).

report_installation_step(install_dependency(dependency(Name, DependencyTerm), _), ResultMessages, install_dependency(dependency(Name, DependencyTerm))-Message):-
    memberchk(result(Name, Message), ResultMessages).

lock_report([], _) --> [].
lock_report([install_dependency(D, lock)|Ps], Result_Report) -->
    { report_lock_step(install_dependency(D, lock), Result_Report, R) },
    [R],
    lock_report(Ps, Result_Report).

lock_report([install_locked_dependency(_, do_nothing)|Ps], Result_Report) -->
    [],
    lock_report(Ps, Result_Report).

lock_report([do_nothing(_, do_nothing)|Ps], Result_Report) -->
    [],
    lock_report(Ps, Result_Report).

report_lock_step(install_dependency(dependency(Name, DependencyTerm), lock), ResultMessages, lock(dependency(Name, DependencyTerm))-Message):-
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
physical_plan_step(do_nothing(dependency(Name, D), do_nothing) , El) :-
    write_term_to_chars(D, [quoted(true), double_quotes(true)], DependencyTermChars),
    append(["dependency_term=", DependencyTermChars, ";dependency_name=", Name, ";dependency_kind=do_nothing"], El).

physical_plan_step(install_locked_dependency(D, Lock), El) :-
    physical_plan_step(install_dependency(D, Lock), El).

physical_plan_step(install_dependency(D, do_nothing), El):-
    physical_plan_step(install_dependency(D), El_0),
    append([El_0, ";lock=false"], El).

physical_plan_step(install_dependency(D, lock), El):-
    physical_plan_step(install_dependency(D), El_0),
    append([El_0, ";lock=true"], El).

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
script_string("ensure_dependencies", "#!/bin/sh\nset -u\n\nwrite_install_result() {\n    flock scryer_libs/temp/install_resp.pl.lock -c \\\n        \"printf \'result(\\\"%s\\\", %s).\\n\' \\\"$1\\\" \\\"$2\\\" >> scryer_libs/temp/install_resp.pl\"\n}\n\nwrite_lock_result() {\n    flock scryer_libs/temp/lock_resp.pl.lock -c \\\n        \"printf \'result(\\\"%s\\\", %s).\\n\' \\\"$1\\\" \\\"$2\\\" >> scryer_libs/temp/lock_resp.pl\"\n}\n\nwrite_install_success() {\n    write_install_result \"$1\" \"success\"\n}\n\nwrite_lock_success() {\n    write_lock_result \"$1\" \"$2\"\n}\n\nwrite_install_error() {\n    escaped_error=$(printf \'%s\' \"$2\" | sed -e \'s/\\\\/\\\\\\\\/g\' -e \'s/\"/\\\\\"/g\')\n    escaped_error=$(printf \'%s\' \"$escaped_error\" | tr \'\\r\\n\' \'\\\\n\')\n    escaped_error=$(printf \'%s\' \"$escaped_error\" | sed \'s/\xa0\/ /g\')\n    write_install_result \"$1\" \"error(\\\\\\\"$escaped_error\\\\\\\")\"\n}\n\nwrite_lock_error() {\n    escaped_error=$(printf \'%s\' \"$2\" | sed -e \'s/\\\\/\\\\\\\\/g\' -e \'s/\"/\\\\\"/g\')\n    escaped_error=$(printf \'%s\' \"$escaped_error\" | tr \'\\r\\n\' \'\\\\n\')\n    escaped_error=$(printf \'%s\' \"$escaped_error\" | sed \'s/\xa0\/ /g\')\n    write_lock_result \"$1\" \"error(\\\\\\\"$escaped_error\\\\\\\")\"\n}\n\nlock_dependency() {\n    dependency_name=$1\n\n    ERROR=\"\"\n\n    GIT_HASH=$(cd \"scryer_libs/packages/${dependency_name}\" && git rev-parse HEAD 2>&1) || ERROR=\"git failure: $GIT_HASH\"\n    GIT_HASH=$(echo \"$GIT_HASH\" | head -n1)\n\n    if [ -z \"$ERROR\" ]; then\n        write_lock_success \"${dependency_name}\" \"lock(\\\\\\\"${GIT_HASH}\\\\\\\")\"\n    else\n        write_lock_error \"${dependency_name}\" \"${ERROR}\"\n    fi\n}\n\ninstall_git_default() {\n    dependency_name=$1\n    git_url=$2\n    lock=$3\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        write_install_success \"${dependency_name}\"\n        if [ \"$lock\" = \"true\" ]; then\n            lock_dependency \"${dependency_name}\"\n        fi\n    else\n        write_install_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_git_branch() {\n    dependency_name=$1\n    git_url=$2\n    git_branch=$3\n    lock=$4\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            --branch \"${git_branch}\" \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        write_install_success \"${dependency_name}\"\n        if [ \"$lock\" = \"true\" ]; then\n            lock_dependency \"${dependency_name}\"\n        fi\n    else\n        write_install_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_git_tag() {\n    dependency_name=$1\n    git_url=$2\n    git_tag=$3\n    lock=$4\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            --branch \"${git_tag}\" \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        write_install_success \"${dependency_name}\"\n        if [ \"$lock\" = \"true\" ]; then\n            lock_dependency \"${dependency_name}\"\n        fi\n    else\n        write_install_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_git_hash() {\n    dependency_name=$1\n    git_url=$2\n    git_hash=$3\n    lock=$4\n\n    error_output=$(\n        git clone \\\n            --quiet \\\n            --depth 1 \\\n            --single-branch \\\n            \"${git_url}\" \\\n            \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null\n    )\n\n    if [ -z \"$error_output\" ]; then\n        fetch_error=$(\n            git -C \"scryer_libs/packages/${dependency_name}\" fetch \\\n                --quiet \\\n                --depth 1 \\\n                origin \"${git_hash}\" 2>&1 1>/dev/null\n        )\n        switch_error=$(\n            git -C \"scryer_libs/packages/${dependency_name}\" switch \\\n                --quiet \\\n                --detach \\\n                \"${git_hash}\" 2>&1 1>/dev/null\n        )\n        combined_error=\"${fetch_error}; ${switch_error}\"\n\n        if [ -z \"$fetch_error\" ] && [ -z \"$switch_error\" ]; then\n            write_install_success \"${dependency_name}\"\n            if [ \"$lock\" = \"true\" ]; then\n                lock_dependency \"${dependency_name}\"\n            fi\n        else\n            write_install_error \"${dependency_name}\" \"${combined_error}\"\n        fi\n    else\n        write_install_error \"${dependency_name}\" \"$error_output\"\n    fi\n}\n\ninstall_path() {\n    dependency_name=$1\n    dependency_path=$2\n\n    if [ -d \"${dependency_path}\" ]; then\n        error_output=$(ln -rsf \"${dependency_path}\" \"scryer_libs/packages/${dependency_name}\" 2>&1 1>/dev/null)\n\n        if [ -z \"$error_output\" ]; then\n            write_install_success \"${dependency_name}\"\n        else\n            write_install_error \"${dependency_name}\" \"$error_output\"\n        fi\n    else\n        write_install_error \"${dependency_name}\" \"${dependency_path} does not exist\"\n    fi\n}\n\nOLD_IFS=$IFS\nIFS=\'|\'\nset -- $DEPENDENCIES_STRING\nIFS=$OLD_IFS\n\ntouch scryer_libs/temp/install_resp.pl\ntouch scryer_libs/temp/lock_resp.pl\n\nfor dependency in \"$@\"; do\n    unset dependency_term dependency_kind dependency_name git_url git_branch git_tag git_hash dependency_path lock\n\n    IFS=\';\'\n    set -- $dependency\n    IFS=$OLD_IFS\n\n    while [ \"$#\" -gt 0 ]; do\n        field=$1\n        shift\n\n        key=$(printf \"%s\" \"$field\" | cut -d= -f1)\n        value=$(printf \"%s\" \"$field\" | cut -d= -f2-)\n\n        case \"$key\" in\n        dependency_term) dependency_term=$value ;;\n        dependency_kind) dependency_kind=$value ;;\n        dependency_name) dependency_name=$value ;;\n        git_url) git_url=$value ;;\n        git_branch) git_branch=$value ;;\n        git_tag) git_tag=$value ;;\n        git_hash) git_hash=$value ;;\n        dependency_path) dependency_path=$value ;;\n        lock) lock=$value ;;\n        esac\n    done\n\n    printf \"Ensuring is installed: %s\\n\" \"${dependency_term}\"\n\n    case \"${dependency_kind}\" in\n    do_nothing) ;;\n\n    git_default)\n        install_git_default \"${dependency_name}\" \"${git_url}\" \"${lock}\" &\n        ;;\n    git_branch)\n        install_git_branch \"${dependency_name}\" \"${git_url}\" \"${git_branch}\" \"${lock}\" &\n        ;;\n    git_tag)\n        install_git_tag \"${dependency_name}\" \"${git_url}\" \"${git_tag}\" \"${lock}\" &\n        ;;\n    git_hash)\n        install_git_hash \"${dependency_name}\" \"${git_url}\" \"${git_hash}\" \"${lock}\" &\n        ;;\n    path)\n        install_path \"${dependency_name}\" \"${dependency_path}\" &\n        ;;\n    *)\n        printf \"Unknown dependency kind: %s\\n\" \"${dependency_kind}\"\n        write_install_error \"${dependency_name}\" \"Unknown dependency kind: ${dependency_kind}\"\n        ;;\n    esac\ndone\n\nwait\n\nrm -f scryer_libs/temp/install_resp.pl.lock\nrm -f scryer_libs/temp/lock_resp.pl.lock").
% === Generated code end ===
