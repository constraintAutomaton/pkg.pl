# Shows all the tasks
default:
    @just --list

# Builds the bakage.pl file
build: codegen
    mv bakage.pl.gen bakage.pl
    chmod +x bakage.pl

[private]
codegen:
    #!/bin/sh
    set -eu

    sed -e "/% === Generated code start ===/q" bakage.pl > bakage.pl.gen

    for file in scripts/*.sh; do
        script_string=$(scryer-prolog -f -g "
            use_module(library(pio)),
            use_module(library(dcgs)),
            phrase_from_file(seq(Script),\"${file}\"),
            write_term(Script, [quoted(true),double_quotes(true)]),
            halt.
        ")
        script_name=$(basename -s .sh "${file}")
        printf '%s\n' "script_string(\"${script_name}\", ${script_string})." >> bakage.pl.gen
    done
    sed -n -e "/% === Generated code end ===/,$ {p}" bakage.pl >> bakage.pl.gen

# Checks if the bakage.pl file is up to date
codegen-check: codegen
    diff bakage.pl bakage.pl.gen
    rm -f bakage.pl.gen

# Run all lints
lint: lint-sh

[private]
lint-sh:
    shellcheck -s sh -S warning ./**/*.sh
    
# Runs all the checks made in CI
ci:
    just codegen-check
    just lint-sh
    just test

# Runs all the tests
test: build
    just example/test
    just tests/test

# Cleans everything that is generated during builds or tests
clean:
    just example/clean
    rm -f bakage.pl.gen
