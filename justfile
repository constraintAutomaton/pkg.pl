default:
    @just --list

build: codegen
    mv bakage.pl.gen bakage.pl

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

codegen-check: codegen
    diff bakage.pl bakage.pl.gen

lint-sh:
    shellcheck -s sh -S warning ./**/*.sh
    
# All the checks made in CI
ci: codegen-check lint-sh test

test: 
    just build 
    just test-example
    just tests/build
    just tests/test

test-example:
    just example/test

clean-codegen:
    rm -f bakage.pl.gen

clean: clean-codegen
