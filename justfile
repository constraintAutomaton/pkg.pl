BUILD_NAME := "./build/bakage.pl"

# Auto build bakage on changes
watch-dev:
    watchexec -w scripts -w src -i "src/script.pl" just build

# Shows all the tasks
default:
    @just --list

# Builds the bakage.pl file
build: codegen
    mkdir -p build
    mv ./src/scripts.pl.gen ./src/scripts.pl
    cat ./src/bakage.pl > "{{BUILD_NAME}}"
    printf "\n" >> "{{BUILD_NAME}}"
    cat ./src/cli.pl >> "{{BUILD_NAME}}"
    printf "\n" >> "{{BUILD_NAME}}"
    cat ./src/scripts.pl >> "{{BUILD_NAME}}"
    sed -i '/% === Dev import start ===/,/% === Dev import end ===/d' "{{BUILD_NAME}}"
    chmod +x "{{BUILD_NAME}}"

[private]
codegen:
    #!/bin/sh
    set -eu

    for file in scripts/*.sh; do
        script_string=$(scryer-prolog -f -g "
            use_module(library(pio)),
            use_module(library(dcgs)),
            phrase_from_file(seq(Script),\"${file}\"),
            write_term(Script, [quoted(true),double_quotes(true)]),
            halt.
        ")
        script_name=$(basename -s .sh "${file}")
        printf '%s\n' "script_string(\"${script_name}\", ${script_string})." >> ./src/scripts.pl.gen
    done

# Checks if the bakage.pl file is up to date
codegen-check: build
    just codegen
    diff ./src/scripts.pl ./src/scripts.pl.gen
    rm -f ./src/scripts.pl.gen

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
    just tests/clean
    rm -f bakage.pl.gen
