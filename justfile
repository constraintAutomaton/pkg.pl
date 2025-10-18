BUILD_NAME := "bakage.pl"

# Auto build bakage on changes
watch-dev:
    watchexec -w scripts -w src just build

# Shows all the tasks
default:
    @just --list

[private]
ensure-build-directory:
    mkdir -p build

# Builds the bakage.pl file
build: codegen-scripts
    cat ./src/shebang.sh > "./build/{{BUILD_NAME}}"
    cat ./src/bakage.pl >> "./build/{{BUILD_NAME}}"
    printf "\n" >> "./build/{{BUILD_NAME}}"
    cat ./src/validation.pl >> "./build/{{BUILD_NAME}}"
    printf "\n" >> "./build/{{BUILD_NAME}}"
    cat ./src/cli.pl >> "./build/{{BUILD_NAME}}"
    printf "\n" >> "./build/{{BUILD_NAME}}"
    cat ./build/scripts.pl >> "./build/{{BUILD_NAME}}"
    chmod +x "./build/{{BUILD_NAME}}"

[private]
codegen-scripts: ensure-build-directory
    #!/bin/sh
    set -eu

    touch ./build/scripts.pl

    for file in scripts/*.sh; do
        script_string=$(scryer-prolog -f -g "
            use_module(library(pio)),
            use_module(library(dcgs)),
            phrase_from_file(seq(Script),\"${file}\"),
            write_term(Script, [quoted(true),double_quotes(true)]),
            halt.
        ")
        script_name=$(basename -s .sh "${file}")
        printf '%s\n' "script_string(\"${script_name}\", ${script_string})." >> ./build/scripts.pl
    done

# Run all lints
lint: lint-sh

[private]
lint-sh:
    shellcheck -s sh -S warning ./**/*.sh

# Runs all the checks made in CI
ci:
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
    rm -rf build
