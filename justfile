BINARY_NAME := "./bin/bakage.pl"
# Shows all the tasks
default:
    @just --list

# Builds bakage.pl when files from ./src are modified
watch-dev:
    watchexec -w ./src just build

# Builds the bakage.pl file
build binary_name=BINARY_NAME: codegen
    mv ./src/script.pl.gen ./src/script.pl
    cat ./src/bakage.pl > "{{binary_name}}"
    printf "\n" >> "{{binary_name}}"
    cat ./src/cli.pl >> "{{binary_name}}"
    printf "\n" >> "{{binary_name}}"
    cat ./src/script.pl >> "{{binary_name}}"
    sed -i '/% === Dev import start ===/,/% === Dev import end ===/d' "{{binary_name}}"
    chmod +x "{{binary_name}}"

[private]
codegen:
    #!/bin/sh
    set -eu

    sed -e "/% === Generated code start ===/q" ./src/script.pl > ./src/script.pl.gen

    for file in scripts/*.sh; do
        script_string=$(scryer-prolog -f -g "
            use_module(library(pio)),
            use_module(library(dcgs)),
            phrase_from_file(seq(Script),\"${file}\"),
            write_term(Script, [quoted(true),double_quotes(true)]),
            halt.
        ")
        script_name=$(basename -s .sh "${file}")
        printf '%s\n' "script_string(\"${script_name}\", ${script_string})." >> ./src/script.pl.gen
    done
    sed -n -e "/% === Generated code end ===/,$ {p}" ./src/script.pl >> ./src/script.pl.gen

# Checks if the ./src/script.pl file is up to date
codegen-check: codegen
    diff ./src/script.pl ./src/script.pl.gen
    rm -f ./src/script.pl.gen

# Check if the ./bin/bakage.pl file is up to date
build-check:
    #!/bin/sh
    temp_build = "./bin/bakage.pl.gen"
    just build temp_build
    diff temp_build "{{BINARY_NAME}}"
    rm -f temp_build

# Run all lints
lint: lint-sh

[private]
lint-sh:
    shellcheck -s sh -S warning ./**/*.sh

# Runs all the checks made in CI
ci:
    just codegen-check
    just build-check
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
