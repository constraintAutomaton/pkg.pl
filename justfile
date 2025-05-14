default:
    @just --list

test: test-example

test-example:
    just example/test
