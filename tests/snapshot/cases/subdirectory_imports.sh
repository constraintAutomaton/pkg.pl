#!/bin/sh

bakage() {
    ../../../../build/bakage.pl "$@"
}

echo "=== Installing dependencies ==="
bakage install

echo ""
echo "=== Running test from project root ==="
scryer-prolog tests/test_my_module.pl -g "run_tests, halt."

echo ""
echo "=== Running test from subdirectory ==="
cd tests
scryer-prolog test_my_module.pl -g "run_tests, halt."
