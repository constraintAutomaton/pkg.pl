#!/bin/sh

bakage() {
    ../../build/bakage.pl "$@"
}

echo "=== Testing help with various different color configurations ==="
bakage --help --color always
echo "================================================================"
bakage --help --color never
echo "================================================================"
CLICOLOR_FORCE=1 bakage --help
echo "================================================================"
NO_COLOR=1 bakage --help

echo "=== Install help ==="
bakage install --help --color always
echo "================================================================"
bakage install --help --color never
echo "================================================================"
