#!/bin/sh

set -o errexit
set -o nounset

cd example

clean() {
    rm --recursive --force scryer_libs/
}

install() {
    scryer-prolog -f pkg.pl -g pkg_install,halt
}

test_package() {
    scryer-prolog -f main.pl -g main,halt
}

clean
install
test_package
