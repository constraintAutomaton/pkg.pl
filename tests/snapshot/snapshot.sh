#!/bin/sh

set -eu

snapshot_test() {
    test_file=$(realpath "$1")
    test_name=$(basename "$test_file" ".sh")

    printf 'Snapshoting test "%s"\n' "$test_name"

    if [ -f "cases/${test_name}.stdin" ]; then
        stdin_file=$(realpath "cases/${test_name}.stdin")
    else
        stdin_file="/dev/null"
    fi

    stdout_file=$(realpath "tmp_snapshot/${test_name}.stdout")
    stderr_file=$(realpath "tmp_snapshot/${test_name}.stderr")
    status_file=$(realpath "tmp_snapshot/${test_name}.status")

    original_pwd=$(pwd)
    has_in_dir=false
    if [ -d "cases/${test_name}.in" ]; then
        has_in_dir=true
        cp -r "cases/${test_name}.in" "cases/${test_name}.in.bak"
        cd "cases/${test_name}.in"
    fi

    status_code=0
    sh "$test_file" \
        < "$stdin_file" \
        > "$stdout_file" \
        2> "$stderr_file" \
        || status_code="$?"
    printf '%s' "$status_code" > "$status_file"
    
    if [ "$has_in_dir" = true ]; then
        cd "$original_pwd"
        rm -rf "cases/${test_name}.in"
        mv "cases/${test_name}.in.bak" "cases/${test_name}.in"
    fi
}

make_snapshot() {
    mkdir -p tmp_snapshot
    for test_file in cases/*.sh; do
        snapshot_test "$test_file"
    done
}

diff_test() {
    test_name="$1"

    printf 'Diffing test "%s"\n' "$test_name"

    status_code=0
    for suffix in stdout stderr status; do
        diff --unified "cases/${test_name}.${suffix}" "tmp_snapshot/${test_name}.${suffix}" \
            || status_code=1
    done
    return "$status_code"
}

case "$1" in
    dump)
        make_snapshot
        rm -rf dump
        mv tmp_snapshot dump
        ;;
    snapshot)
        make_snapshot
        mv tmp_snapshot/* .
        rm -rf tmp_snapshot
        ;;
    test)
        make_snapshot
        overall_status_code=0
        for test_file in cases/*.sh; do
            test_name=$(basename "$test_file" ".sh")
            diff_test "$test_name" || overall_status_code=1
        done
        rm -rf tmp_snapshot
        exit "$overall_status_code"
        ;;
    *)
        printf '%s\n' "Unknown command"
        exit 1
        ;;
esac
