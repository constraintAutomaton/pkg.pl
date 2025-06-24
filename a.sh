#!/bin/sh
set -u

write_install_result() {
    flock scryer_libs/temp/install_resp.pl.lock -c \
        "printf 'result(\"%s\", %s).\n' \"$1\" \"$2\" >> scryer_libs/temp/install_resp.pl"
}

write_lock_result() {

    flock scryer_libs/temp/lock_resp.pl.lock -c \
        "printf 'result(\"%s\", %s).\n' \"$1\" \"$2\" >> scryer_libs/temp/lock_resp.pl"
}

write_no_lock_operation() {
    write_lock_result "$1" "nothing"
}

write_install_success() {
    write_install_result "$1" "success"
}

write_lock_success() {
    write_lock_result "$1" "$2"
}

write_install_error() {
    escaped_error=$(printf '%s' "$2" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
    escaped_error=$(printf '%s' "$escaped_error" | tr '\r\n' '\\n')
    escaped_error=$(printf '%s' "$escaped_error" | sed 's/ / /g')
    write_install_result "$1" "error(\\\"$escaped_error\\\")"
}

write_lock_error() {
    escaped_error=$(printf '%s' "$2" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')
    escaped_error=$(printf '%s' "$escaped_error" | tr '\r\n' '\\n')
    escaped_error=$(printf '%s' "$escaped_error" | sed 's/ / /g')
    write_lock_result "$1" "error(\\\"$escaped_error\\\")"
}

lock_dependency() {
    dependency_name=$1

    ERROR=""

    cd "scryer_libs/packages/${dependency_name}"

    GIT_HASH=$(git rev-parse HEAD 2>&1) || ERROR="$ERROR git failure: $GIT_HASH"
    GIT_HASH=$(echo "$GIT_HASH" | head -n1)

    if [ -z "$ERROR" ]; then
        write_lock_success "${dependency_name}" "${GIT_HASH}"
    else
        write_lock_error "${dependency_name}" "${ERROR}"
    fi
}

install_git_default() {
    dependency_name=$1
    git_url=$2
    lock=$3

    error_output=$(
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            "${git_url}" \
            "scryer_libs/packages/${dependency_name}" 2>&1 1>/dev/null
    )

    if [ -z "$error_output" ]; then
        if [ "$lock" = "true" ]; then
            lock_dependency "${dependency_name}"
        else
            write_no_lock_operation "${dependency_name}"
        fi
        write_install_success "${dependency_name}"
    else
        write_install_error "${dependency_name}"
    fi
}

install_git_branch() {
    dependency_name=$1
    git_url=$2
    git_branch=$3
    lock=$4

    error_output=$(
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            --branch "${git_branch}" \
            "${git_url}" \
            "scryer_libs/packages/${dependency_name}" 2>&1 1>/dev/null
    )

    if [ -z "$error_output" ]; then
        if [ "$lock" = "true" ]; then
            lock_dependency "${dependency_name}"
        else
            write_no_lock_operation "${dependency_name}"
        fi
        write_install_success "${dependency_name}"
    else
        write_install_error "${dependency_name}"
    fi
}

install_git_tag() {
    dependency_name=$1
    git_url=$2
    git_tag=$3
    lock=$4

    error_output=$(
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            --branch "${git_tag}" \
            "${git_url}" \
            "scryer_libs/packages/${dependency_name}" 2>&1 1>/dev/null
    )

    if [ -z "$error_output" ]; then
        if [ "$lock" = "true" ]; then
            lock_dependency "${dependency_name}"
        else
            write_no_lock_operation "${dependency_name}"
        fi
        write_install_success "${dependency_name}"
    else
        write_install_error "${dependency_name}"
    fi
}

install_git_hash() {
    dependency_name=$1
    git_url=$2
    git_hash=$3
    lock=$4

    error_output=$(
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            "${git_url}" \
            "scryer_libs/packages/${dependency_name}" 2>&1 1>/dev/null
    )

    if [ -z "$error_output" ]; then
        fetch_error=$(
            git -C "scryer_libs/packages/${dependency_name}" fetch \
                --quiet \
                --depth 1 \
                origin "${git_hash}" 2>&1 1>/dev/null
        )
        switch_error=$(
            git -C "scryer_libs/packages/${dependency_name}" switch \
                --quiet \
                --detach \
                "${git_hash}" 2>&1 1>/dev/null
        )
        combined_error="${fetch_error}; ${switch_error}"

        
        if [ -z "$fetch_error" ] && [ -z "$switch_error" ]; then
            if [ "$lock" = "true" ]; then
                lock_dependency "${dependency_name}"
            else
                write_no_lock_operation "${dependency_name}"
            fi
            write_install_success "${dependency_name}"
        else
            write_install_error "${dependency_name}"
        fi
    else
        write_install_error "${dependency_name}"
    fi
}

install_path() {
    dependency_name=$1
    dependency_path=$2

    if [ -d "${dependency_path}" ]; then
        error_output=$(ln -rsf "${dependency_path}" "scryer_libs/packages/${dependency_name}" 2>&1 1>/dev/null)

        if [ -z "$error_output" ]; then
            write_no_lock_operation "${dependency_name}"
        else
            write_install_error "${dependency_name}" "$error_output"
        fi
    else
        write_install_error "${dependency_name}" "${dependency_path} does not exist"
    fi
}

OLD_IFS=$IFS
IFS='|'
set -- $DEPENDENCIES_STRING
IFS=$OLD_IFS

touch scryer_libs/temp/install_resp.pl
touch scryer_libs/temp/lock_resp.pl

for dependency in "$@"; do
    unset dependency_term dependency_kind dependency_name git_url git_branch git_tag git_hash dependency_path lock

    IFS=';'
    set -- $dependency
    IFS=$OLD_IFS

    while [ "$#" -gt 0 ]; do
        field=$1
        shift

        key=$(printf "%s" "$field" | cut -d= -f1)
        value=$(printf "%s" "$field" | cut -d= -f2-)

        case "$key" in
        dependency_term) dependency_term=$value ;;
        dependency_kind) dependency_kind=$value ;;
        dependency_name) dependency_name=$value ;;
        git_url) git_url=$value ;;
        git_branch) git_branch=$value ;;
        git_tag) git_tag=$value ;;
        git_hash) git_hash=$value ;;
        dependency_path) dependency_path=$value ;;
        lock) lock=$value ;;
        esac
    done

    printf "Ensuring is installed: %s\n" "${dependency_term}"

    case "${dependency_kind}" in
    do_nothing) ;;

    git_default)
        install_git_default "${dependency_name}" "${git_url}" "${lock}" &
        ;;
    git_branch)
        install_git_branch "${dependency_name}" "${git_url}" "${git_branch}" "${lock}" &
        ;;
    git_tag)
        install_git_tag "${dependency_name}" "${git_url}" "${git_tag}" "${lock}" &
        ;;
    git_hash)
        install_git_hash "${dependency_name}" "${git_url}" "${git_hash}" "${lock}" &
        ;;
    path)
        install_path "${dependency_name}" "${dependency_path}" &
        ;;
    *)
        printf "Unknown dependency kind: %s\n" "${dependency_kind}"
        write_install_error "${dependency_name}" "Unknown dependency kind: ${dependency_kind}"
        ;;
    esac
done

wait

rm -f scryer_libs/temp/install_resp.pl.lock
rm -f scryer_libs/temp/lock_resp.pl.lock
