#!/bin/sh
set -eu

echo "Ensuring is installed: ${DEPENDENCY_TERM}"

rm --recursive --force scryer_libs/tmp-package

relocate_tmp() {
    rm --recursive --force "scryer_libs/packages/${DEPENDENCY_NAME}"
    mv scryer_libs/tmp-package "scryer_libs/packages/${DEPENDENCY_NAME}"
}

case "${DEPENDENCY_KIND}" in
    git_default)
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            "${GIT_URL}" \
            scryer_libs/tmp-package
        relocate_tmp
        ;;
    git_branch)
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            --branch "${GIT_BRANCH}" \
            "${GIT_URL}" \
            scryer_libs/tmp-package
        relocate_tmp
        ;;
    git_tag)
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            --branch "${GIT_TAG}" \
            "${GIT_URL}" \
            scryer_libs/tmp-package
        relocate_tmp
        ;;
    git_hash)
        git clone \
            --quiet \
            --depth 1 \
            --single-branch \
            "${GIT_URL}" \
            scryer_libs/tmp-package
        git -C scryer_libs/tmp-package fetch \
            --quiet \
            --depth 1 \
            origin "${GIT_HASH}"
        git -C scryer_libs/tmp-package switch \
            --quiet \
            --detach \
            "${GIT_HASH}"
        relocate_tmp
        ;;
    path)
        ln -rsf "${DEPENDENCY_PATH}" "scryer_libs/packages/${DEPENDENCY_NAME}"
        ;;
    *)
        echo "Unknown dependency kind"
        exit 1
        ;;
esac
