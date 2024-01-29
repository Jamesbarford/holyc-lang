#!/usr/bin/env bash
# A simple yet fairly effective way of being able to run unit tests

runTest() {
    local file="${1}"
    if [[ -z "${file}" ]]; then
        echo >&2 "Must provide a file to run a test for"
        exit 1
    fi
    local asm_file=$(echo $file | tr -d '.HC')
    if [[ "$file" = "testhelper.HC" ]]; then
        return
    fi
    hcc "./${file}" > /dev/null
    if [[ $? != 0 ]]; then
        echo "Failed: ${file}"
        return
    fi
    ./a.out
    rm ./a.out 
}

main() {
    local files=$(ls | grep '.HC$')
    for file in ${files[@]}; do
        runTest "${file}"
    done
}

main
