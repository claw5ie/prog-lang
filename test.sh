#!/bin/bash

set -eu

red="\e[31m"
green="\e[32m"
reset="\e[0m"

if [ -t 1 ]; then
    colors=$(tput colors)
    if [ $colors -eq -1 ]; then
        red=""
        green=""
        reset=""
    fi
fi

{
    parser_tests="tests/parser/*.test"
    typechecker_tests="tests/typechecker/*.test"
    ircode_tests="tests/*.test"
}
tests="${parser_tests} ${typechecker_tests} ${ircode_tests}"
total_count=0
passed_count=0

for file in $tests
do
    ((total_count += 1))

    printf "Testing \"%s\"... " $file

    base_file_path=${file%.*}
    if [ -e "${base_file_path}.expect" ]; then
        set +e
        output=$(./zig-out/bin/prog-lang "${base_file_path}.test" 2>&1)
        set -e
        expected=$(cat "${base_file_path}.expect")

        if [ "$output" == "$expected" ]; then
            echo -e "${green}OK${reset}"
            ((passed_count += 1))
        else
            echo -e "${red}FAILED${reset}"
        fi
    else
        echo -e "\e[31mmissing expected output\e[0m"
    fi
done

failed_count=$((total_count-passed_count))

echo ""
echo -e "${green}Passed${reset}/${red}Failed${reset}/Total ----- ${green}${passed_count}${reset}/${red}${failed_count}${reset}/${total_count} tests"

if [ $passed_count -ne $total_count ]; then
    exit 1
fi
