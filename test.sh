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

parser_tests="tests/parser/*.prog"
typechecker_tests="tests/typechecker/*.prog"
tests="tests/*.prog"

total_count=0
passed_count=0

error_message=""
output=""
failed=0

test_error()
{
    input_filepath=$1
    output_filepath=$2
    set +e
    output=$(./zig-out/bin/prog-lang ${input_filepath} -o ${output_filepath} 2>&1)
    set -e
}

test_output()
{
    input_filepath=$1
    output_filepath=$2
    set +e
    error_messages=$(./zig-out/bin/prog-lang ${input_filepath} -o ${output_filepath} 2>&1)
    set -e
    if [ "$error_messages" = "" ]; then
        output=$(./zig-out/bin/prog-lang -r ${output_filepath})
    else
        failed=1
    fi
}

test_all()
{
    files=$1
    test_fn=$2
    output_filepath="output.tmp.irc"

    for file in $files
    do
        printf "Testing \"%s\"... " $file

        ((total_count += 1))

        base_file_path=${file%.*}
        input_filepath="${base_file_path}.prog"
        expected_filepath="${base_file_path}.expect"

        error_messages=""
        output=""
        failed=0
        $test_fn $input_filepath $output_filepath

        if [ -e ${expected_filepath} ]; then
            expected=$(cat ${expected_filepath})

            if [ $failed -eq 0 ] && [ "${output}" = "${expected}" ]; then
                echo -e "${green}OK${reset}"
                ((passed_count += 1))
            else
                echo -e "${red}FAILED${reset}"
            fi
        else
            # echo -n "${output}" > ${expected_filepath}
            echo -e "${red}missing expected output${reset}"
        fi
    done

    if [ -e ${output_filepath} ]; then
        rm ${output_filepath}
    fi
}

test_all "$parser_tests" test_error
test_all "$typechecker_tests" test_error
test_all "$tests" test_output

failed_count=$((total_count-passed_count))

echo ""
echo -e "${green}Passed${reset}/${red}Failed${reset}/Total ----- ${green}${passed_count}${reset}/${red}${failed_count}${reset}/${total_count} tests"

if [ $passed_count -ne $total_count ]; then
    exit 1
fi
