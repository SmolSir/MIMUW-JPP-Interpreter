#!/bin/bash

interpreter=interpreter
examples_path=examples

echo -e "Correct examples"

good_total=0
good_passed=0

for example in $examples_path/good/*.txt; do
    ((good_total=good_total+1))

    echo -e "$example:\n"
    ./$interpreter "$example"

    if [[ $? -eq 0 ]]; then
        ((good_passed=good_passed+1))
        echo -e "OK\n"
    else
        echo -e "ERROR\n"
    fi
done

bad_total=0
bad_passed=0

for example in $examples_path/bad/*.txt; do
    ((bad_total=bad_total+1))

    echo -e "$example:\n"
    ./$interpreter "$example"

    if [[ $? -eq 1 ]]; then
        ((bad_passed=bad_passed+1))
        echo -e "OK\n"
    else
        echo -e "ERROR\n"
    fi
done

total=$((good_total+bad_total))
passed=$((good_passed+bad_passed))

echo -e "Good examples:  $good_passed/$good_total"
echo -e "Bad examples:   $bad_passed/$bad_total"
echo -e "Total examples: $passed/$total"
