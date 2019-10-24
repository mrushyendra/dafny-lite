#!/bin/bash

TestsDir="./tests"
passed=0
failed=0

function test {
    Test=$1
    expected=$2
    echo "----------------"
    echo "Testing: $Test"
    res=$(cabal run vcgen $TestsDir/$Test)

    if [[ $res =~ $expected ]];
        then 
            echo "Test Passed. Got $expected" 
            passed=$((passed+1))
        else 
            echo "Test Failed. Got $res. Expected $expected"
            failed=$((failed+1))
    fi
}

echo "------------------"
echo "Test Valid Tests: "

test "valid/bubble.imp" "Verified"
test "valid/find.imp" "Verified"
test "valid/gcd.imp" "Verified"
test "valid/lcm.imp" "Verified"
test "valid/mod.imp" "Verified"
test "valid/mult.imp" "Verified"
test "valid/order.imp" "Verified"
test "valid/prime.imp" "Verified"
test "valid/rev.imp" "Verified"
#test "../../CS454-2018-master/Benchmarks/valid/factorial.imp" "Verified"
test "../../CS454-2018-master/Benchmarks/valid/binary_search.imp" "Verified"
test "../../CS454-2018-master/Benchmarks/valid/findMax.imp" "Verified"
test "../../CS454-2018-master/Benchmarks/valid/sort3.imp" "Verified"
test "../../CS454-2018-master/grading/valid_student/collatz.imp" "Verified"
test "../../CS454-2018-master/grading/valid_student/findOdd.imp" "Verified"
#test "../../CS454-2018-master/grading/valid_student/realbubble.imp" "Verified"
test "../../CS454-2018-master/grading/valid_student/scale_array.imp" "Verified"
test "../../CS454-2018-master/grading/valid_student/setAll2Two.imp" "Verified"
test "../../CS454-2018-master/grading/valid_student/testwhile.imp" "Verified"
test "../../CS454-2018-master/grading/valid_student/xsign.imp" "Verified"

echo "-------------------"
echo "Test Invalid Tests: "

test "invalid/find_invalid.imp" "Not verified"
test "invalid/order_invalid.imp" "Not verified"
test "invalid/prime_invalid.imp" "Not verified"
test "../../CS454-2018-master/Benchmarks/invalid/bubble_bad.imp" "Not verified"
test "../../CS454-2018-master/Benchmarks/invalid/mult_bad.imp" "Not verified"
test "../../CS454-2018-master/Benchmarks/invalid/findMax_bad.imp" "Not verified"
test "../../CS454-2018-master/Benchmarks/invalid/rev_bad.imp" "Not verified"

echo "Tests Summary: "
echo "Passed: $passed Failed: $failed"


