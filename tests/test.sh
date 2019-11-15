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
#test "valid/factorial.imp" "Verified"
test "valid/binary_search.imp" "Verified"
test "valid/findMax.imp" "Verified"
test "valid/sort3.imp" "Verified"
test "valid/collatz.imp" "Verified"
test "valid/findOdd.imp" "Verified"
#test "valid/realbubble.imp" "Verified"
test "valid/scale_array.imp" "Verified"
test "valid/setAll2Two.imp" "Verified"
test "valid/testwhile.imp" "Verified"
test "valid/xsign.imp" "Verified"

echo "-------------------"
echo "Test Invalid Tests: "

test "invalid/find_invalid.imp" "Not verified"
test "invalid/order_invalid.imp" "Not verified"
test "invalid/prime_invalid.imp" "Not verified"
test "invalid/bubble_bad.imp" "Not verified"
test "invalid/mult_bad.imp" "Not verified"
test "invalid/findMax_bad.imp" "Not verified"
test "invalid/rev_bad.imp" "Not verified"

echo "Tests Summary: "
echo "Passed: $passed Failed: $failed"


