#!/bin/bash

TestsDir="./tests"
passed=0
failed=0

function test {
    echo "----------------"
    Test=$1
    expected=$2
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

echo "-------------------"
echo "Test Invalid Tests: "

test "invalid/find_invalid.imp" "Not verified"
test "invalid/order_invalid.imp" "Not verified"
test "invalid/prime_invalid.imp" "Not verified"

echo "Tests Summary: "
echo "Passed: $passed Failed: $failed"
