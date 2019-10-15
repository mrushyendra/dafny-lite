#!/bin/bash

TestsDir="./tests"

cabal run parseImp $TestsDir/small.imp

cabal run parseImp $TestsDir/simpleAddition.imp

cabal run parseImp $TestsDir/mod.imp

cabal run parseImp $TestsDir/lcm.imp
