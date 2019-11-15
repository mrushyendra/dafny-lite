## Dafny-Lite
---
#### About
Dafny-Lite is a Verification Condition Generator capable of verifying programs written in the IMP Language. Given
an IMP program with pre and post conditions and loop invariants, it computes the weakest precondition
and verifies the program using Microsoft's Z3 SMT Solver.

If all postconditions and loop invariants are verified, Dafny-Lite prints 'Verified'. Else, it prints 'Not Verified'.

---

#### Dependencies
* GHC version 8.2.2: https://www.haskell.org/ghc/ (other GHC versions will probably work too)
* Z3 SMT Solver: https://github.com/Z3Prover/z3

---
#### Setup:
1) Install GHC 8.2.2 (other GHC versions will probably work too)
2) Install Microsoft Z3 (https://github.com/Z3Prover/z3)
3) Add Z3 to the PATH
4) Create a new Cabal Sandbox (Optional)
5) Install the required packages
6) Build:
`cabal build vcgen`

---
#### To run:

Command Line:

Run the built executable with any IMP program:
`cabal run vcgen ./Path/to/program.imp`

---
