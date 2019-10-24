## Dafny-Lite
---
#### About
Dafny-Lite is a Verification Condition Generator capable of verifying programs written in the IMP Language. Given
an IMP program along with pre and post conditions as well as loop invariants, it computes the weakest precondition
and verifies the program using Microsoft's Z3 SMT Solver.

---

#### Dependencies
* GHC version 8.2.2: https://www.haskell.org/ghc/ (other GHC versions might work too)
* Z3 SMT Solver: https://github.com/Z3Prover/z3

---
#### Setup:
1) Install GHC 8.2.2 (other GHC versions might also work)
2) Install Z3
3) Add Z3 to the PATH
4) Create a new Cabal Sandbox (Optional)
5) Install the required packages
6) Build
`cabal build vcgen`

---
#### To run:

###### Command Line:

`cabal run vcgen ./Path/to/program.imp`
---
