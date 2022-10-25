# pl0-haskell

Parser, interpreter and compiler (WIP) for [PL/0](https://en.wikipedia.org/wiki/PL/0), an educational programming language designed by Niklaus Wirth.

For documentation, see [here](https://fpringle.github.io/pl0-haskell/).

## Instructions

Run interpreter with cabal:
```bash
cabal new-run exe:pl0 run test/primes_under_100.pl0
```

Run compiler (WIP) with cabal:
```bash
cabal new-run exe:pl0 compile test/first_10_squares.pl0
```
