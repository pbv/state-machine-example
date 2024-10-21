# QuickCheck state machine testing example

Testing a C implementation of a bounded queue using the Haskell
`quickcheck-state-machine` library.

## Instructions

~~~bash
$ cd lib
$ make
$ cd ..
$ stack build
$ LD_LIBRARY_PATH=./lib stack run --rts-options -N
~~~

Note the RTS options to run on multicores for parallel testing.

Shrinking can find buggy counterexamples in parallel testing where a
pre-condition to dequeue fails.  I believe this is related to this
[issue](https://github.com/stevana/quickcheck-state-machine/issues/51]).

---

Pedro Vasconcelos, 2024
