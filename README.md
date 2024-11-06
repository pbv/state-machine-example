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

The vanilla Haskell bindings to the C implementation are not
thread-safe, and this is detected by the parallel testing.

The alternative bindings in `QueueSafeAPI.hs` enforce mutual exclusion
using Haskell's `MVar`s and *are* thread safe.


---

Pedro Vasconcelos, 2024
