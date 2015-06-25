# Haskell solutions for 2015 Java course

This repo will contain Haskell solutions for exercises from Freiburg
University's Java programming course (edition summer term 2015). Each exercise
I could be bothered to solve can be found in the corresponding folder
(`exXX_Y/` for sheet `XX`, exercise `Y`). The code is always in `src/`; the
rest is Haskell-specific scaffolding. Some directories contain solutions for
multiple exercises (e.g. ex05\_3 contains ex05\_2 and ex05\_3).

To build an exercise project:

1. Download and install the
   [`stack`](https://github.com/commercialhaskell/stack) build tool.
2. Inside the `exXX_Y` directory, run `stack build`. You may have to run a
   setup command first to obtain a Haskell build environment; if so, `stack`
   will tell you to.
