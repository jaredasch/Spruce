# project-cis5520

This is an "Empty project" for Haskell. It is configured in the same way as
the lecture demo and homework assignments for CIS 5520, but contains no
code. Feel free to use this project for experimentation!

If you want to change the name of this project, look for all occurrences of
`project-cis5520` in the `project-cis5520.cabal` file. (And change the name of
the file to match!)


## Module organization

Haskell packages typically divide their source code into three separate places:
  - The bulk of your code should be developed as a reusable library in modules in the `src` directory. We've created [Lib.hs](src/Lib.hs) for you to get started. You can add additional modules here.
  
  - The entry point for your executable is in [Main.hs](app/Main.hs). 
  
  - All of your test cases should be in [the test directory](test/Spec.hs).

## Building, running, and testing

This project compiles with `stack build`. 
You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Finally, you can start a REPL with `stack ghci`.

## Importing additional libraries

This project is designed to run with stackage: you can easily use any library
in https://www.stackage.org/lts-19.19 by adding an entry to the
`build-depends` list of the `common-stanza` in the cabal file. If you want to
use a library that is not on stackage, you'll need to update the common-stanza
*and* add information to `stack.yaml` about where to find that library.

