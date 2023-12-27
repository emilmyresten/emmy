### Emmy
Tree-walking interpreter implementation of a LISP-type language implemented in OCaml. Toy project. Syntax inspired by Clojure. 

### Ongoing
Module system: 
- File paths do not resolve correctly (currently need to import using relative path from compilation root, or absolute path). Should instead be relative to the file passed to the interpreter imo.
- All names should be namespaced. Right now all source files are summed up into the same character stream, and without renaming, conflicts will occur.
- Detect cyclic dependencies. Currently such cycles will result in an infinite loop.
- Enable importing specific definitions. Currently, only wildcard imports are possible (the entire source file is imported).


### Examples

`Calculate the nth fibonacci number`:
![](assets/fibonacci.png)

`Calculate the number of iterations before the Collatz equation reaches 1:`
![](assets/collatz.png)

#### Build:

`opam install --deps-only --with-test .`

#### Run REPL:

`dune exec emmy`

#### Run on source files:

`dune exec emmy ./examples/main.emmy`

#### Tests:
Testing is done with [alcotest](https://github.com/mirage/alcotest).

`dune runtest`

#### Debug:

`ocamldebug _build/install/default/bin/emmy`

`(ocd) info modules`

##### To set breakpoint: 

`break @ <module> linenumber`
