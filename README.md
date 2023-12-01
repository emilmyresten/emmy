### Interp
Tree-walking interpreter implementation of a LISP-type language implemented in OCaml. Toy project. Syntax inspired by Clojure. 

```
$ (def fib 
    (fn x -> 
      (cond 
        (< x 3) 1 
        (+ (fib (- x 1)) 
           (fib (- x 2))))))
$ (fib 10)
> 55
```

### Build & run:

`opam install --deps-only --with-test .`
`dune exec interp`

#### Tests:
Testing is done with [alcotest](https://github.com/mirage/alcotest).

`dune runtest`

### Debug:

`ocamldebug _build/install/default/bin/interp`

`(ocd) info modules`

#### To set breakpoint: 

`break @ <module> linenumber`