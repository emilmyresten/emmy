open Alcotest
open Interp

let simple_interp_test ?(expected_context = "") in_program expected_result () =
  let eval, ctx = Interpreter.eval_program in_program [] in
  let expected_eval = expected_result in
  if not (String.equal expected_context "") then
    check string "test context" expected_context (Pprint.string_of_context ctx);
  check string "test return value" expected_eval (Pprint.string_of_expr eval)

(* `Quick marks it as a fast running test, can be ran every time.
   the alternative is `Slow, that can be suppressed. Unclear how to do that using dune. *)
let suite =
  [
    (let integers = "(def x 1)"
     and expected_result = "Unit"
     and expected_context = "x = 1\n" in
     ( integers,
       `Quick,
       simple_interp_test ~expected_context integers expected_result ));
    (let booleans = "(def x true)"
     and expected_result = "Unit"
     and expected_context = "x = true\n" in
     ( booleans,
       `Quick,
       simple_interp_test ~expected_context booleans expected_result ));
    (let equality = "(= 1 1)" and expected_result = "true" in
     (equality, `Quick, simple_interp_test equality expected_result));
    (let program = "(def id (fn x -> x))" and expected_result = "Unit" in
     (program, `Quick, simple_interp_test program expected_result));
    (* let ';' symbolize separate expressions, new command in repl. *)
    (let program = "(def id (fn x -> x)); \n      (id 1)"
     and expected_result = "1" in
     (program, `Quick, simple_interp_test program expected_result));
    (let hofs = "(def id (fn x -> x)); \n      ((id (fn y -> y)) 1)"
     and expected_result = "1" in
     (hofs, `Quick, simple_interp_test hofs expected_result));
    (let return_fn = "(def id (fn -> (fn y -> y))); \n      (id)"
     and expected_result = "(function y#398 -> Identifier y#398)" in
     (return_fn, `Quick, simple_interp_test return_fn expected_result));
    (let scoping =
       "(def alpha_converted (fn x -> (fn x -> (fn x -> x))));\n\
       \      (((alpha_converted 1) 2) 3)"
     and expected_result = "3" in
     (scoping, `Quick, simple_interp_test scoping expected_result));
    (let scoping =
       "(def alpha_converted (fn x -> (fn x -> x)));\n\
       \      ((alpha_converted 1) 2)"
     and expected_result = "2" in
     (scoping, `Quick, simple_interp_test scoping expected_result));
    (let scoping =
       "(def alpha_converted (fn x -> (fn y -> (fn z -> x))));\n\
       \      (((alpha_converted 1) 2) 3)"
     and expected_result = "1" in
     (scoping, `Quick, simple_interp_test scoping expected_result));
    (let multi_arity = "(def multi_arity (fn x y z -> z))"
     and expected_result = "Unit" in
     (multi_arity, `Quick, simple_interp_test multi_arity expected_result));
    (let multi_arity =
       "(def multi_arity (fn x y z -> ((fn x y -> x) x y))); \n\
       \      (multi_arity 1 2 3)"
     and expected_result = "1" in
     (multi_arity, `Quick, simple_interp_test multi_arity expected_result));
    (let multi_arity =
       "(def multi_arity (fn -> ((fn x y -> x) 1 2))); \n      (multi_arity)"
     and expected_result = "1" in
     (multi_arity, `Quick, simple_interp_test multi_arity expected_result));
    (let arity_0 = "(def arity_0 (fn -> 1)); \n      (arity_0)"
     and expected_result = "1"
     and expected_context = "arity_0 = (function  -> 1)\n" in
     ( arity_0,
       `Quick,
       simple_interp_test ~expected_context arity_0 expected_result ));
    (let addition = "(def inc (fn x -> (+ x 1))); \n      (inc 1)"
     and expected_result = "2"
     and expected_context = "inc = (function x -> (+ Identifier x 1))\n" in
     ( addition,
       `Quick,
       simple_interp_test ~expected_context addition expected_result ));
    (let subtraction = "(def dec (fn x -> (- x 1))); \n      (dec 1)"
     and expected_result = "0"
     and expected_context = "dec = (function x -> (- Identifier x 1))\n" in
     ( subtraction,
       `Quick,
       simple_interp_test ~expected_context subtraction expected_result ));
    (let multiplication =
       "(def multiply (fn x -> (* x 2))); \n      (multiply 2)"
     and expected_result = "4"
     and expected_context = "multiply = (function x -> (* Identifier x 2))\n" in
     ( multiplication,
       `Quick,
       simple_interp_test ~expected_context multiplication expected_result ));
    (let division = "(def divide (fn x -> (/ x 2))); \n      (divide 2)"
     and expected_result = "1"
     and expected_context = "divide = (function x -> (/ Identifier x 2))\n" in
     ( division,
       `Quick,
       simple_interp_test ~expected_context division expected_result ));
    (let cond_case = "(cond (= 0 1) 1 (= 2 2) 2 3)" and expected_result = "2" in
     (cond_case, `Quick, simple_interp_test cond_case expected_result));
    (let and_fn =
       "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 \
        pred2) true false)));\n\
       \       (and (= 1 0) (= 2 2))"
     and expected_result = "false" in
     (and_fn, `Quick, simple_interp_test and_fn expected_result));
    (let and_fn_2 =
       "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 \
        pred2) true false)));\n\
       \       (and (= 1 1) (= 2 2))"
     and expected_result = "true" in
     (and_fn_2, `Quick, simple_interp_test and_fn_2 expected_result));
    (let not_fn = "(def not (fn x -> (cond x false true)));\n       (not 1)"
     and expected_result = "false" in
     (not_fn, `Quick, simple_interp_test not_fn expected_result));
    (let not_fn_2 =
       "(def not \n\
       \        (fn x -> \n\
       \          (cond \n\
       \            x false \n\
       \            true)));\n\
       \       (not false)"
     and expected_result = "true" in
     (not_fn_2, `Quick, simple_interp_test not_fn_2 expected_result));
    (let or_fn =
       "(def and \n\
       \        (fn pred1 pred2 -> \n\
       \          (cond \n\
       \            (= pred1 false) false \n\
       \            (= pred1 pred2) true \n\
       \            false)));\n\
       \       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= \
        pred1 true) true (= pred2 true) true false)));\n\
       \       (or (= 1 1) (= 1 0))"
     and expected_result = "true" in
     (or_fn, `Quick, simple_interp_test or_fn expected_result));
    (let or_fn_2 =
       "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 \
        pred2) true false)));\n\
       \       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= \
        pred1 true) true (= pred2 true) true false)));\n\
       \       (or (= 0 1) (= 1 1))"
     and expected_result = "true" in
     (or_fn_2, `Quick, simple_interp_test or_fn_2 expected_result));
    (let or_fn_3 =
       "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 \
        pred2) true false)));\n\
       \       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= \
        pred1 true) true (= pred2 true) true false)));\n\
       \       (or (= 1 1) (= 1 1))"
     and expected_result = "true" in
     (or_fn_3, `Quick, simple_interp_test or_fn_3 expected_result));
    (let or_fn_4 =
       "(def and \n\
       \        (fn pred1 pred2 -> \n\
       \          (cond \n\
       \            (= pred1 false) false \n\
       \            (= pred1 pred2) true \n\
       \            false)));\n\
       \       (def or \n\
       \        (fn pred1 pred2 -> \n\
       \          (cond\n\
       \           (and pred1 pred2) true \n\
       \           (= pred1 true) true \n\
       \           (= pred2 true) true \n\
       \           false)));\n\
       \       (or (= 0 1) (= 0 1))"
     and expected_result = "false" in
     (or_fn_4, `Quick, simple_interp_test or_fn_4 expected_result));
    (let recurse =
       "(def fib (fn x -> (cond (< x 3) 1 (+ (fib (- x 1)) (fib (- x 2))))));\n\
       \      (fib 10)"
     and expected_result = "55" in
     (recurse, `Quick, simple_interp_test recurse expected_result));
    (let less_than = "(< 5 10)" and expected_result = "true" in
     (less_than, `Quick, simple_interp_test less_than expected_result));
    (let less_than_or_eq =
       "(def and \n\
       \        (fn pred1 pred2 -> \n\
       \          (cond \n\
       \            (= pred1 false) false \n\
       \            (= pred1 pred2) true \n\
       \            false)));\n\
       \       (def or \n\
       \        (fn pred1 pred2 -> \n\
       \          (cond\n\
       \           (and pred1 pred2) true \n\
       \           (= pred1 true) true \n\
       \           (= pred2 true) true \n\
       \           false)));\n\
       \       (def <= \n\
       \        (fn x y -> \n\
       \          (or \n\
       \            (= x y) \n\
       \            (< x y))));\n\
       \       (<= 1 1)"
     and expected_result = "true" in
     ( less_than_or_eq,
       `Quick,
       simple_interp_test less_than_or_eq expected_result ));
    (let collatz =
       "(def collatz\n\
       \        (fn x count -> \n\
       \          (cond\n\
       \           (= x 1) count\n\
       \           (= (% x 2) 0) (collatz (/ x 2) (+ count 1))\n\
       \           (collatz (+ (* 3 x) 1) (+ count 1))))); \n\
       \ (collatz 10 0)"
     and expected_result = "6" in
     (collatz, `Quick, simple_interp_test collatz expected_result));
    (let let_bindings =
       "(def l        \n\
       \            (let [x 4]\n\
       \                  x));\n\
       \        l"
     and expected_result = "4" in
     (let_bindings, `Quick, simple_interp_test let_bindings expected_result));
    (let let_bindings_in_fn =
       "(def l_fn   \n\
       \         (fn x ->\n\
       \            (let [is-zero (= x 0)\n\
       \                  is-under (< x 0)\n\
       \                  is-over (< 0 x)]\n\
       \              (cond\n\
       \                is-zero \"zero\"\n\
       \                is-under \"under\"\n\
       \                is-over \"over\"\n\
       \                \"default\"))));\n\
       \              (l_fn 1)"
     and expected_result = "\"over\"" in
     ( let_bindings_in_fn,
       `Quick,
       simple_interp_test let_bindings_in_fn expected_result ));
    (let nested_lets =
       "(def l \n\
       \          (let [x 4] \n\
       \            (let [y (+ x 1)]\n\
       \              y)));\n\n\
       \                            \n\
       \        l"
     and expected_result = "5" in
     (nested_lets, `Quick, simple_interp_test nested_lets expected_result));
    (let fns_in_let =
       "(def f\n\
       \          (let [x (fn y -> (+ y 1))] \n\
       \            (x 2)));\n\n\
       \                            \n\
       \        f"
     and expected_result = "3" in
     (fns_in_let, `Quick, simple_interp_test fns_in_let expected_result));
    (let lists_test = "(def f [1 2 3]); f" and expected_result = "[1 2 3]" in
     (lists_test, `Quick, simple_interp_test lists_test expected_result));
    (let list_index_test = "([1 2 3] 0)" and expected_result = "1" in
     ( list_index_test,
       `Quick,
       simple_interp_test list_index_test expected_result ));
    (let list_index_test = "(def x [1 2 3]); (x 2)" and expected_result = "3" in
     ( list_index_test,
       `Quick,
       simple_interp_test list_index_test expected_result ));
  ]

let interp_tests () = Alcotest.run "Interp" [ ("E2E", suite) ]
let () = interp_tests ()
