open Alcotest
open Interp



let simple_interp_test ?(expected_context = "") in_program expected_result () =
  let (eval, ctx) = Interpreter.eval_program in_program [] in
  let expected_eval = expected_result in
  if (not (String.equal expected_context "")) then
    check string "test context" expected_context (Pprint.string_of_context ctx); 
  check string "test return value" expected_eval (Pprint.string_of_expr eval)

  (* `Quick marks it as a fast running test, can be ran every time.
     the alternative is `Slow, that can be suppressed. Unclear how to do that using dune. *)
let suite =
  [ 
    (let integers = "(def x 1)" 
    and expected_result = "Unit" and expected_context = "x = Integer 1\n" in 
    integers, `Quick, simple_interp_test ~expected_context integers expected_result); 

    (let booleans = "(def x true)" 
    and expected_result = "Unit" and expected_context = "x = true\n" in 
    booleans, `Quick, simple_interp_test ~expected_context booleans expected_result); 

    (let equality = "(= 1 1)" 
    and expected_result = "true" in 
    equality, `Quick, simple_interp_test equality expected_result); 

    (let program = "(def id (fn x -> x))"
    and expected_result = "Unit" in 
    program, `Quick, simple_interp_test program expected_result); 

    (* let ';' symbolize separate expressions, new command in repl. *)
    (let program = 
      "(def id (fn x -> x)); 
      (id 1)" 
    and expected_result = "Integer 1" in
    program, `Quick, simple_interp_test program expected_result); 

    (let hofs = 
      "(def id (fn x -> x)); 
      ((id (fn y -> y)) 1)"
    and expected_result = "Integer 1" in 
    hofs, `Quick, simple_interp_test hofs expected_result); 

    (let return_fn = 
      "(def id (fn -> (fn y -> y))); 
      (id)"
    and expected_result = "(Function y#398 -> Identifier y#398)" in 
    return_fn, `Quick, simple_interp_test return_fn expected_result); 

    (let scoping = 
      "(def alpha_converted (fn x -> (fn x -> (fn x -> x))));
      (((alpha_converted 1) 2) 3)"
    and expected_result = "Integer 3" in 
    scoping, `Quick, simple_interp_test scoping expected_result); 

    (let scoping = 
      "(def alpha_converted (fn x -> (fn x -> x)));
      ((alpha_converted 1) 2)"
    and expected_result = "Integer 2" in 
    scoping, `Quick, simple_interp_test scoping expected_result); 

    (let scoping = 
      "(def alpha_converted (fn x -> (fn y -> (fn z -> x))));
      (((alpha_converted 1) 2) 3)"
    and expected_result = "Integer 1" in 
    scoping, `Quick, simple_interp_test scoping expected_result); 

    (let multi_arity = "(def multi_arity (fn x y z -> z))" 
    and expected_result = "Unit" in 
    multi_arity, `Quick, simple_interp_test multi_arity expected_result); 

    (let multi_arity = 
      "(def multi_arity (fn x y z -> ((fn x y -> x) x y))); 
      (multi_arity 1 2 3)" 
    and expected_result = "Integer 1" in 
    multi_arity, `Quick, simple_interp_test multi_arity expected_result); 

    (let multi_arity = 
      "(def multi_arity (fn -> ((fn x y -> x) 1 2))); 
      (multi_arity)" 
    and expected_result = "Integer 1" in 
    multi_arity, `Quick, simple_interp_test multi_arity expected_result); 

    (let arity_0 = 
      "(def arity_0 (fn -> 1)); 
      (arity_0)" 
    and expected_result = "Integer 1"
    and expected_context = "arity_0 = (Function -> Integer 1)\n" in
    arity_0, `Quick, simple_interp_test ~expected_context arity_0 expected_result); 


    (let addition = 
      "(def inc (fn x -> (+ x 1))); 
      (inc 1)" 
    and expected_result = "Integer 2"
    and expected_context = "inc = (Function x -> (+ Identifier x Integer 1))\n" in
    addition, `Quick, simple_interp_test ~expected_context addition expected_result); 

    (let subtraction = 
      "(def dec (fn x -> (- x 1))); 
      (dec 1)" 
    and expected_result = "Integer 0"
    and expected_context = "dec = (Function x -> (- Identifier x Integer 1))\n" in
    subtraction, `Quick, simple_interp_test ~expected_context subtraction expected_result); 

    (let multiplication = 
      "(def multiply (fn x -> (* x 2))); 
      (multiply 2)" 
    and expected_result = "Integer 4"
    and expected_context = "multiply = (Function x -> (* Identifier x Integer 2))\n" in
    multiplication, `Quick, simple_interp_test ~expected_context multiplication expected_result); 

    (let division = 
      "(def divide (fn x -> (/ x 2))); 
      (divide 2)" 
    and expected_result = "Integer 1"
    and expected_context = "divide = (Function x -> (/ Identifier x Integer 2))\n" in
    division, `Quick, simple_interp_test ~expected_context division expected_result); 

    (let cond_case = "(cond (= 0 1) 1 (= 2 2) 2 3)" 
    and expected_result = "Integer 2" in
    cond_case, `Quick, simple_interp_test cond_case expected_result); 

    (let and_fn = 
      "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 pred2) true false)));
       (and (= 1 0) (= 2 2))" 
    and expected_result = "false" in
    and_fn, `Quick, simple_interp_test and_fn expected_result); 

    (let and_fn_2 = 
      "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 pred2) true false)));
       (and (= 1 1) (= 2 2))" 
    and expected_result = "true" in
    and_fn_2, `Quick, simple_interp_test and_fn_2 expected_result); 

    (let not_fn = 
      "(def not (fn x -> (cond x false true)));
       (not 1)" 
    and expected_result = "false" in
    not_fn, `Quick, simple_interp_test not_fn expected_result); 

    (let not_fn_2 = 
      "(def not (fn x -> (cond x false true)));
       (not false)" 
    and expected_result = "true" in
    not_fn_2, `Quick, simple_interp_test not_fn_2 expected_result); 

    (let or_fn = 
      "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 pred2) true false)));
       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= pred1 true) true (= pred2 true) true false)));
       (or (= 1 1) (= 1 0))" 
    and expected_result = "true" in
    or_fn, `Quick, simple_interp_test or_fn expected_result); 

    (let or_fn_2 = 
      "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 pred2) true false)));
       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= pred1 true) true (= pred2 true) true false)));
       (or (= 0 1) (= 1 1))" 
    and expected_result = "true" in
    or_fn_2, `Quick, simple_interp_test or_fn_2 expected_result); 

    (let or_fn_3 = 
      "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 pred2) true false)));
       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= pred1 true) true (= pred2 true) true false)));
       (or (= 1 1) (= 1 1))" 
    and expected_result = "true" in
    or_fn_3, `Quick, simple_interp_test or_fn_3 expected_result); 

    (let or_fn_4 = 
      "(def and (fn pred1 pred2 -> (cond (= pred1 false) false (= pred1 pred2) true false)));
       (def or (fn pred1 pred2 -> (cond (and pred1 pred2) true (= pred1 true) true (= pred2 true) true false)));
       (or (= 0 1) (= 0 1))" 
    and expected_result = "false" in
    or_fn_4, `Quick, simple_interp_test or_fn_4 expected_result); 

    (let recurse =
      "(def fib (fn x -> (cond (= x 0) 0 (= x 1) 1 (= x 2) 1 (+ (fib (- x 1)) (fib (- x 2))))));
      (fib 10)"
    and expected_result = "Integer 55" in
    recurse, `Quick, simple_interp_test recurse expected_result)
  ]

let interp_tests () =
  Alcotest.run "Interp" [ 
    "E2E", suite 
    ]

let () = interp_tests ()