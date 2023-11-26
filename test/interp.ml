open Alcotest
open Interp


let run_program p = 
  let cmds = String.split_on_char ';' p in
  let rec eval_all_aux ctx cmds = 
    (match cmds with
    | [] -> failwith "empty program"
    | cmd :: [] -> 
      (let in_program = String.to_seq cmd |> List.of_seq in
      let (eval, ctx) = let ast = Parser.parse in_program in Interpreter.eval ast ctx in
      (eval, ctx))
    | cmd :: t -> 
        (let in_program = String.to_seq cmd |> List.of_seq in
        let (_, ctx) = let ast = Parser.parse in_program in Interpreter.eval ast ctx in (* we do not care about intermediate evals *)
        eval_all_aux ctx t)) in
  eval_all_aux [] cmds

let simple_interp_test ?(expected_context = "") in_program expected_result () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = expected_result in
  if (not (String.equal expected_context "")) then
    check string "test context" expected_context (Pprint.string_of_context ctx); 
  check string "test return value" expected_eval (Pprint.string_of_expr eval)

  (* `Quick marks it as a fast running test, can be ran every time.
     the alternative is `Slow, that can be suppressed. Unclear how to do that using dune. *)
let suite =
  [ 
    (let program = "(def x 1)" 
    and expected_result = "Unit" and expected_context = "x = Integer 1\n" in 
    program, `Quick, simple_interp_test ~expected_context program expected_result); 

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
  ]

let interp_tests () =
  Alcotest.run "Interp" [ 
    "E2E", suite 
    ]

let () = interp_tests ()