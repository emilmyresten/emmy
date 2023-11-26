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

let test_simple_def in_program expected_result () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = expected_result in
  let expected_ctx = "x = Integer 1\n" in
  check string "def stored pair in context" expected_ctx (Interpreter.string_of_context ctx); 
  check string "def returned Unit" expected_eval (Interpreter.string_of_val eval)

let test_simple_fn in_program expected_result () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = expected_result in
  let expected_ctx = "id = (Function x -> Identifier x)\n" in
  check string "def stored pair in context" expected_ctx (Interpreter.string_of_context ctx); 
  check string "def returned Unit" expected_eval (Interpreter.string_of_val eval)

let test_simple_fn_invoke in_program expected_result () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = expected_result in
  let expected_ctx = "id = (Function x -> Identifier x)\n" in
  check string "def stored id as function in context" expected_ctx (Interpreter.string_of_context ctx); 
  check string "evaluating function gave correct answer" expected_eval (Interpreter.string_of_val eval)

let test_alpha_conversion in_program expected_result () =
  let (eval, _) = run_program in_program in
  let expected_eval = expected_result in
  check string "Check that scoping occurs correctly with nested functions." expected_eval (Interpreter.string_of_val eval)

let test_multi_arity_fn_definition in_program expected_result () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = expected_result in
  let expected_ctx = "multi_arity = (Function x y z -> Identifier z)\n" in
  check string "def stored multi_arity as multi-arity function in context" expected_ctx (Interpreter.string_of_context ctx);
  check string "evaluating function gave correct answer" expected_eval (Interpreter.string_of_val eval)

let test_multi_arity_fn_invoke in_program expected_result () =
  let (eval, ctx) = run_program in_program in
  let expected_ctx = "multi_arity = (Function x y z -> (Function x y -> Identifier x) Identifier x Identifier y )\n" in
  let expected_eval = expected_result in
  check string "def stored multi_arity as multi-arity function in context" expected_ctx (Interpreter.string_of_context ctx);
  check string "evaluating function gave correct answer" expected_eval (Interpreter.string_of_val eval)

  (* `Quick marks it as a fast running test, can be ran every time.
     the alternative is `Slow, that can be suppressed. Unclear how to do that using dune. *)
let suite =
  [ 
    (let program = "(def x 1)" 
    and expected_result = "Unit" in 
    program, `Quick, test_simple_def program expected_result); 

    (let program = "(def id (fn x -> x))"
    and expected_result = "Unit" in 
    program, `Quick, test_simple_fn program expected_result); 

    (* let ';' symbolize separate expressions, new command in repl. *)
    (let program = 
      "(def id (fn x -> x)); 
      (id 1)" 
    and expected_result = "Integer 1" in
    program, `Quick, test_simple_fn_invoke program expected_result); 

    (let program = 
      "(def id (fn x -> x)); 
      ((id (fn y -> y)) 1)"
    and expected_result = "Integer 1" in 
    program, `Quick, test_simple_fn_invoke program expected_result); 

    (let program = 
      "(def alpha_converted (fn x -> (fn x -> (fn x -> x))));
      (((alpha_converted 1) 2) 3)"
    and expected_result = "Integer 3" in 
    program, `Quick, test_alpha_conversion program expected_result); 

    (let program = "(def multi_arity (fn x y z -> z))" 
    and expected_result = "Unit" in 
    program, `Quick, test_multi_arity_fn_definition program expected_result); 

    (let program = 
      "(def multi_arity (fn x y z -> ((fn x y -> x) x y))); 
      (multi_arity 1 2 3)" 
    and expected_result = "Integer 1" in 
    program, `Quick, test_multi_arity_fn_invoke program expected_result); 
  ]

let lexer_tests () =
  Alcotest.run "Interp" [ 
    "E2E", suite 
    ]

let () = lexer_tests ()