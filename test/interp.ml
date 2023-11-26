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

let test_simple_def in_program () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = "Unit" in
  let expected_ctx = "x = Integer 1\n" in
  check string "def stored pair in context" expected_ctx (Interpreter.string_of_context ctx); 
  check string "def returned Unit" expected_eval (Interpreter.string_of_val eval)

let test_simple_fn in_program () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = "Unit" in
  let expected_ctx = "id = Function x -> Parameter x\n" in
  check string "def stored pair in context" expected_ctx (Interpreter.string_of_context ctx); 
  check string "def returned Unit" expected_eval (Interpreter.string_of_val eval)

let test_simple_fn_invoke in_program () =
  let (eval, ctx) = run_program in_program in
  let expected_eval = "Integer 1" in
  let expected_ctx = "id = Function x -> Parameter x\n" in
  check string "def stored id as function in context" expected_ctx (Interpreter.string_of_context ctx); 
  check string "evaluating function gave correct answer" expected_eval (Interpreter.string_of_val eval)

  (* `Quick marks it as a fast running test, can be ran every time.
     the alternative is `Slow, that can be suppressed. Unclear how to do that using dune. *)
let suite =
  [ 
    (let program = "(def x 1)" in program, `Quick, test_simple_def program); 
    (let program = "(def id (fn x -> x))" in program, `Quick, test_simple_fn program); 
    (* let ';' symbolize separate expressions, new command in repl. *)
    (let program = "(def id (fn x -> x)); (id 1)" in program, `Quick, test_simple_fn_invoke program); 
    (let program = "(def id (fn x -> x)); ((id (fn y -> y)) 1)" in program, `Quick, test_simple_fn_invoke program); 
    (let program = "(def id (fn x y -> x)); (id 1)" in program, `Quick, test_simple_fn_invoke program); 
  ]

let lexer_tests () =
  Alcotest.run "Interp" [ 
    "E2E", suite 
    ]

let () = lexer_tests ()