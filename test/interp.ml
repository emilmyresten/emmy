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
    check string "test context" expected_context (Interpreter.string_of_context ctx); 
  check string "test return value" expected_eval (Interpreter.string_of_val eval)

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

    (let scoping = 
      "(def alpha_converted (fn x -> (fn x -> (fn x -> x))));
      (((alpha_converted 1) 2) 3)"
    and expected_result = "Integer 3" in 
    scoping, `Quick, simple_interp_test scoping expected_result); 

    (let multi_arity = "(def multi_arity (fn x y z -> z))" 
    and expected_result = "Unit" in 
    multi_arity, `Quick, simple_interp_test multi_arity expected_result); 

    (let multi_arity = 
      "(def multi_arity (fn x y z -> ((fn x y -> x) x y))); 
      (multi_arity 1 2 3)" 
    and expected_result = "Integer 1" in 
    multi_arity, `Quick, simple_interp_test multi_arity expected_result); 
  ]

let lexer_tests () =
  Alcotest.run "Interp" [ 
    "E2E", suite 
    ]

let () = lexer_tests ()