open Unix
open Printf
open Extensions

(* escape code in hex: \x1b, in decimal: \027*)

let escape_code = '\027'
and new_line = '\010'
and delete = '\127'

let iprint_escape_code esc = printf esc
let clear_line () =  iprint_escape_code "\r\x1b[K"
let move_cursor_right () = iprint_escape_code "\x1b[C"
let move_cursor_left () = iprint_escape_code "\x1b[D"

let iprint_format msg str = 
  (* clear_line (); *)
  printf msg str
  
let iprint str =
  (* clear_line (); *)
  printf "%s" str  

let read_bytes () =
  let char_buffer = Bytes.create 3 in
  let number_read = input Stdlib.stdin char_buffer 0 3 in
  let char_list = char_buffer |> Bytes.to_seq |> List.of_seq in
  (char_list, number_read)

let rec get_cursor_position ?(acc="") () = 
  iprint_escape_code "\x1b[6n";
  flush Stdlib.stdout;
  let (char_list, num_bytes) = read_bytes () in
  match char_list with 
  | '\027' :: '[' :: t -> 
    get_cursor_position ~acc:(acc ^ (String.of_char_list t)) ()
  | c -> 
    if num_bytes = 3 then 
      get_cursor_position ~acc:(acc ^ (String.of_char_list c)) ()
    else 
      let sequence = acc ^ (String.of_char_list c)  in
      (* let (row, col) = Scanf.sscanf sequence "%d;%d" (fun x y -> (x, y)) in *)
      let pos_of_r = String.index sequence 'R' in
      let pos = (String.split_on_char ';' (String.sub sequence 0 pos_of_r)) in
      let row = List.hd pos and col = (List.hd (List.tl pos)) in
      (row, col)
      
let set_raw_mode () =
  (* need to handle everything myself if I go for raw mode. *)
  let termios = tcgetattr stdin in
  let new_termios = { termios with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSAFLUSH new_termios

let handle_insert_at row col char line = 
  let char_list = String.to_seq line |> List.of_seq in
  let rec insert_at_aux acc curr_row curr_col line = 
    match line with
    | [] -> char :: acc
    | h :: t -> 
      (if curr_row = row && curr_col = col then
        (List.rev t) @ [h; char] @ acc
      else
        insert_at_aux (h :: acc) curr_row (curr_col + 1) t)
  in
  String.of_char_list (List.rev (insert_at_aux [] 0 0 char_list))
        
        
let handle_delete_at row col line =
  let char_list = String.to_seq line |> List.of_seq in
  let rec delete_at_aux acc curr_row curr_col line = 
    match line with
    | [] -> acc
    | _ :: [] -> acc
    | h :: t -> 
      (if curr_row = row && curr_col = col then
        let () = iprint "found char to remove" in
         (List.rev t) @ acc
      else
        delete_at_aux (h :: acc) curr_row (curr_col + 1) t)
  in
  String.of_char_list (List.rev (delete_at_aux [] 0 0 char_list))

  
let _handle_backspace line = 
    let line_len = String.length line in
    let new_line = if (line_len - 1 < 0) then line else String.sub line 0 (line_len - 1) in 
    clear_line (); 
    iprint_format "%s" new_line;
    new_line

let rec read_line2 ?(line="") ?(row=0) ?(col=0) () =
  flush Stdlib.stdout;
  (* let (_, col1) = get_cursor_position () in *)
  let (char_list, read_chars) = read_bytes () in
  match char_list with
  (* arrow keys *)
  | '\027' :: '[' :: 'A' :: _ -> iprint "arrow up!"; read_line2 ~line ~row ~col ()
  | '\027' :: '[' :: 'B' :: _ -> iprint "arrow down!"; read_line2 ~line ~row ~col ()
  | '\027' :: '[' :: 'C' :: _ ->  move_cursor_right (); read_line2 ~line ~row ~col:(col + 1) ()
  | '\027' :: '[' :: 'D' :: _ ->  move_cursor_left (); let new_col = if col = 0 then 0 else col - 1 in read_line2 ~line ~row ~col:new_col ()

  (*  *)
  | '\027' :: t -> 
    (match t with 
    | '\010' :: _ when read_chars = 2 -> line (* alt + enter, submit *)
    | _ -> read_line2 ~line ~row ~col ()) (* uncaught escape seq, do nothing. *)

  (* new line *)
  | '\010' :: _ -> let line = (line ^ "\n") in iprint "\n"; read_line2 ~line ~row ~col ()

  (* delete *)
  | '\127' :: _ -> 
    let _ = get_cursor_position () in
    let new_line = handle_delete_at row col line in 
    let new_col = if col = 0 then 0 else col - 1 in 
    iprint_format "%s" new_line;
    read_line2 ~line:new_line ~row ~col:new_col ()

  (* all other chars, put if single char otherwise do nothing. *)
  | c :: _ when read_chars = 1 -> 
    let new_line = handle_insert_at row col c line in
    iprint_format "%s" new_line;
    read_line2 ~line:new_line ~row ~col:(col + 1) ()

  | _ -> read_line2 ~line ~row ~col ()

(*
let rec read_line2 ?(line="") () =
  flush Stdlib.stdout;
  (* let char_buffer = Bytes.create 3 in
  let _ = input Stdlib.stdin char_buffer 0 2 in *)
  let c = input_char Stdlib.stdin in
  match c with
  | '[' -> iprint "escp pressed"; read_line2 ~line ()
  | '\n' -> 
    begin
      match input_char Stdlib.stdin with 
      | _ -> line
      (* | _ -> output_char Stdlib.stdout c; let line = (line ^ (String.make 1 c)) in read_line2 ~line ()  *)
    end
  | '\127' -> let new_line = handle_backspace line in read_line2 ~line:new_line ()
  | '\027' ->  (* Escape key was pressed, read additional characters for arrow keys *)
      begin
        match input_char Stdlib.stdin with
        | '[' ->  (* '[' follows the Escape key for arrow keys *)
          begin
            match input_char Stdlib.stdin with
            | 'A' -> Printf.printf "\rUp arrow key pressed\n"; read_line2 ~line ()
            | 'B' -> Printf.printf "\rDown arrow key pressed\n"; read_line2 ~line ()
            | 'C' -> move_cursor_right (); read_line2 ~line ()
            | 'D' -> move_cursor_left (); read_line2 ~line ()
            | _ -> read_line2 ~line ()
          end
        | _ -> read_line2 ~line ()
      end
  | _ -> output_char Stdlib.stdout c; let line = (line ^ (String.make 1 c)) in read_line2 ~line ()  *)
