open Unix
open Printf
open Extensions

(* escape code in hex: \x1b, in decimal: \027*)
(* Escape codes can be found here: https://www2.ccs.neu.edu/research/gpc/VonaUtils/vona/terminal/vtansi.htm *)

let escape_code = '\027'
and new_line = '\010'
and delete = '\127'

let iprint_escape_code esc = printf esc
let clear_line () =  iprint_escape_code "\r\x1b[K"
let move_cursor_right () = iprint_escape_code "\x1b[C"
let move_cursor_left () = iprint_escape_code "\x1b[D"


let set_cursor_position (row, col) = printf "\x1b[%d;%dH" row col

let iprint_format msg str = 
  clear_line ();
  printf msg str
  
let iprint str =
  clear_line ();
  printf "%s" str  

let read_bytes () =
  let char_buffer = Bytes.create 3 in
  let number_read = input Stdlib.stdin char_buffer 0 3 in
  let char_list = char_buffer |> Bytes.to_seq |> List.of_seq in
  (char_list, number_read)

let get_cursor_position () = 
  iprint_escape_code "\x1b[6n";
  flush Stdlib.stdout;
  let rec cursor_position_aux acc = 
    let (char_list, num_bytes) = read_bytes () in
      match char_list with 
      | '\027' :: '[' :: t -> 
        cursor_position_aux (acc ^ (String.of_char_list t))
      | c -> 
        if num_bytes = 3 then 
          cursor_position_aux (acc ^ (String.of_char_list c))
        else 
          let sequence = acc ^ (String.of_char_list c)  in
          let (row, col) = Scanf.sscanf sequence "%d;%dR" (fun x y -> (x, y)) in
          (* let pos_of_r = String.index sequence 'R' in
          let pos = (String.split_on_char ';' (String.sub sequence 0 pos_of_r)) in
          let row = List.hd pos and col = (List.hd (List.tl pos)) in *)
          (* iprint_format "found %d" col;
          flush Stdlib.stdout; *)
          (row, col)
    in cursor_position_aux ""
      
      
let set_raw_mode () =
  (* need to handle everything myself if I go for raw mode. *)
  let termios = tcgetattr stdin in
  let new_termios = { termios with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSAFLUSH new_termios

let handle_insert_at col char line = 
  let char_list = String.to_seq line |> List.of_seq in
  let rec insert_at_aux acc curr_row curr_col line = 
    match line with
    | [] -> char :: acc
    | h :: t -> 
      (if curr_col = col - 1 then
        (List.rev t) @ [h; char] @ acc
      else
        insert_at_aux (h :: acc) curr_row (curr_col + 1) t)
  in
  String.of_char_list (List.rev (insert_at_aux [] 0 0 char_list))
        
        
let handle_delete_at col line =
  let char_list = String.to_seq line |> List.of_seq in
  let rec delete_at_aux acc curr_row curr_col line = 
    match line with
    | [] -> acc
    | _ :: [] -> acc
    | h :: t -> 
      (if curr_col = col - 1 then
         (List.rev t) @ acc
      else
        delete_at_aux (h :: acc) curr_row (curr_col + 1) t)
  in
  String.of_char_list (List.rev (delete_at_aux [] 0 0 char_list))

let get_from_history cmd_history history_index =
  if history_index = -1 then
    "" 
  else 
    match List.nth_opt cmd_history history_index with
    | Some (cmd) -> cmd
    | None -> ""

let get_next_index t history_index cmd_history = 
  match t with
  | `UP -> if List.length cmd_history = history_index + 1 then history_index else history_index + 1
  | `DOWN -> if history_index < 0 then -1 else history_index - 1

let get_next_cursor_position t row col =
  match t with
  | `INSERT -> (row, col + 1)
  | `DELETE -> if col < 0 then (row, 0) else (row, col - 1)
  
let _handle_backspace line = 
    let line_len = String.length line in
    let new_line = if (line_len - 1 < 0) then line else String.sub line 0 (line_len - 1) in 
    clear_line (); 
    iprint_format "%s" new_line;
    new_line

let rec next_cmd ?(line="") ?(history_index= -1) cmd_history =
  flush Stdlib.stdout;
  let (row, col) = get_cursor_position () in 
  let (char_list, read_chars) = read_bytes () in
  match char_list with
  (* arrow keys *)
  | '\027' :: '[' :: 'A' :: _ -> (* UP *)
    let next_history_index = get_next_index `UP history_index cmd_history in
    let prev_line = get_from_history cmd_history next_history_index in
    iprint prev_line;
    next_cmd ~line:prev_line ~history_index:next_history_index cmd_history
  | '\027' :: '[' :: 'B' :: _ -> (* DOWN *)
    let next_history_index = get_next_index `DOWN history_index cmd_history in
    let next_line = get_from_history cmd_history next_history_index in
    iprint next_line;
    next_cmd ~line ~history_index:next_history_index cmd_history
  | '\027' :: '[' :: 'C' :: _ -> if col - 1 < String.length line then move_cursor_right (); next_cmd ~line ~history_index cmd_history
  | '\027' :: '[' :: 'D' :: _ -> move_cursor_left (); next_cmd ~line ~history_index cmd_history

  (*  *)
  | '\027' :: t -> 
    (match t with 
    | '\010' :: _ when read_chars = 2 -> line (* alt + enter, submit *)
    | _ -> next_cmd ~line ~history_index cmd_history) (* uncaught escape seq, do nothing. *)

  (* new line *)
  (* | '\010' :: _ -> let line = (line ^ "\n") in iprint "\n"; next_cmd ~line ~history_index cmd_history *)

  (* delete *)
  | '\127' :: _ -> 
    let delete_col = if col = 1 then 1 else col - 1 in 
    let new_line = handle_delete_at delete_col line in 
    iprint_format "%s" new_line;
    set_cursor_position (get_next_cursor_position `DELETE row col);
    next_cmd ~line:new_line ~history_index cmd_history

  (* all other chars, put if single char otherwise do nothing. *)
  | c :: _ when read_chars = 1 
          && (Char.is_letter c || Char.is_digit c || Char.is_symbol c || Char.is_whitespace c)
          && c != '\n' -> 
    let new_line = handle_insert_at col c line in
    iprint_format "%s" new_line;
    set_cursor_position (get_next_cursor_position `INSERT row col);
    next_cmd ~line:new_line ~history_index cmd_history

  | _ -> next_cmd ~line ~history_index cmd_history