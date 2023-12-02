open Unix
open Printf
open Extensions

(* escape code in hex: \x1b, in decimal: \x1b*)
(* Escape codes can be found here: https://www2.ccs.neu.edu/research/gpc/VonaUtils/vona/terminal/vtansi.htm *)

let iprint_escape_code esc = printf "%s" esc
let clear_line () = iprint_escape_code "\r\x1b[K"
let clear_screen_from_cursor_down () = iprint_escape_code "\r\x1b[0J"
let scroll_window_down_one_line () = iprint_escape_code "\x1b[M"

let clear_from_cursor () =
  clear_screen_from_cursor_down ();
  scroll_window_down_one_line ()

let move_cursor_right () = iprint_escape_code "\x1b[C"
let move_cursor_left () = iprint_escape_code "\x1b[D"
let ( << ) t h = List.rev (h :: List.rev t)
let set_cursor_position (row, col) = printf "\x1b[%d;%dH" row col

let iprint_format msg str =
  clear_line ();
  printf msg str

let iprint str =
  clear_line ();
  printf "%s" (String.concat "" str)

let read_bytes () =
  let char_buffer = Bytes.create 3 in
  let number_read = input Stdlib.stdin char_buffer 0 3 in
  let char_list = char_buffer |> Bytes.to_seq |> List.of_seq in
  (char_list, number_read)

let rec get_cursor_position () =
  iprint_escape_code "\x1b[6n";
  flush Stdlib.stdout;
  let rec cursor_position_aux acc =
    let char_list, num_bytes = read_bytes () in
    match char_list with
    | '\x1b' :: '[' :: t -> cursor_position_aux (acc ^ String.of_char_list t)
    | c -> (
        if num_bytes = 3 then cursor_position_aux (acc ^ String.of_char_list c)
        else
          let sequence = acc ^ String.of_char_list c in
          try
            let row, col = Scanf.sscanf sequence "%d;%dR" (fun x y -> (x, y)) in
            (row, col)
          with _ -> get_cursor_position ())
  in
  cursor_position_aux ""

let set_raw_mode () =
  (* need to handle everything myself if I go for raw mode. *)
  let termios = tcgetattr stdin in
  let new_termios = { termios with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSAFLUSH new_termios

let split_by_newline line =
  let lines = String.split_on_char '\n' line in
  List.mapi
    (fun i cmd -> if i <> List.length lines - 1 then cmd ^ "\n" else cmd)
    lines

let handle_insert_at row col char lines =
  let old_line = List.nth lines row |> String.to_list in
  let new_line =
    (if List.length old_line = col - 1 then old_line << char
     else
       snd
         (List.fold_left
            (fun acc m ->
              let i = fst acc and line = snd acc in
              if i = col - 1 then (i + 1, line << char << m)
              else (i + 1, line << m))
            (0, []) old_line))
    |> String.of_char_list
  in
  List.mapi (fun i x -> if i = row then new_line else x) lines

let handle_delete_at row col lines =
  let new_line =
    List.nth lines row |> String.to_list
    |> List.filteri (fun curr_col _ ->
           if curr_col = col - 1 then false else true)
    |> String.of_char_list
  in
  List.mapi (fun i line -> if i = row then new_line else line) lines

let get_from_history cmd_history history_index =
  if history_index = -1 then [ "" ]
  else
    match List.nth_opt cmd_history history_index with
    | Some line -> split_by_newline line
    | None -> [ "" ]

let get_next_col t erow col lines =
  match t with
  | `FORWARD ->
      if
        erow = List.length lines - 1
        && col = String.length (List.nth lines erow)
      then col
      else if col = String.length (List.nth lines erow) then 1
      else col + 1
  | `BACK ->
      if erow = 0 && col <= 1 then col
      else if col = 1 then String.length (List.nth lines (erow - 1))
      else col - 1

let get_next_row t erow tty_row col lines =
  match t with
  | `FORWARD ->
      if erow = List.length lines - 1 then (erow, tty_row)
      else (erow + 1, tty_row + 1)
  | `BACK ->
      if erow > 0 && col = 1 then (erow - 1, tty_row - 1) else (erow, tty_row)

let get_next_index t history_index cmd_history =
  match t with
  | `UP ->
      if List.length cmd_history = history_index + 1 then history_index
      else history_index + 1
  | `DOWN -> if history_index < 0 then -1 else history_index - 1

let get_next_cursor_position t row col =
  match t with
  | `INSERT -> (row, col + 1)
  | `DELETE -> if col < 0 then (row, 0) else (row, col)
  | `NEW_LINE -> (row + 1, 0)

let rec next_char lines history_index cmd_history editing_row =
  flush Stdlib.stdout;
  let row, col = get_cursor_position () in
  let char_list, read_chars = read_bytes () in
  match char_list with
  (* arrow keys *)
  | '\x1b' :: '[' :: 'A' :: _ ->
      (* UP *)
      let next_history_index = get_next_index `UP history_index cmd_history in
      let prev_lines = get_from_history cmd_history next_history_index in
      set_cursor_position (row - (List.length prev_lines - 1), 1);
      iprint prev_lines;
      next_char prev_lines next_history_index cmd_history editing_row
  | '\x1b' :: '[' :: 'B' :: _ ->
      (* DOWN *)
      let next_history_index = get_next_index `DOWN history_index cmd_history in
      let next_lines = get_from_history cmd_history next_history_index in
      set_cursor_position (row - (List.length next_lines - 1), 1);
      iprint next_lines;
      next_char next_lines next_history_index cmd_history editing_row
  | '\x1b' :: '[' :: 'C' :: _ ->
      if col - 1 < String.length (List.nth lines editing_row) then
        move_cursor_right ();
      next_char lines history_index cmd_history editing_row
  | '\x1b' :: '[' :: 'D' :: _ ->
      move_cursor_left ();
      next_char lines history_index cmd_history editing_row
  (*  *)
  | '\x1b' :: t -> (
      match t with
      | '\010' :: _ when read_chars = 2 -> lines (* alt + enter, submit *)
      | _ ->
          next_char lines history_index cmd_history editing_row
          (* uncaught escape seq, do nothing. *)
          (* | ' ' ::  _ -> set_cursor_position (row - (List.length lines - 1), 1); remove_line (); next_char [""] history_index cmd_history editing_row *)
      )
  (* new line *)
  | '\010' :: _ ->
      let new_lines = handle_insert_at editing_row col '\n' lines @ [ "" ] in
      set_cursor_position (row - (List.length lines - 1), 1);
      clear_from_cursor ();
      iprint new_lines;
      set_cursor_position (get_next_cursor_position `NEW_LINE row col);
      next_char new_lines history_index cmd_history (editing_row + 1)
  (* delete *)
  | '\127' :: _ ->
      let col_to_delete = get_next_col `BACK editing_row col lines in
      let erow, row_in_tty = get_next_row `BACK editing_row row col lines in
      let new_lines = handle_delete_at erow col_to_delete lines in
      (* Remove the trailing empty line. *)
      let new_lines =
        if col_to_delete >= col then List.rev (List.tl (List.rev new_lines))
        else new_lines
      in
      set_cursor_position (row - (List.length new_lines - 1), 1);
      clear_from_cursor ();
      iprint new_lines;
      (* iprint_format "lenght of new_lines: %d" (List.length new_lines); *)
      set_cursor_position
        (get_next_cursor_position `DELETE row_in_tty col_to_delete);
      next_char new_lines history_index cmd_history erow
  (* all other chars, put if single char otherwise do nothing. *)
  | c :: _ when read_chars = 1 ->
      let new_lines = handle_insert_at editing_row col c lines in
      set_cursor_position (row - (List.length lines - 1), 1);
      clear_from_cursor ();
      iprint new_lines;
      set_cursor_position (get_next_cursor_position `INSERT row col);
      next_char new_lines history_index cmd_history editing_row
  | _ -> next_char lines history_index cmd_history editing_row

let next_cmd cmd_history =
  String.concat "" (next_char [ "" ] (-1) cmd_history 0)
