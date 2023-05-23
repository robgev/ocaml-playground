let print_dict_entry (i, s) =
  print_int i; print_newline ();
  print_string s; print_newline()

let rec print_dict d =
  match d with
    [] -> ()
  | h::t -> print_dict_entry h; print_dict t

(*
  Write a general iterator to apply f
*)
let rec for_each f l = 
  match l with
    [] -> ()
  | h::t -> f h; for_each f t

let print_dict_better =
  for_each print_dict_entry

(*
  Handle wrong inputs with exceptions
*)
let rec read_dict () =
  try
    let i = read_int () in
      if i = 0 then [] else
        let s = read_line () in
          (i, s) :: read_dict ()
  with
    Failure "int_of_string" ->
      print_string "Invalid input: Not Integer. Try again.";
      print_newline ();
      read_dict ()

let write_entry ch (k, v) =
  output_string ch (string_of_int k);
  output_char ch '\n';
  output_string ch v;
  output_char ch '\n'

let write_dictionary ch =
  for_each (write_entry ch)

(*
  Use open_out and close_out to open and close
  a write channel
*)
let save_dict_file filename dict = 
  let ch = open_out filename in
    write_dictionary ch dict;
    close_out ch

(*
  Combine user input to writing to a file
*)
let input_dict_to_file filename = 
  save_dict_file filename (read_dict ())

(*
  Same functions for reading from the file
*)
let read_entry ch =
  let number = input_line ch in
    let name = input_line ch in 
      (int_of_string number, name)

let rec read_dictionary ch = 
  try
    let e = read_entry ch in
      e :: read_dictionary ch
  with
    End_of_file -> []

let open_dict_file filename =
  let ch = open_in filename in
    let dict = read_dictionary ch in
      close_in ch;
      dict

(* Exercises *)
let rec print_int_list_aux l =
  match l with
    [] -> print_string ""; 
  | [h] -> print_int h; 
  | h::t -> print_int h; print_string "; ";  print_int_list_aux t

let print_int_list l = 
  print_string "[";
  print_int_list_aux l;
  print_string "]"

let rec input_triple () = 
  try
    print_string "Input three integers separated with newline";
    print_newline ();
    let int1 = read_int () in
      let int2 = read_int () in 
        let int3 = read_int () in
          (int1, int2, int3)
  with
    Failure "int_of_string" -> 
      print_string "Input is not integer. Try again.";
      print_newline ();
      input_triple () 

let rec read_dict_improved () =
  try
    let i = read_line () in
      if i = "end" then [] else
        let num = int_of_string i in
          let s = read_line () in
            (num, s) :: read_dict_improved ()
  with
    Failure "int_of_string" ->
      print_string "Invalid input: Not Integer. Try again.";
      print_newline ();
      read_dict_improved ()

let rec read_numbered n = 
  if n = 0 then [] else
    try 
      let i = read_line () in 
        let s = read_line () in 
          (i, s) :: read_numbered (n - 1)
  with 
    Failure "int_of_string" ->
      print_string "Invalid input: Not Integer. Try again.";
      print_newline ();
      print_string "Please enter integer and name again.";
      print_newline ();
      read_numbered n

exception BadNumber

let rec read_dict_improved () =
  print_string "Enter the number of entries";
  print_newline ();
  try 
    let n = read_int () in
      if n < 0 then raise BadNumber else read_numbered n
  with 
    Failure "int_of_string" ->
      print_string "Input is not a number. Please try again.";
      print_newline ();
      read_dict_improved ()
  | BadNumber -> 
      print_string "Number of entries cannot be negative. Please try again.";
      print_newline ();
      read_dict_improved ()

(*
  Can't wait to learn modules :) 
  To print a multiplication table
  up to some number we need a map 
  and upto functions which we can use
  with for_each we have 
*)
let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t

let rec count t acc f =
  if f < t then acc
  else count t (f :: acc) (f-1)

let upto = count 1 []

let output_line ch mult n = 
  for_each 
  (fun i -> 
    output_string ch (string_of_int i);
    output_char ch '\t'
  )
  (map (( * ) mult) (upto n)) 

let write_table_channel ch n = 
  for_each 
  (fun x -> 
    output_line ch x n;
    output_char ch '\n'
  )
  (upto n)

let table filename n =
  if n < 0 then raise BadNumber
  else 
    let ch = open_out filename in 
      write_table_channel ch n;
      close_out ch 
(* END *)

let rec number_of_file_lines ch acc = 
  try
    let _ = input_line ch in 
      number_of_file_lines ch acc+1
  with
    End_of_file -> acc

let lines_in_file filename = 
  let ch = open_in filename in
    let nums = number_of_file_lines ch 0 in
      close_in ch;
      nums

exception Copy_Failed

let rec copy_channel from_ch to_ch =
  try
    output_string to_ch (input_line from_ch);
    output_char to_ch '\n';
    copy_channel from_ch to_ch
  with
    End_of_file -> ()

let rec copy target destination = 
  try 
    let from_ch = open_in target in
      let to_ch = open_out destination in 
        copy_channel from_ch to_ch;
        close_in from_ch;
        close_out to_ch;
  with
    _ -> raise Copy_Failed
