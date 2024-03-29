(* ref is a func of 'a -> 'a ref *)
let x = ref 0

(* ! is a func of 'a ref -> 'a *)
let v = !x;;

(* update content of a ref using := *)
(* type of := is 'a ref -> 'a -> unit *)
x := 15

(* Now some utilities *)
let swap a b = 
  let tmp = !a in
      a := !b;
      b := tmp

(* Ocaml lets us omit else if it's just a unit *)
let omitted_else r = 
  if r = 0 then x := 50

(* So if we do have else we need begin/end to avoid
   ambiguity
*)
let side_effects a b c =
  if 5 > 2 then
    begin
      a := !a + 1;
      b := !b - 1
    end
  else 
    c := !c + 10

(* For loop! *)
let print_up_to n =
  for i = 1 to n do
    print_int i;
    print_char '\t'
  done 

(* We could implement our own for loop for a while now 
   we already kind of did that with count in io.ml *)
let rec forloop fn from _to =
  if from <= _to then 
    begin 
        fn from;
        forloop fn (from + 1) _to
    end

(* While loop! *)
let continuous_input () = 
  let x = ref "" in 
    print_string "Try to find the hidden word to get out";
    print_newline ();
    while !x <> "alohomora" do 
      x := read_line ()
    done;
    print_string "Congrats, you found the hidden word!";
    print_newline ()

let to_uppercase c = 
  let char_code = int_of_char c in 
    if char_code >= 97 && char_code <= 122
    then char_of_int (char_code - 32)
    else c

let to_lowercase c = 
  let char_code = int_of_char c in 
    if char_code >= 65 && char_code <= 90
    then char_of_int (char_code + 32)
    else c
