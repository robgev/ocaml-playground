(* We are using raise (Exception_name "some_string") syntax to raise exception *)
(* Here we raise a built-in exception Invalid_argument *)
let rec take n l =
  match l with
    [] ->
    if n = 0
    then []
    else raise (Invalid_argument "take")
  | h::t ->
     if n < 0 then raise (Invalid_argument "take") else
       if n = 0 then [] else h :: take (n - 1) t

let rec drop n l =
  match l with
    [] ->
    if n = 0
    then []
    else raise (Invalid_argument "take")
  | h::t ->
     if n < 0 then raise (Invalid_argument "take") else
       if n = 0 then l else drop (n - 1) t

(* We can define our own exceptions like this *)
exception Problem
(* It can also carry info with it using the of construct, like this one carries int *)
exception NotPrime of int            

let f x = if x < 0 then raise Problem else 100 / x
(* f 5 -> 20, f (-1) -> Exception *)

(* Exceptions can also be handled using try ... with *)
let safe_divide x y =
  try x / y with
    Division_by_zero -> 0
                                                   
(* Another example *)
let rec last l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | _::t -> last t

let rec smallest_inner l current found =
  match l with
    [] -> if found then current else raise Not_found
  | h::t -> if h > 0 && h < current
            then smallest_inner t h true
            else smallest_inner t current found
                 
let rec smallest l =
  smallest_inner l max_int false
  
let rec smallest_or_zero l =
  try smallest l with
    Not_found -> 0

exception Complex_root

let rec sqrt_inner n x =
  if x * x > n then x - 1 else sqrt_inner n (x + 1)  
            
let sqrt n =
  if n < 0
  then raise Complex_root
  else sqrt_inner n 1
            
let sqrt_or_zero n =
  try sqrt n with
    Complex_root -> 0
