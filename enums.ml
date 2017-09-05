(* We define types using type keyword. We then give constructors to it *)
type color = Red | Green | Blue | Yellow;;
(* Now we can use this type *)
let col = Blue;;
let cols = [Red; Red; Green];;
let colpair = ('R', Red);;
(* We also can associate primitive types with the values of enum *)
type colorRGB =
  Red
| Green
| Blue
| Yellow
| RGB of int * int * int;;

let colsRGB = [Red; Red; Green; RGB (150, 0, 255)];;

(* Also we can use pattern matching *)
let components c =
  match c with
    Red -> (255, 0, 0)
  | Green -> (0, 255, 0)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b);;

(* You can have type variable like 'a for polymorphic types *)
type 'a option = None | Some of 'a;;
(* Examples using this type *)
let nothing = None;;
let number = Some 50;;
let numbers = [Some 12; None; None; Some 2];;
let word = Some ['o'; 'c'; 'a'; 'm'; 'l'];;

(* We can use our defined types for functions instead of throwing exceptions *)
let rec lookup x l =
  match l with
    [] -> None (* Instead of raise Not_found *)
  | (k, v) :: t -> if x = k then Some v else lookup x t;;

(* We can define recursive types. Helps create lists like OCaml's [] one *)
type 'a sequence = Nil | Cons of 'a * 'a sequence;;
(* Now [] is None and any non-empty list is smth like *)
let axe = Cons ('a', Cons ('x', Cons ('e', Nil)));;
let double_custom = Cons (Red, Cons (RGB (20, 20, 20), Nil));;

(* We can build same functions we had on built-in OCaml lists on our 'a sequence type *)
let rec length s =
  match s with
    Nil -> 0
  | Cons (_, t) -> 1 + length t;;

let rec append a b =
  match a with
    Nil -> b
  | Cons (h, t) -> Cons(h, append t b);;

(* Recursive definition of mathematical expression type *)
type expr =
  Num of int
| Add of expr * expr
| Subtract of expr * expr
| Multiply of expr * expr
| Divide of expr * expr;;

(* Now 1 + 2 x 3 is Add(Num 1, Multiply(Num 2, Num 3)) *)
(* Lets also write a function to evaluate the expression *)
let rec evaluate e =
  match e with
    Num n -> n
  | Add (e, e1) -> evaluate e + evaluate e1
  | Subtract (e, e1) -> evaluate e - evaluate e1
  | Multiply (e, e1) -> evaluate e * evaluate e1
  | Divide (e, e1) -> evaluate e / evaluate e1;;

(* Type for a rectangle *)
type rect =
  Rectangle of int*int
| Square of int;;

let area r =
  match r with
    Square s -> s * s
  | Rectangle (w, h) -> w * h;;

let rotate r =
  match r with
    Rectangle (w, h) ->
     if w > h then Rectangle (h, w) else r
  | _ -> r;;

(* We need our map and sort functions for this task *)
(* We will just copy paste them for now ;) *)
let rec length_inner l n =
  match l with
    [] -> n
  | h::t -> length_inner t (n + 1);;

let length l = length_inner l 0;;

let rec take n l =
  match l with
    [] ->
    if n = 0
    then []
    else raise (Invalid_argument "take")
  | h::t ->
     if n < 0 then raise (Invalid_argument "take") else
       if n = 0 then [] else h :: take (n - 1) t;;

let rec drop n l =
  match l with
    [] ->
    if n = 0
    then []
    else raise (Invalid_argument "take")
  | h::t ->
     if n < 0 then raise (Invalid_argument "take") else
       if n = 0 then l else drop (n - 1) t;;

let rec map f l =
  match l with
    [] -> []
  | h::t -> f h :: map f t;;

let rec merge_adv cmp a b =
  match a, b with
    [], l -> l
  | l, [] -> l
  | ha::ta, hb::tb ->
     if cmp ha hb
     then ha :: merge_adv cmp ta (hb::tb)
     else hb :: merge_adv cmp (ha::ta) tb;;
       
let rec sort cmp l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
     let half = length l / 2 in
     let left = take half l in
     let right = drop half l in
     merge_adv cmp (sort cmp left) (sort cmp right);;

let get_width r =
  match r with
    Rectangle (w, _) -> w
  | Square s -> s;;

let compare_widths r1 r2 =
  get_width r1 > get_width r2;;

let rec rect_sort rects =
  sort compare_widths (map rotate rects);;


let rec take_seq s n =
  match s with
    Nil -> if n = 0
           then Nil
           else raise (Invalid_argument "take_seq")
  | Cons (h, t) -> Cons(h, take_seq t (n - 1));;

let rec drop_seq s n =
  match s with
    Nil -> if n = 0
           then s
           else raise (Invalid_argument "drop_seq")
  | Cons (_, t) -> drop_seq t (n - 1);;

let rec map_seq f s =
  match s with
    Nil -> Nil
  | Cons (h, t) -> Cons (f h, map_seq f t);;

(* Extended expression type *)
type xexpr =
  Num of int
| Add of xexpr * xexpr
| Subtract of xexpr * xexpr
| Multiply of xexpr * xexpr
| Divide of xexpr * xexpr
| Power of xexpr * xexpr;;

(* Implement power function *)
let rec apply f n init_arg =
  if n = 0
  then init_arg
  else f (apply f (n - 1) init_arg);;

let power a b =
  apply (( * ) a) b 1;;

(* Now use both power and xexpr for a new evaluate func *)
let rec evaluate e =
  match e with
    Num n -> n
  | Add (e, e1) -> evaluate e + evaluate e1
  | Subtract (e, e1) -> evaluate e - evaluate e1
  | Multiply (e, e1) -> evaluate e * evaluate e1
  | Divide (e, e1) -> evaluate e / evaluate e1
  | Power (b, e) -> power (evaluate b) (evaluate e);;

