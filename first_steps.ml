let x = 200 in (let y = x * x in x + y)

(* This is how you define a function *)
let cube x = x * x * x
let neg x = x < 0
let isVowel1 c =
  c = 'a' || c = 'e' || c  = 'i' || c = 'o' || c = 'u'
let isSumTen a b =
  a + b = 10
(* To define a recursive function, you need to specify the keyword rec *)
let rec factorial1 x =
  if x = 1 then 1 else x * factorial1 (x - 1)
let rec gcd1 a b =
  if b = 0 then a else gcd1 b (a mod b)
(* Nested  if else construction: Not the best example though :) *)
let rec power1 x n =
  if n = 0 then 1 else
    if n = 1 then x else
      x * power1 x (n - 1)
let isConsonant c = not (isVowel1 c)
(* Ladies and gentlemen, please welcome your majesty pattern matching :D *) 
let rec factorial a =
  match a with
    1 -> 1
  | _ -> a * factorial (a - 1)
let isVowel c =
  match c with
    'a' | 'e' | 'i' | 'o' | 'u' -> true
    | _ -> false

let rec gcd a b =
  match b with
    0 -> a
  | _ -> gcd b (a mod b)

let not a =
  match a with
    false -> true
  | true -> false

let rec sum a =
  match a with
    1 -> 1
  | _ -> a + sum (a - 1)

let rec power x n =
  match n with
    0 -> 1
  | 1 -> x
  | _ -> x * power x (n - 1)

(* Just a nested match example. The result will be 5 *)
let a =
  match 1 + 1 with
    2 -> ( match 2 + 2 with
             3 -> 4
           | 4 -> 5
           | _ -> 0
         )
  | _ -> 01
         

let isLower c =
  match c with
    'a' .. 'z' -> true
  | _ -> false

(* This one is pretty much awesome. Using con(::) to deconstruct the list. *)
let rec length l =
  match l with
    [] -> 0
  | h::t -> length t + 1

let rec add_list l =
  match l with
    [] -> 0
  | h::t -> h + add_list t

(* More advanced example, needs less memory *)
let rec length_inner l n =
  match l with
    [] -> n
  | h::t -> length_inner t (n + 1)

let length_adv l = length_inner l 0

(* A little tricky one *)
let rec odd_elements l =
  match l with
    [] -> []
  | [h] -> [h]
  | h::_::t -> h :: odd_elements t
                                 
(* Better version *)
let rec odd_elements_refined l =
  match l with
    h::_::t -> h :: odd_elements_refined t
  | _ -> l

(* Coooool *)
let rec append a b =
  match a with
    [] -> b
  | h::t -> h :: append t b
  
let rec reverse a =
  match a with
    [] -> []
  | h::t -> reverse t @ [h]

let rec reverse_inner l a =
  match l with
    [] -> a
  | h::t -> reverse_inner t (h :: a)

let reverse_t l =
  reverse_inner l []
                          
(* Simple functions to take/drop first n elements of the list *)
let rec take n l =
  if n = 0 then [] else
    match l with
      h::t -> h :: take (n - 1) l

let rec drop n l =
  if n = 0 then l else
    match l with
      h::t -> drop (n - 1) l

(* Counting the number of true elements - looks awesome. Second version is tail recurisve one *)                   
let rec count_true l =
  match l with
    [] -> 0
  | true::t -> 1 + count_true t
  | _::t -> count_true t
                       
let rec count_true_inner l n =
  match l with
    [] -> n
  | true::t -> count_true_inner t (n + 1)
  | _::t -> count_true_inner t n

let count_true_tailrec l =
  count_true_inner l 0

(* Simple palindrome ones *)
let build_palindrome l =
  l @ reverse l

let check_palindrome l =
  l = reverse l

(* Drop last, 2 versions *)
let rec drop_last l =
  match l with
    [] -> []
  | [_] -> []
  | h::t -> h :: drop_last t

let rec drop_last_inner l acc =
  match l with
    [] -> []
  | [_] -> reverse_t acc
  | h::t -> drop_last_inner t (h :: acc)

let rec drop_last_tailrec l =
  drop_last_inner l []
