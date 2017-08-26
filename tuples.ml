(* This is how to define tuples/pairs whatever :) *)
let p = (1, 4)
let p1 = (1, 'a')
let p2 = (1, [2; 3; 4])

(* Get elements using pattern matching *)
let fst p = match p with (x, _) -> x
let snd p = match p with (_, y) -> y

(* Shorter way to do the same *)
let first (x, _) = x
let second (_, y) = y

(* Create a dictionary using tuples *)
let dict = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)]

let rec lookup x l =
  match l with
    [] -> raise Not_found
  | (k, v)::t ->
     if x = k
     then v
     else lookup x t

let rec add k v d =
  match d with
    [] -> [(k, v)]
  | (k1, v1)::t ->
     if k = k1
     then (k, v) :: t
     else (k1, v1) :: add k v t

let rec remove x d =
  match d with
    [] -> []
  | (k, v)::t ->
     if k = x
     then t
     else (k, v) :: remove x t

(* Nice way to use a function inside a function *)
let rec key_exists k d =
  try
    let _ = lookup k d in true
  with
    Not_found -> false

(* Function to count the number of keys. Can use length of the list though :D *)
let rec number_of_keys_inner d n =
  match d with
    [] -> 0
  | (_, _)::t ->
     number_of_keys_inner t (n + 1)
                   

let rec number_of_keys d =
  number_of_keys_inner d 0

let rec replace k v d =
  match d with
    [] -> raise Not_found
  | (k1, v1)::t ->
     if k = k1
     then (k, v) :: t
     else (k1, v1) :: replace k v t

let rec build_dictionary l1 l2 =
  match l1, l2 with
    [], [] -> []
  | [], _ -> raise (Invalid_argument "build_dictionary")
  | _, [] -> raise (Invalid_argument "build_dictionary")
  | h1::t1, h2::t2 -> (h1, h2) :: build_dictionary t1 t2

(* This is very beautiful *)
let rec extract_dictionary d =
  match d with
    [] -> ([], [])
  | (k, v)::t ->
     let (k1, v1) = extract_dictionary t in
     (k :: k1, v :: v1)

let rec member n l =
  match l with
    [] -> false
  | h::t -> h = n || member n t

(* Make dictionary from any list of pairs *)
let rec dictionary_of_pairs_inner seen_keys l =
  match l with
    [] -> []
  | (k, v)::t ->
     if member k seen_keys
     then dictionary_of_pairs_inner seen_keys t
     else (k, v) :: dictionary_of_pairs_inner (k :: seen_keys) t

let rec dictionary_of_pairs l =
  dictionary_of_pairs_inner [] l
                                              
let rec union a b =
  match a with
    [] -> b
  | (k, v)::t -> add k v (union t b)
