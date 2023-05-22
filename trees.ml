type 'a tree = Br of 'a * 'a tree * 'a tree 
            |  Lf

let rec size t = 
    match t with
        Br (_, tl, tr) -> 1 + size tl + size tr
      | Lf -> 0

let rec total t = 
    match t with
        Br (x, tl, tr) -> x + total tl + total tr
      | Lf -> 0

let max a b = if a > b then a else b
let rec max_depth t =
    match t with
        Br (x, tl, tr) -> 1 + max (max_depth tl) (max_depth tr)
      | Lf -> 0

let rec list_of_tree t = 
    match t with
        Br (x, tl, tr) -> list_of_tree tl @ [x] @ list_of_tree tr
      | Lf -> []

let rec tree_map f t =
    match t with
        Br (x, tl, tr) -> Br (f x, tree_map f tl, tree_map f tr)
      | Lf -> Lf

let rec binary_search tr k =
    match tr with
      Lf -> None
    | Br ((k', v), l, r) -> 
        if k' = k then Some v
        else if k < k' then
          binary_search l k
        else binary_search r k

(*TODO: Use leaves to store data*)
let rec binary_search_tree_insert tr k v =
    match tr with
      Lf -> Br ((k, v), Lf, Lf)
    | Br ((k', v'), l, r) -> 
        if k = k' then Br((k', v), l, r)
        else if k < k' then Br((k', v'), binary_search_tree_insert l k v, r)
        else Br((k', v'), l, binary_search_tree_insert r k v)

(* Exercises *)
let rec is_in_tree v tr =
    match tr with
      Lf -> false
    | Br (x, l, r) -> 
        x = v || (is_in_tree v l) || (is_in_tree v r)

let rec mirror tr =
    match tr with
      Lf -> Lf
    | Br(x, l, r) -> Br(x, mirror r, mirror l)

let rec same_shape tr1 tr2 =
    match tr1, tr2 with
      Lf, Lf -> true
    | Br(_, l1, r1), Br(_, l2, r2) -> 
        (same_shape l1 l2) && (same_shape r1 r2)
    | _ -> false

(*
  Compare two trees, no matter their types
  For this we need to make them both of the 
  same type and then can use = operator
*)
let same_shape_general tr1 tr2 = 
  tree_map (fun _ -> 0) tr1 = tree_map (fun _ -> 0) tr2


let rec tree_of_list l = 
  match l with 
    [] -> Lf
  | (k, v)::t -> binary_search_tree_insert (tree_of_list t) k v

(*
  Quick tail recursive implementation
*)
let rec tree_of_list_aux l tr = 
  match l with
    [] -> tr
  | (k, v)::t -> tree_of_list_aux t (binary_search_tree_insert tr k v)

let rec tree_of_list_tl l = tree_of_list_aux l Lf

(*
  Merge two binary search trees
*)
let rec merge tr1 tr2 =
  tree_of_list_tl (list_of_tree tr1 @ list_of_tree tr2)

(*
  Trees with arbitrary number of branches at each node
  abt = arbitrary_branched_tree
  Leaves are also branches with ts of []
*)
type 'a abt = 
  Br of 'a * 'a abt list

let rec sum_aux acc l =
  match l with
    [] -> acc
  | h::t -> sum_aux (acc + h) t 

let list_sum = sum_aux 0

let rec list_map f l =
  match l with
    [] -> []
  | h::t -> f h :: list_map f t

let rec abt_size (Br(r, bs)) =
  1 + list_sum (list_map abt_size  bs)

let rec abt_total (Br(r, bs)) =
  r + list_sum (list_map abt_total  bs)

let rec abt_map f (Br(r, bs)) =
  Br(f r, list_map (abt_map f) bs)

