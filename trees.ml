type 'a tree = Br of 'a * 'a tree * 'a tree 
            |  Lf of 'a

let rec size t = 
    match t with
        Br (_, tl, tr) -> 1 + size tl + size tr
      | Lf _ -> 0

let rec total t = 
    match t with
        Br (x, tl, tr) -> x + total tl + total tr
      | Lf x -> x

let max a b = if a > b then a else b
let rec max_depth t =
    match t with
        Br (x, tl, tr) -> 1 + max (max_depth tl) (max_depth tr)
      | Lf _ -> 0

let rec list_of_tree t = 
    match t with
        Br (x, tl, tr) -> list_of_tree tl @ [x] @ list_of_tree tr
      | Lf _ -> []

let rec tree_map f t =
    match t with
        Br (x, tl, tr) -> Br (f x, tree_map f tl, tree_map f tr)
      | Lf x -> Lf (f x)


