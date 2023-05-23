let sum_of_array arr = 
  let total = ref 0 in
    for i = 0 to Array.length arr - 1 do
      total := !total + arr.(i)
    done;
    !total

(* Need swap util for the next exercise *)
let swap arr i1 i2 = 
  let tmp = arr.(i1) in
    arr.(i1) <- arr.(i2);
    arr.(i2) <- tmp

let reverse_in_place arr = 
  let l = Array.length arr in 
    if l > 1 then 
      (* For loops goes 0 to n INCLUSIVE *)
      for i = 0 to l / 2 - 1 do
        swap arr i (l - 1 - i)
      done

(* Cannot do Array.make n (Array.make n 0)
   because this means populating the ref
   to the same array in all rows 
*)
let table_as_array n = 
  let result = Array.make n [||] in 
    for i = 0 to n - 1 do
      result.(i) <- Array.make n 0
    done;
    for i = 1 to n do 
      for j = 1 to n do
        result.(i - 1).(j - 1) <- i * j
      done
    done;
    result


