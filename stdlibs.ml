let concat = 
  let rec concat_aux acc ls = 
    match ls with 
      [] -> acc
    | h::t -> concat_aux (acc @ h) t 
  in
    concat_aux [] 

let all_have_true =
  List.for_all (List.mem true)

let all_have_true2 ls = 
  not (List.mem false (List.map (List.mem true) ls))

let count_exclamations = 
  String.fold_left (
    fun acc c -> if c = '!' then acc + 1 else acc 
  )
  0

let calm = 
  String.map (
    function '!' -> '.'
      | c -> c
  )

let concat_all = String.concat ""

let concat_all_buff ss = 
  let b = Buffer.create 16 in
    List.iter (Buffer.add_string b) ss;
  Buffer.contents b

let occurences pred s = 
  if pred = "" then 0 else 
    let result = ref 0 in 
    let str = ref s in 
      while 
        String.length pred <= String.length !str && !str <> ""
      do 
        if String.sub !str 0 (String.length pred) = pred then
          result := !result + 1;
        str := String.sub !str 1 (String.length !str - 1)
      done;
      !result
