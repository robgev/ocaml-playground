(* Some float numbers 
   Note the operations have a dot after the sign
*)
let a = 1.5
let b = 6.
let c = -.2.3
let d = a +. b
let e = 1.0 /. 1000.0
let f = 1. /. 1000000.0
let g = 2.0 ** 10.
let h = a *. b
(* Notice the calculation error *)
let i = 3.123 -. 3.
(* Check the range of values by *)
let j = max_float
let k = min_float
(* Float utils that are provided *)

(* Square root *)
let two = sqrt 4.

(* Natural logarithm, exponent *)
let one = log (exp 1.)

(* Base ten log *)
let dec = log10 100.

(* Trigonometric functions *)
let one_sin = sin (Float.pi /. 2.)
let minus_one = cos Float.pi 
let one_tan = tan (Float.pi /. 4.)
let pi_4 = atan one_tan

(* Floor/ceil *)
let ten = floor 10.23498
let eleven = ceil 10.23498

(* Now time for some fun *)
let make_vector (x1, y1) (x2, y2) =
  (x2 -. x1, y2 -. y1)

let vector_length (x, y) =
  sqrt (x ** 2. +. y ** 2.)

let offset_point (x, y) (px, py) =
  (px +. x, py +. y)

let scale_to_length l (a, b) =
  let veclength = vector_length (a, b) in
    if veclength = 0. then (a, b) else (* avoid values like infinity *)
      let factor = l /. veclength in 
        (a *. factor, b *. factor)

(* Exercises *)
let round n = 
  if n -. floor n >= 0.5 then 
    ceil n 
  else 
    floor n

let midpoint (x1, y1) (x2, y2) = 
  ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

let parts_of n =
  if n < 0. then
    (ceil n, n -. ceil n)
  else
    (floor n, n -. floor n)

let star n =
  let pos = 
      if n < 0. then 
        50 - int_of_float ((-.n) *. 50.) 
      else 50 + int_of_float (n *. 50.) 
  in 
    for i = 0 to pos - 1 do print_char ' ' done;
    print_char '*';
    print_newline ()

let plot fn _from _to step = 
  let pos = ref _from in
    while !pos <= _to do
      star (fn !pos);
      pos := !pos +. step
    done
