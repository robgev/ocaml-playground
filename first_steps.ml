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
