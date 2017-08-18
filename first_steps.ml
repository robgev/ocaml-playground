let x = 200 in (let y = x * x in x + y)

(* This is how you define a function *)
let cube x = x * x * x
let neg x = x < 0
let isVowel1 c =
  c = 'a' || c = 'e' || c  = 'i' || c = 'o' || c = 'u'
let isSumTen a b =
  a + b = 10
