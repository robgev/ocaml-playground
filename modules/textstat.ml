(* Text statistics module *)
type stats = int * int * int * int * int array

(* Some interface to return different statistics
   as needed 
*)
let lines (l, _, _, _, _) = l
let characters (_, c, _, _, _) = c 
let words (_, _, w, _, _) = w
let sentences (_, _, _, s, _) = s
let frequency (_, _, _, _, h) c = h.(int_of_char c)

(* Now let's do some file statistics *)
let stats_from_channel ch = 
  let lines = ref 0 in 
  let chars = ref 0 in
  let words = ref 0 in 
  let sentences = ref 0 in 
  let histogram = Array.make 256 0 in
    try 
      while true do
        let line = input_line ch in
          lines := !lines + 1;
          chars := !chars + String.length line + 1;
          (* Go over every char. If it's a char that 
             ends the sentences increment sentences
             If it's a space increment words. Otherwise
             do nothing
          *)
          String.iter 
            (fun single_char -> 
              match single_char with
                  '.' | '?' | '!' -> sentences := !sentences + 1
                | ' ' -> words := !words + 1
                | _ -> ()
            )
            line;
          (* Since we count words with spaces the last words
             get ommited. So for every line count for one more
             word
          *)
          words := !words + 1;

          (* Now go over every character and add increment count
              in the histogram array
          *)
          String.iter
            (fun single_char -> 
              let i = int_of_char single_char in 
                histogram.(i) <- histogram.(i) + 1
            )
            line
      done;
      (* The function should return int * int * int * int 
         so we need this here to make the types agree
      *)
      (0, 0, 0, 0, [||])
    with 
      End_of_file -> (!lines, !chars, !words, !sentences, histogram)

(* Read stats from a file *)
(* TODO: Handle exceptions *)
let stats_from_file filename = 
  let ch = open_in filename in
    let result = stats_from_channel ch in
      close_in ch;
      result
