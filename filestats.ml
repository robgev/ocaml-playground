
(* Let's use array data type for character stats *)
(* The function takes arr of type int array and 
   prints histogram of the arr
*)
let print_histogram arr = 
  print_string "Character frequency histogram:";
  print_newline ();
  for x = 0 to 255 do
    if arr.(x) > 0 then
      begin
        print_string "For character '";
        print_char (char_of_int x);
        print_string "'(character number ";
        print_int x;
        print_string ") the count is ";
        print_int arr.(x);
        print_string ".";
        print_newline ()
      end
  done

(* Now let's do some file statistics *)
let channel_stats ch = 
  let lines = ref 0 in 
  let chars = ref 0 in
  let words = ref 0 in 
  let sentences = ref 0 in 
  (* There are 255 ASCII chars, all counts are 0 for now *)
  let histogram = Array.make 255 0 in
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
      done
    with 
      End_of_file ->
        print_string "The file has ";
        print_int !lines;
        print_string " lines, ";
        print_int !chars;
        print_string " characters, ";
        print_int !words;
        print_string " words, and ";
        print_int !sentences;
        print_string " sentences.";
        print_newline ();
        print_histogram histogram

let file_stats filename = 
  let ch = open_in filename in
    try
      channel_stats ch;
      close_in ch
    with
      (* If something went wrong just close the channel *)
      _ -> close_in ch
