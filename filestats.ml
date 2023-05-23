
(* Now let's do some file statistics *)
let channel_stats ch = 
  let lines = ref 0 in 
  let chars = ref 0 in
  let words = ref 0 in 
  let sentences = ref 0 in 
    try 
      while true do
        let line = input_line ch in
          lines := !lines + 1;
          chars := !chars + String.length line;
          String.iter 
            (fun single_char -> 
              match single_char with
                  '.' | '?' | '!' -> sentences := !sentences + 1
                | ' ' -> words := !words + 1
                | _ -> ()
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
        print_newline ()

  let file_stats filename = 
    let ch = open_in filename in
      try
        channel_stats ch;
        close_in ch
      with
        (* If something went wrong just close the channel *)
        _ -> close_in ch
