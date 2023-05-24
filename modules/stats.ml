(* Command line tool to find the statistics of any text file *)

(* The function takes arr of type int array and 
   prints histogram of the arr
*)
let print_histogram stats = 
  print_string "Character frequency histogram:";
  print_newline ();
  for x = 0 to 255 do
    let freq = Textstat.frequency stats (char_of_int x) in  
      if freq > 0 then
        begin
          print_string "For character '";
          print_char (char_of_int x);
          print_string "'(character number ";
          print_int x;
          print_string ") the count is ";
          print_int freq;
          print_string ".\n";
        end
  done
in
  try
    begin
      match Sys.argv with 
        [|_; filename |] -> 
          let stats = Textstat.stats_from_file filename in
            print_string "The file has ";
            print_int (Textstat.lines stats);
            print_string " lines, ";
            print_int (Textstat.characters stats);
            print_string " characters, ";
            print_int (Textstat.words stats);
            print_string " words, and ";
            print_int (Textstat.sentences stats);
            print_string " sentences.";
            print_newline ();
            print_histogram stats
        | _ -> 
            print_string "The program accepts just one argument of filename:";
            print_newline ();
            print_string "Usage: stats <filename>";
            print_newline ();
    end
  with 
    e -> 
      print_string "An error occured: ";
      print_string (Printexc.to_string e);
      print_newline ();
      exit 1
