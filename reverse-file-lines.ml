let read_all_lines filename =
  let ch = open_in filename in
  let lines = ref [] in
    try 
      while true do
        lines := (input_line ch) :: !lines
      done;
      []
    with 
      End_of_file -> 
        close_in ch;
        !lines

let write_all_lines filename lines = 
  let ch = open_out filename in 
    List.iter (
      fun s -> 
        output_string ch s;
        output_char ch '\n'
    )
    lines;
    close_out ch

let _ = 
  match Sys.argv with 
    [|_; input_file; output_file |] -> 
      begin 
        try 
          let lines = read_all_lines input_file in
            write_all_lines output_file lines
        with 
          e -> 
            print_string "Error: \n";
            print_string (Printexc.to_string e);
            print_newline ();
            exit 1
      end 
  | _ -> 
      print_string "Usage: reverse <input_filename> <output_filename> \n";
      exit 1
