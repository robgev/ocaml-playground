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

let rec is_in_line pred line pos =
  pos + String.length pred <= String.length line &&
  (
    String.sub line pos (String.length pred) = pred ||
    is_in_line pred line (pos + 1)
  )

let _ = 
  match Sys.argv with 
    [| _; pred; filename |] -> 
      begin 
        try
          List.iter (
            fun line -> 
              if is_in_line pred line 0 then
                begin 
                  print_string line;
                  print_newline ()
                end
          )
          (read_all_lines filename)
        with 
          e -> 
            print_string "An error occured: \n";
            print_string (Printexc.to_string e);
            print_newline ();
            exit 1
      end 
    | _ -> 
        print_string "Usage: filegrep <predicate> <filename> \n";
        exit 1
