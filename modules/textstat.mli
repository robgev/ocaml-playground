(* Textstat module interface *)

(* Bad definition example 
   This is bad for several reasons
   - Exposes the type of stats - which can be misused to
     create non-realistic stats (such as negative nums etc)
   - Exposes stats_from_channel which also can be misused 
  In general, implementation details should not be visible
  to the module users

type stats = int * int * int * int

(** Count the number of lines *)
val lines : stats -> int

(** Return the number of characters from stats *)
val characters : stats -> int

(** Return the number of words from stats *)
val words : stats -> int 

(** Return the number of sentences from stats *)
val sentences : stats -> int 

val stats_from_channel : in_channel -> stats

val stats_from_file : string -> stats
*)

type stats (* User should just be aware of the type, not what it actually is *) 

(** Count the number of lines *)
val lines : stats -> int

(** Return the number of characters from stats *)
val characters : stats -> int

(** Return the number of words from stats *)
val words : stats -> int 

(** Return the number of sentences from stats *)
val sentences : stats -> int 

(** Return the frequency of a single char *)
val frequency : stats -> char -> int

(* The only way to get something of type stats is 
   stats_from_file now so to use the module you need to 
   start with this function
*)
val stats_from_file : string -> stats
