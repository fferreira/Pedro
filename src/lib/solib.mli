
(* loads a pedro file from disk, it forgets the previous one if successful *)
val load_from_file : string -> bool

(* saves a pedro file to disk *)
val save_to_file : string -> bool

(* gets a list of enabled transitions *)
val get_enabled_transitions : unit -> string list

(* tries to do a transition, if it succeeds then it also has the efect
   of updating the markings on the net *)
val do_transition : string -> bool
