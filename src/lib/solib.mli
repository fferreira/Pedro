(* imports a scribble style file using the nuscr library, if forgets the
   previous one only if successful *)
val import_nuscr_file : string -> string -> string option

(* loads a pedro file from disk, it forgets the previous one only if
   successful *)
val load_from_file : string -> string option

(* saves a pedro file to disk *)
val save_to_file : string -> bool

(* gets a list of enabled transitions *)
val get_enabled_transitions : unit -> string list

(* tries to do a transition, if it succeeds then it also has the efect of
   updating the markings on the net *)
val do_transition : string -> bool

(* says if the net is in a final state *)
val has_finished : unit -> bool

(* Hash of the commit, used to validate API compatibility when interfacing
 * with Go *)
val commit_hash : string
