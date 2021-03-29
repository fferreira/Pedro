open Pedrolib

let () =
  (* Solib exported functions *)
  Callback.register "import_nuscr_file" Solib.import_nuscr_file ;
  Callback.register "load_from_file" Solib.load_from_file ;
  Callback.register "save_to_file" Solib.save_to_file ;
  Callback.register "get_enabled_transitions" Solib.get_enabled_transitions ;
  Callback.register "do_transition" Solib.do_transition ;
  Callback.register "has_finished" Solib.has_finished ;
  Callback.register "commit_hash" Solib.commit_hash
