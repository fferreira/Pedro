let () = Callback.register "main" Lib.main
(*
let () = print_endline "//Vote for Pedro!" ;
         print_endline @@ "//Current working directory: " ^ Sys.getcwd ()  ;
         let fn =
           if Array.length Sys.argv = 2 then
             Sys.argv.(1)
           else
             "examples/proto.pdr"
         in
         Lib.main fn
*)
