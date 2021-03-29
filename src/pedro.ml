let fmt_of_string = function
  | "nuscr" -> Pedrolib.Lib.Nuscr
  | "pedro" -> Pedrolib.Lib.Pedro
  | "dot" -> Pedrolib.Lib.Dot
  | "sexp" -> Pedrolib.Lib.Sexp
  | "info" -> Pedrolib.Lib.Info
  | err -> failwith @@ "Unknown option: " ^ err

let () =
  print_endline "(*) Vote for Pedro!" ;
  print_endline @@ "(*) Current working directory: " ^ Sys.getcwd () ;
  if Array.length Sys.argv = 4 then
    let inp = Sys.argv.(1) |> fmt_of_string in
    let out = Sys.argv.(2) |> fmt_of_string in
    let fn = Sys.argv.(3) in
    Pedrolib.Lib.main inp out fn
  else "Invalid arguments" |> print_endline
