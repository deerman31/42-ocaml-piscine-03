(* main.ml *)

let () =
  let colors = Color.all () in
  List.iter (fun color ->
    Printf.printf "Short: %s, Verbose: %s\n"
      (Color.toString color)
      (Color.toStringVerbose color)
  ) colors