(* The calls into the host are deliberately not tail calls, so that the
   plugin's own frames appear in the backtrace. *)

let[@inline never] call_host n =
  let r = Host_api.boom (n + 1) in
  Printf.printf "host returned %d\n" r

let () =
  print_endline "plugin loaded";
  Host_api.register (fun () ->
    call_host 20;
    print_endline "callback done")
