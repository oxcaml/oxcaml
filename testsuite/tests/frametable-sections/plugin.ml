(* The calls into the host are deliberately not tail calls, so that the
   plugin's own frames appear in the backtrace. *)

let[@inline never] call_host n =
  let r = Host_api.boom (n + 1) in
  Printf.printf "host returned %d\n" r

let[@inline never] rec deep_alloc n acc =
  if n = 0
  then (
    Gc.full_major ();
    List.length acc)
  else deep_alloc (n - 1) (string_of_int n :: acc) + 1

let () =
  print_endline "plugin loaded";
  Printf.printf "plugin deep %d\n" (deep_alloc 500 []);
  Host_api.register (fun () ->
    call_host 20;
    print_endline "callback done")
