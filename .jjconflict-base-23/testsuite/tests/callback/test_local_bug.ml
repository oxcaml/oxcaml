(* TEST
 modules = "test_local_bug_.c";
*)

module M : sig end = struct
  external call : 'fn -> unit = "local_args_repro_call"

  let f () () () () () () () = print_endline "ok"
  let () = call f
end
