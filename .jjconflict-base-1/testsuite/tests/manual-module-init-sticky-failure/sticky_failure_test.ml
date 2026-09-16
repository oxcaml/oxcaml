(* Test that module initialization failures are sticky: once a module
   fails, all subsequent attempts to initialize it (or modules that
   depend on it) return the same stored exception.

   Dependency graph:
     Fail_a -> Fail_b -> Fail_c -> Fail_config
     Fail_d -> Fail_b -> Fail_c -> Fail_config
   Fail_c raises [Invalid_config] based on [Fail_config.state]. *)

external init_module : string -> unit = "caml_init_module_from_ocaml"

let string_of_state = function
  | Fail_config.Not_ready -> "Not_ready"
  | Fail_config.Broken msg -> Printf.sprintf "Broken %S" msg
  | Fail_config.Ready -> "Ready"

let expect_exception name =
  match init_module name with
  | () ->
    Printf.printf "ERROR: expected exception from %s\n%!" name;
    exit 1
  | exception (Fail_config.Invalid_config reason) ->
    Printf.printf "%s raised Invalid_config (%s)\n%!"
      name (string_of_state reason);
    reason
  | exception exn ->
    Printf.printf "ERROR: unexpected exception from %s: %s\n%!"
      name (Printexc.to_string exn);
    exit 1

let () =
  (* Fail_a -> Fail_b -> Fail_c: Fail_c raises, so Fail_a should fail. *)
  let reason1 = expect_exception "Fail_a" in
  (* Set state to Ready.  If Fail_c were re-run it would succeed,
     but the cached failure on Fail_b should prevent that. *)
  Fail_config.state := Ready;
  (* Fail_d -> Fail_b: Fail_b is cached as failed. *)
  let reason2 = expect_exception "Fail_d" in
  (* Fail_b and Fail_c should also be cached as failed. *)
  let reason3 = expect_exception "Fail_b" in
  let reason4 = expect_exception "Fail_c" in
  (* All should be the exact same object (from the cached exception). *)
  if reason1 == reason2 && reason2 == reason3 && reason3 == reason4 then
    Printf.printf "OK: same reason object\n%!"
  else begin
    Printf.printf "FAIL: different reason objects\n%!";
    exit 1
  end
