external expose : string -> 'a -> unit = "partition_test_expose"
external primitive : unit -> int = "partition_test_c_only"

let () =
  Shared.record "lazy-c";
  Both.cell := !Both.cell + 2;
  let state = !Shared.state in
  let snapshot = state.value in
  expose "cProbe" (fun () -> (snapshot * 100) + state.value);
  expose "cCell" Both.read;
  expose "cPrimitive" primitive;
  expose "sameIdentity" (fun () ->
    match !Both.b_identity with
    | None -> 0
    | Some (cell, read, exception_) ->
      if cell == Both.cell && read == Both.read && exception_ == Both.exception_
      then 1
      else 0);
  expose "cLiteral" (fun () ->
    Shared.record "MULTI_ROOT_C_ONLY_SENTINEL";
    1)
;;
