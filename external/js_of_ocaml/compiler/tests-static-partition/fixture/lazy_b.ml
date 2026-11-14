external expose : string -> 'a -> unit = "partition_test_expose"
external primitive : unit -> int = "partition_test_b_only"

let value = 7
let (_ : Lazy_root.marker option) = None

let () =
  Shared.record "lazy-b";
  Both.cell := !Both.cell + 1;
  Both.b_identity := Some (Both.cell, Both.read, Both.exception_);
  let state = !Shared.state in
  let snapshot = state.value in
  expose "bProbe" (fun () -> (snapshot * 100) + state.value);
  expose "bCell" Both.read;
  expose "bPrimitive" primitive;
  expose "bLiteral" (fun () ->
    Shared.record "MULTI_ROOT_B_ONLY_SENTINEL";
    1)
;;
