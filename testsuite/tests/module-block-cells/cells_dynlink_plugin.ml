(* Compiled with the host unit's .cmx: the static field resolves to the
   host's closure symbol, the others to its cells. *)

let () =
  let module A = (val (module Cells_dynlink_api : Cells_dynlink_api.S)) in
  Cells_dynlink_api.report :=
    Printf.sprintf "plugin whole: f 1 = %d, n = %d, !r = %d"
      (A.f 1) A.n !A.r
    :: Printf.sprintf "plugin fields: f 1 = %d, n = %d, !r = %d"
         (Cells_dynlink_api.f 1) Cells_dynlink_api.n !Cells_dynlink_api.r
    :: !Cells_dynlink_api.report;
  A.r := 42
