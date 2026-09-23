(* Compiled without the host unit's .cmx: every field, including the static
   closure, is loaded from the host's cells at run time. *)

let () =
  let module A = (val (module Cells_dynlink_api : Cells_dynlink_api.S)) in
  Cells_dynlink_api.report :=
    Printf.sprintf "nocmx whole: f 1 = %d, n = %d, !r = %d"
      (A.f 1) A.n !A.r
    :: Printf.sprintf "nocmx fields: f 1 = %d, n = %d, !r = %d"
         (Cells_dynlink_api.f 1) Cells_dynlink_api.n !Cells_dynlink_api.r
    :: !Cells_dynlink_api.report;
  A.r := 43
