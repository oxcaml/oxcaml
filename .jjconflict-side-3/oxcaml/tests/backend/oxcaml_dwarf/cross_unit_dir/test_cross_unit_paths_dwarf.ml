(* This file deliberately shares its basename with the consuming unit. It is
   compiled as [Cu_prim]; see gen/gen_dune.ml. *)

external prim_time : unit -> float = "caml_sys_time"
