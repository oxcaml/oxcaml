[@@@ocaml.warning "+a-40-41-42"]

(** Replace spill/reload pairs directly adjacent to call instructions with
    push/pop instructions. This reduces code size on x86 because push/pop
    encodings are shorter than mov-to/from-stack-slot encodings. *)
val run : Cfg_with_infos.t -> Cfg_with_infos.t
