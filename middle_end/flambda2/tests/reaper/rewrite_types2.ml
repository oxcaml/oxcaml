(* ocamlopt -flambda2-reaper -reaper-local-fields -flambda2-result-types-all-functions -c rewrite_types1.mli rewrite_types1.ml rewrite_types2.ml *)

external __ignore__ : 'a -> unit = "%ignore"
let check_uniqueness_of_merged () =
  __ignore__ (Rewrite_types1.merge_iter ())
