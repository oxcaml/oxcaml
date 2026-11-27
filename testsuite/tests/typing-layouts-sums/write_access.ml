(* TEST
 readonly_files = "sum_storage.mli sum_storage.ml check_replacements.sh";
 setup-ocamlc.byte-build-env;
 flags = "-c";
 module = "sum_storage.mli";
 ocamlc.byte;
 module = "sum_storage.ml";
 ocamlc.byte;
 module = "write_access.ml";
 ocamlc.byte;
 script = "sh check_replacements.sh interfaces ${ocamlsrcdir}/stdlib ${ocamlrun} ${ocamlc_byte}";
 script;
*)

open Sum_storage

external boxed_blit : t array array -> int -> t array array @ write corrupted ->
  int -> int -> unit = "%arrayblit"

external[@layout_poly] weak_set : ('a : any separable).
  'a array @ write corrupted -> int -> 'a -> unit = "%array_safe_set"

let copy_one src (dst : t array @ write uncontended) = copy src dst
let copy_boxes src (dst : t array array @ write corrupted) =
  boxed_blit src 0 dst 0 1
let replace_scalar (dst : int array @ write corrupted) = weak_set dst 0 1
