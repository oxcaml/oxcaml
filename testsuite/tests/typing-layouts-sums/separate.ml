(* TEST
 readonly_files = "sum_shape.mli sum_shape.ml";
 setup-ocamlc.byte-build-env;
 module = "sum_shape.mli";
 ocamlc.byte;
 module = "sum_shape.ml";
 ocamlc.byte;
 module = "separate.ml";
 ocamlc.byte;
*)

module Checked : sig
  type t : (bits32 & bits64) + (value + void)
end = Sum_shape

module Widened : sig
  type refined : bits32 + value
end = Sum_shape

type ('a : bits32 + value) wide
type ('a : bits32 + immediate) narrow

let meet (_ : 'a wide) (_ : 'a narrow) = ()
let check_meet = (meet : Sum_shape.refined wide -> Sum_shape.refined narrow
                         -> unit)
