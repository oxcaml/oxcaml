(* TEST
 readonly_files = "destructive_base.mli";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   module = "destructive_base.mli";
   ocamlc.byte;
   module = "destructive.ml";
   ocamlc.byte;
 }{
   setup-ocamlopt.byte-build-env;
   module = "destructive_base.mli";
   ocamlopt.byte;
   module = "destructive.ml";
   ocamlopt.byte;
 }
*)

open Destructive_base
module Actual = struct type t = bool end
module type T = S with module X := Actual
module Check (X : T) = struct
  let f (x : X.N.t) : int = x
  let g (x : X.N.u) : bool = x
end

module type Expanded_body = Strengthened with type Target.t := int
module Check_body (X : Expanded_body) = struct
  let f (x : X.N.t) : int = x
end

module type Expanded_module = Module_constraint with type Target.t := int
module Check_module (X : Expanded_module) = struct
  let f (x : X.N.P.t) : int = x
  let g (x : int) : int = X.N.P.id x
end

module type Expanded_modtype = Modtype_constraint with type Target.t := int
module Check_modtype (X : Expanded_modtype) (Y : X.N.P) = struct
  let f (x : Y.t) : int = x
end
