(* TEST
 readonly_files = "usage.mli usage_provider.mli";
 compile_only = "true";
 flags = "-w +34 -warn-error +34";
 setup-ocamlc.byte-build-env;
 module = "usage_provider.mli";
 ocamlc.byte;
 module = "usage.mli";
 ocamlc.byte;
 module = "usage.ml";
 ocamlc.byte;
*)

module type S = sig type hidden type t end

module Refine (X : sig type hidden type t end) : S with type t = X.t = X

module Consume (X : S) = struct end

module Apply (X : sig type hidden type t end) = Consume (Refine (X))

module type Projected = sig type hidden type t end
module Local_arg = struct module type T = Projected end

module Refine_projection (X : sig type hidden type t end) :
  Usage_provider.F(Local_arg).S with type t = X.t = X

module Consume_projection (X : Usage_provider.F(Local_arg).S) = struct end

module Apply_projection (X : sig type hidden type t end) =
  Consume_projection (Refine_projection (X))
