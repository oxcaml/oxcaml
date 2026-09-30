(* TEST
 readonly_files = "definitions.mli";
 compile_only = "true";
 flags = "-opaque";
 {
   setup-ocamlc.byte-build-env;
   module = "definitions.mli";
   ocamlc.byte;
   module = "test.ml";
   ocamlc.byte;
 }{
   setup-ocamlopt.byte-build-env;
   module = "definitions.mli";
   ocamlopt.byte;
   module = "test.ml";
   ocamlopt.byte;
 }
*)

open Definitions

module Check (X : U) = struct
  let t (x : X.t) : int = x
  let u (x : X.u) : int list = x
  let v (x : X.N.v) : int = x
  let w (x : X.N.w) : string = x
  let use (x : int) (y : string) : int list = X.N.use x y
end

module Int = struct type t = int end
module G = F (Int)
module Check_functor (X : G.Result) = struct
  let t (x : X.t) : int = x
  let u (x : X.u) : int list = x
  let v (x : X.N.v) : int = x
end

module type Mod = M with module A = Int
module Check_module (X : Mod) = struct
  let a (x : X.A.t) : int = x
  let b (x : X.B.t) : int = x
end

module type Modtype = MT with module type T = sig type t = int end
module Check_modtype (X : Modtype) = struct
  let t (x : X.A.t) : int = x
end

module type Destructive = U with type t := int
module Check_destructive (X : Destructive) = struct
  let v (x : X.N.v) : int = x
  let use (x : int) (y : string) : int list = X.N.use x y
end

module type Shadow = sig
  type t = string
  module X : T
end
module Check_shadow (X : Shadow) = struct
  let outer (x : X.t) : string = x
  let inner (x : X.X.t) : int = x
end

module Check_record (X : Refined_record) = struct
  let field (x : X.r) : int = x.field
end

module type Nested_modtype = sig
  type t
  module type Inner = sig type u = t end
  module N : Inner
end
module type Nested_int = Nested_modtype with type t = int
module Check_nested_modtype (X : Nested_int) = struct
  let f (x : X.N.u) : int = x
end

module Check_rhs (X : Shadow_rhs) = struct
  let key (x : X.t) : X.M.key = x
end
