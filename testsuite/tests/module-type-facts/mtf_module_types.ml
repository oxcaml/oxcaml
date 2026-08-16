(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Module-type facts extracted from module ascriptions and from the ways one
   module type is built out of another: aliases, includes, [with]
   constraints, destructive substitutions, [module type of] and
   strengthening.  See [mtf_facts.ml] for the output format. *)

open Mtf_facts

let () = heading "an ascription, and the members of an ascribed signature"

(* The ascription of [Pair] is checked as a whole, and each module member of
   the signature it is ascribed is paired with the member of the
   implementation it is checked against. *)
let () =
  report_implementation ~filename:"ascription.ml"
    {|
module type S = sig type t end
module M : S = struct type t = int end
module Pair : sig module Inner : S end = struct
  module Inner = struct type t = int end
end
|}

let () =
  heading "aliases, includes, with constraints, module type of, functor types"

let () =
  report_implementation ~filename:"edges.ml"
    {|
module type S = sig type t end
module type A = S
module type W = S with type t = int
module type K = sig include S val k : int end
module type Fn = functor (X : S) -> K
module M : S = struct type t = int end
module type G = module type of M
|}

let () = heading "destructive substitution and strengthening"

let () =
  report_implementation ~filename:"subst.ml"
    {|
module type S = sig type t end
module type Base = sig type t end
module type T = sig module type X type y end
module type U = T with module type X := S
module M : S = struct type t = int end
module type K = Base with M
module N : K = M
|}

let () = heading "module type of a structure that is not a path is omitted"

(* The subject of the [module type of] cannot be named, so the definition of
   [T] is recorded as partial rather than silently dropped. *)
let () =
  report_implementation ~filename:"typeofanon.ml"
    {|
module type T = module type of struct type t = int end
|}
