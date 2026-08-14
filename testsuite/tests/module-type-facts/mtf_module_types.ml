(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Module-type facts extracted from module ascriptions and from the ways one
   module type is built out of another: aliases, includes, [with]
   constraints, destructive substitutions, [module type of] and
   strengthening.  See [mtf_facts.ml] for the output format. *)

open Mtf_facts

let () = heading "ascription of a structure against a named module type"

let () =
  report_implementation ~filename:"ascription.ml"
    {|
module type S = sig type t end
module M : S = struct type t = int end
|}

let () = heading "the check is directional: only the implementation is checked"

let () =
  report_implementation ~filename:"directional.ml"
    {|
module type S = sig type t end
module type T = sig type t end
module M : S = struct type t = int end
module N : T = struct type t = int end
|}

let () = heading "aliases, includes, with constraints and module type of"

let () =
  report_implementation ~filename:"edges.ml"
    {|
module type S = sig type t end
module type A = S
module type W = S with type t = int
module M : S = struct type t = unit end
module type G = module type of M
module type K = sig
  include S
  val k : int
end
module MW : W = struct type t = int end
module MK : K = struct type t = unit let k = 1 end
module MA : A = struct type t = char end
|}

let () = heading "composed destructive substitutions of types"

let () =
  report_implementation ~filename:"subst.ml"
    {|
module type S1 = sig type a end
module type S2 = sig type b end
module type T = sig
  include S1
  include S2
  type c
end
module type U = T with type a := int with type b := bool
module M : U = struct type c = int end
|}

let () = heading "destructive substitution of a module type"

let () =
  report_implementation ~filename:"mtsubst.ml"
    {|
module type S2 = sig type x end
module type T = sig module type X type y end
module type U = T with module type X := S2
module M : U = struct type y = int end
|}

let () = heading "with module and with module := check the substituted module"

let () =
  report_implementation ~filename:"withmod.ml"
    {|
module type S = sig type t end
module type T = sig module M : S type u end
module N = struct type t = int end
module type U = T with module M = N
module type V = T with module M := N
module MU : U = struct module M = N type u = unit end
module MV : V = struct type u = unit end
|}

let () = heading "strengthening depends on the subject's own expectation"

let () =
  report_implementation ~filename:"strength.ml"
    {|
module type S = sig type t end
module type Base = sig type t end
module M : S = struct type t = int end
module type K = Base with M
module N : K = M
|}

let () = heading "module type of a structure that is not a path is omitted"

let () =
  report_implementation ~filename:"typeofanon.ml"
    {|
module type S = sig type t end
module type T = module type of struct type t = int end
module M : S = struct type t = unit end
module N : T = struct type t = int end
|}

let () = heading "packages and first-class module types"

let () =
  report_implementation ~filename:"package.ml"
    {|
module type S = sig type t end
module M = struct type t = int end
let packed = (module M : S)
let repack (module X : S) = (module X : S)
|}

let () = heading "an interface source records the same dependency facts"

let () =
  report_interface ~filename:"edgesintf.mli"
    {|
module type S = sig type t end
module type A = S
module type W = S with type t = int
module M : S
module type K = sig include S val k : int end
|}
