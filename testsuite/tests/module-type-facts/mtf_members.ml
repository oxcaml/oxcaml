(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Module-type facts about the members of an ascription: when a module is
   ascribed a signature, the module members of that signature are checked
   against the corresponding members of the implementation.  Also covers
   recursive modules and forward references, where the expectation of a
   sibling is only known once the group is typed.  See [mtf_facts.ml] for the
   output format. *)

open Mtf_facts

let () = heading "members of an ascribed signature are checked pairwise"

let () =
  report_implementation ~filename:"members.ml"
    {|
module type S = sig type t end
module M : sig
  module Inner : S
  module Other : sig type u end
end = struct
  module Inner = struct type t = int end
  module Other = struct type u = char end
end
|}

let () = heading "nested members pair with each side's own declaration"

let () =
  report_implementation ~filename:"nestpair.ml"
    {|
module type SB = sig type b end
module M : sig
  module N : sig module type T = sig type b end end
end = struct
  module N = struct module type T = SB end
end
|}

let () = heading "members reached through an include pair through the body"

let () =
  report_implementation ~filename:"incpair.ml"
    {|
module type SB = sig type b end
module Base = struct module type T = SB end
module M : sig module type T = sig type b end end = struct
  include Base
end
|}

let () = heading "an abstract expected module type seals without pairing"

let () =
  report_implementation ~filename:"abspair.ml"
    {|
module type SB = sig type b end
module M : sig module type A end = struct module type A = SB end
|}

let () = heading "members of a module type of a named module"

let () =
  report_implementation ~filename:"typeofmember.ml"
    {|
module type S = sig type t end
module Base = struct module Inner : S = struct type t = int end end
module type T = module type of Base
module N : T = struct module Inner = struct type t = int end end
module P : T = Base
|}

let () = heading "members of a module type of an anonymous structure"

let () =
  report_implementation ~filename:"typeofanonmember.ml"
    {|
module type S = sig type t end
module type T = module type of struct
  module Inner : S = struct type t = int end
end
module N : T = struct
  module Inner = struct type t = int end
end
|}

let () = heading "recursive modules resolve in their final declarations"

let () =
  report_implementation ~filename:"recursive.ml"
    {|
module rec A : sig
  module type LOCAL = sig type t end
  module Inner : LOCAL
end = struct
  module type LOCAL = sig type t end
  module Inner = struct type t = int end
end
and B : sig type u end = struct type u = int end
module Client : A.LOCAL = struct type t = unit end
|}

let () = heading "an ascription inside a recursive group keeps its check"

let () =
  report_implementation ~filename:"recrelev.ml"
    {|
module type S = sig type t end
module N : sig
  module rec A : S
  and B : sig end
end = struct
  module rec A : S = struct type t = int end
  and B : sig end = struct end
end
|}

let () = heading "a forward reference in a recursive group"

let () =
  report_implementation ~filename:"recforward.ml"
    {|
module type S = sig type t end
module rec A : sig module type T end = struct
  module type T = module type of B
end
and B : sig include S end = struct type t = int end
|}

let () = heading "a recursive functor keeps its parameter expectation"

let () =
  report_implementation ~filename:"recfunctor.ml"
    {|
module type S = sig type t end
module rec F : functor (X : S) -> sig type u end =
  functor (X : S) -> struct type u = X.t end
and A : S = struct type t = int end
module FA = F (A)
|}

let () = heading "a local module in an expression is checked"

let () =
  report_implementation ~filename:"localmodule.ml"
    {|
module type S = sig type t end
module M = struct type t = int end
let f () = let module L : S = M in ()
|}
