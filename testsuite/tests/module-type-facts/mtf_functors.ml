(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Module-type facts of functors: the expectation recorded for a functor
   parameter, the argument checks recorded at each application, and the
   identity of the module types reached through an application.  See
   [mtf_facts.ml] for the output format. *)

open Mtf_facts

let () = heading "a parameter expectation is checked at each application"

let () =
  report_implementation ~filename:"apply.ml"
    {|
module type S = sig type t end
module F (X : S) = struct type u = X.t end
module A = struct type t = int end
module B = struct type t = char end
module FA = F (A)
module FB = F (B)
|}

let () = heading "the parameter of a functor type in a signature"

let () =
  report_implementation ~filename:"functortype.ml"
    {|
module type S = sig type t end
module type T = sig type u end
module type F = functor (X : S) -> T
module G : F = functor (X : S) -> struct type u = X.t end
|}

let () = heading "an anonymous parameter and an opaque result signature"

let () =
  report_implementation ~filename:"opaque.ml"
    {|
module type PARAM = sig type t end
module type OUT = sig module type T = sig type r end end
module F (_ : PARAM) : OUT = struct
  module type T = sig type r end
end
module Int = struct type t = int end
module R = F (Int)
module M : R.T = struct type r = int end
|}

let () =
  heading "applicative applications of equal arguments share one instance"

let () =
  report_implementation ~filename:"applicative.ml"
    {|
module type S = sig module type T = sig type u end end
module F (X : sig type t end) : S = struct module type T = sig type u end end
module A = struct type t = int end
module R1 = F (A)
module R2 = F (A)
module M1 : R1.T = struct type u = int end
module M2 : R2.T = struct type u = int end
|}

let () = heading "generative applications get distinct site anchors"

let () =
  report_implementation ~filename:"generative.ml"
    {|
module type S = sig module type T = sig type u end end
module F () : S = struct module type T = sig type u end end
module R1 = F ()
module R2 = F ()
module M1 : R1.T = struct type u = int end
module M2 : R2.T = struct type u = int end
|}

let () = heading "parameter members are checked against the argument's members"

let () =
  report_implementation ~filename:"argmember.ml"
    {|
module type S = sig type t end
module type Members = sig
  module type T = S

  module M : S
end
module F (X : Members) = struct type u = X.M.t end
module A = struct
  module type T = S

  module M : S = struct type t = int end
end
module FA = F (A)
|}

let () = heading "a strengthened parameter keeps the subject at the argument"

let () =
  report_implementation ~filename:"strengthparam.ml"
    {|
module type S = sig type t end
module M : S = struct type t = int end
module F (X : S with M) = struct type u = X.t end
module FM = F (M)
|}

let () = heading "a functor body declaration is reached through the body"

let () =
  report_implementation ~filename:"template.ml"
    {|
module F (X : sig type t end) = struct
  module type S = sig type t = X.t end
  module M : S = struct type t = X.t end
end
module Int = struct type t = int end
module R = F (Int)
|}

let () = heading "inline and generative inclusions anchor at a site"

(* An application whose result is included has no path to name it, so its
   instance is anchored at a [Site] context, one per occurrence in the
   artifact. *)
let () =
  report_implementation ~filename:"site.ml"
    {|
module type S = sig module type T = sig type u end end
module type S2 = sig module type U = sig type v end end
module F (X : sig type t end) : S = struct module type T = sig type u end end
module G () : S2 = struct module type U = sig type v end end
include F (struct type t = int end)
include G ()
module M : T = struct type u = int end
module N : U = struct type v = int end
|}

let () = heading "applications appearing in a module type path"

let () =
  report_implementation ~filename:"pathapply.ml"
    {|
module type S = sig module type T = sig type u end end
module F (X : sig type t end) : S = struct module type T = sig type u end end
module A = struct type t = int end
module M : F(A).T = struct type u = int end
|}
