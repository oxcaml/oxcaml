(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Module-type facts of functors: the expectation recorded for a functor
   parameter and checked at each application, the members of that expectation
   checked against the members of the argument, and the context an
   application that has no path to name it is anchored at.  See
   [mtf_facts.ml] for the output format. *)

open Mtf_facts

let () = heading "a parameter expectation is checked at each application"

let () =
  report_implementation ~filename:"apply.ml"
    {|
module type S = sig type t end
module F (X : S) = struct type u = X.t end
module A = struct type t = int end
module FA = F (A)
|}

let () = heading "parameter members are checked against the argument's members"

let () =
  report_implementation ~filename:"argmember.ml"
    {|
module type S = sig type t end
module type Members = sig module type T = S end
module F (X : Members) = struct end
module A = struct module type T = S end
module FA = F (A)
|}

let () = heading "an inlined application anchors at a site"

(* An application whose result is included has no path to name it, so its
   instance is anchored at a [Site] context, one per occurrence in the
   artifact. *)
let () =
  report_implementation ~filename:"site.ml"
    {|
module type S = sig module type T = sig type u end end
module F (X : sig type t end) : S = struct module type T = sig type u end end
include F (struct type t = int end)
module M : T = struct type u = int end
|}
