(* TEST
 expect;
*)

(* Matching a GADT witness can refine an abstract type's jkind (here [M.t : any]
   becomes [bits64] under [T]). A type relying on that narrowing must not escape
   the match.

   These tests check that such a type cannot be laundered out through
   a module type produced by a functor application *)

type ('a : any) is_bits64 = T : ('a : bits64). 'a is_bits64

module M : sig
  type t : any
  val witness : t is_bits64
end = struct
  type t = int64#
  let witness = T
end

type ('a : bits64) foo
[%%expect {|
type ('a : any) is_bits64 = T : ('a : bits64). 'a is_bits64
module M : sig type t : any val witness : t is_bits64 end
type ('a : bits64) foo
|}]

(* For reference: directly writing [M.t foo] is rejected. *)
let direct =
  let T = M.witness in
  (fun (x : M.t foo) -> x)
[%%expect {|
Line 3, characters 7-20:
3 |   (fun (x : M.t foo) -> x)
           ^^^^^^^^^^^^^
Error: This pattern matches values of type "M.t foo"
       but a pattern was expected which matches values of type "'a"
       This instance of "$0" is ambiguous:
       it would escape the scope of its equation
|}]

(* Regression test: Laundering it through a functor application is rejected too
*)
module F_c (X : sig type t : bits64 end) = struct
  let f (x : X.t foo) = x
end

let via_functor_application =
  let T = M.witness in
  let module N = F_c(M) in
  N.f
[%%expect {|
module F_c :
  functor (X : sig type t : bits64 end) -> sig val f : X.t foo -> X.t foo end
Line 8, characters 2-5:
8 |   N.f
      ^^^
Error: The value "N.f" has type "M.t foo -> M.t foo"
       but an expression was expected of type "'a"
       This instance of "$0" is ambiguous:
       it would escape the scope of its equation
|}]


(* We also reject a module with a module type produced by a functor application
*)
module Mk_S (X : sig type t : bits64 end) = struct
  module type S = sig val f : X.t foo -> X.t foo end
end

let via_modtype_path =
  let T = M.witness in
  let module N : Mk_S(M).S = struct let f x = x end in
  N.f
[%%expect {|
module Mk_S :
  functor (X : sig type t : bits64 end) ->
    sig module type S = sig val f : X.t foo -> X.t foo end end
Line 8, characters 2-5:
8 |   N.f
      ^^^
Error: The value "N.f" has type "M.t foo -> M.t foo"
       but an expression was expected of type "'a"
       This instance of "$0" is ambiguous:
       it would escape the scope of its equation
|}]

(* Similar test, but we match on the witness inside of the module *)
module F_b (X : sig type t : bits64 end) = struct
  let f (x : X.t foo) = x
end

module Mk_S_b (X : sig type t : bits64 end) = struct
  module type S = sig val f : X.t foo -> X.t foo end
end

let via_first_class_module =
  let module N =
    (val
      let T = M.witness in
      (module F_b(M) : Mk_S_b(M).S))
  in
  N.f
[%%expect {|
module F_b :
  functor (X : sig type t : bits64 end) -> sig val f : X.t foo -> X.t foo end
module Mk_S_b :
  functor (X : sig type t : bits64 end) ->
    sig module type S = sig val f : X.t foo -> X.t foo end end
Line 15, characters 2-5:
15 |   N.f
       ^^^
Error: The value "N.f" has type "M.t foo -> M.t foo"
       but an expression was expected of type "'a"
       This instance of "M.t" is ambiguous:
       it would escape the scope of its equation
|}]

(* CR layouts: Once we support [any] in tuples, the below will and should
   compile and can be deleted. *)

type ('a : any) is_value = V : ('a : value). 'a is_value

module Mv : sig
  type t : any
  val v : unit -> t
  val witness : t is_value
end = struct
  type t = string
  let v () = ""
  let witness = V
end

module Mk_St (X : sig type t val v : unit -> t end) = struct
  module type S = sig val x : X.t * X.t end
end
[%%expect {|
type ('a : any) is_value = V : 'a is_value
module Mv : sig type t : any val v : unit -> t val witness : t is_value end
module Mk_St :
  functor (X : sig type t val v : unit -> t end) ->
    sig module type S = sig val x : X.t * X.t end end
|}]

let tuple_direct =
  let V = Mv.witness in
  (Mv.v (), Mv.v ())
[%%expect {|
Line 3, characters 3-10:
3 |   (Mv.v (), Mv.v ())
       ^^^^^^^
Error: This expression has type "Mv.t" = "$0"
       but an expression was expected of type "'a"
       This instance of "$0" is ambiguous:
       it would escape the scope of its equation
|}]

let tuple_via_modtype_path =
  let V = Mv.witness in
  let module N : Mk_St(Mv).S = struct let x = Mv.v (), Mv.v () end in
  N.x
[%%expect {|
Line 4, characters 2-5:
4 |   N.x
      ^^^
Error: The value "N.x" has type "Mv.t * Mv.t"
       but an expression was expected of type "'a"
       This instance of "$0" is ambiguous:
       it would escape the scope of its equation
|}]
