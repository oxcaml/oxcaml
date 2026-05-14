(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on


(** Compatibility checks for [mcomp] **)

(* Since [mcomp] is allowed to over-approximate, we need only test
   that things are allowed to pass. *)

(* [Inst*.t] is instantiable as it is an abstract data type with no parameters,
   while [_ NonInst*.t] is non-instantiable as it has a parameter.
   They are also not non-aliasable since they live inside submodules. *)
module Inst0 : sig
  type t
  type t'
end = struct
  type t  = int -> int
  type t' = int -> int
end
module NonInst0 : sig
  type 'a t
  type 'a t'
end = struct
  type 'a t  = int -> int
  type 'a t' = int -> int
end
(* CR quoted-kinds jbachurski: Annotate these with quoted kinds *)
module Inst1 : sig
  type t
  type t'
end = struct
  type t  = <[int -> int]>
  type t' = <[int -> int]>
end
module NonInst1 : sig
  type 'a t
  type 'a t'
end = struct
  type 'a t  = <[int -> int]>
  type 'a t' = <[int -> int]>
end
module Inst2 : sig
  type t
end = struct
  type t = <[int -> int]>
end
module NonInst2 : sig
  type 'a t
end = struct
  type 'a t = <[int -> int]>
end
(* We wrap some constructs in [A.t] so that they are checked
   in [mcomp], but not [unify]. *)
(* CR quoted-kinds jbachurski: We might need extra versions of A
   with 'a quoted-kinded. *)
module A = struct
  type _ t  = |
  type _ t' = |
end
[%%expect {|
module Inst0 : sig type t type t' end
module NonInst0 : sig type 'a t type 'a t' end
module Inst1 : sig type t type t' end
module NonInst1 : sig type 'a t type 'a t' end
module Inst2 : sig type t end
module NonInst2 : sig type 'a t end
module A : sig type _ t = | type _ t' = | end
|}]
#mark_toplevel_in_quotations

(* Flexible: type variables under quotes/splices *)

(* [mcomp] gets called on these field descriptions.
   Of these, [f]s have flexible subterms, while the corresponding subterm
   in [r] is non-aliasable. [1] and [0] stand for whether it is spliced. *)
module F = struct
  type 'a f1  = { x : <[ $( 'a)  -> unit]> expr }
  type 'b f1' = { x : <[ $( 'b)  -> unit]> expr }
  type  _ r0  = { x : <[   int   -> unit]> expr }
  type  _ r1  = { x : <[ $(int)  -> unit]> expr }
end
(* CR-someday jbachurski: A corresponding [F] with quotes might be constructible,
   but I don't know how and I believe it might be impossible. *)
[%%expect{|
module F :
  sig
    type 'a f1 = { x : <[$('a) -> unit]> expr; }
    type 'b f1' = { x : <[$('b) -> unit]> expr; }
    type _ r0 = { x : <[int -> unit]> expr; }
    type _ r1 = { x : <[$(int) -> unit]> expr; }
  end
|}]
#mark_toplevel_in_quotations

(* One side flexible and under splices *)
(* $t ~ s  when t flexible *)
let _ = <[ fun (Equal : (int F.f1, int F.r0) Type.eq) -> () ]>
[%%expect{|
- : <[(int F.f1, int F.r0) Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    ((int) F.f1, (int) F.r0) Stdlib.Type.eq) -> ()
]>
|}]
(* s ~ $t  when t flexible *)
let _ = <[ fun (Equal : (int F.r0, int F.f1) Type.eq) -> () ]>
[%%expect{|
- : <[(int F.r0, int F.f1) Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    ((int) F.r0, (int) F.f1) Stdlib.Type.eq) -> ()
]>
|}]
(* One side flexible, both under splices *)
(* $t ~ s  when t flexible *)
let _ = <[ fun (Equal : (int F.f1, int F.r1) Type.eq) -> () ]>
[%%expect{|
- : <[(int F.f1, int F.r1) Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    ((int) F.f1, (int) F.r1) Stdlib.Type.eq) -> ()
]>
|}]
(* s ~ $t  when t flexible *)
let _ = <[ fun (Equal : (int F.r1, int F.f1) Type.eq) -> () ]>
[%%expect{|
- : <[(int F.r1, int F.f1) Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    ((int) F.r1, (int) F.f1) Stdlib.Type.eq) -> ()
]>
|}]
(* Both sides flexible and under splices *)
let _ = <[ fun (Equal : (int F.f1, int F.f1') Type.eq) -> ()]>
[%%expect{|
- : <[(int F.f1, int F.f1') Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    ((int) F.f1, (int) F.f1') Stdlib.Type.eq) -> ()
]>
|}]

(* Aliasable: [non_aliasable] abstract types under quotes/splices *)

(* One aliasable under quotes/splices *)
let e = <[ fun (Equal : ($(int NonInst1.t), int -> int) Type.eq) -> () ]>
[%%expect{|
val e : <[($(int NonInst1.t), int -> int) Type.eq -> unit]> expr =
  <[
    fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (_, int -> int)
      Stdlib.Type.eq) -> ()
  ]>
|}]
let e = <[ fun (Equal : (int -> int, $(int NonInst1.t)) Type.eq) -> () ]>
[%%expect{|
val e : <[(int -> int, $(int NonInst1.t)) Type.eq -> unit]> expr =
  <[
    fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (int -> int, _)
      Stdlib.Type.eq) -> ()
  ]>
|}]
(* CR quoted-kinds jbachurski: These should fail with stage-kind errors. *)
let e = <[ fun (Equal : (<[int NonInst0.t]>, int -> int) Type.eq) -> () ]>
[%%expect{|
val e : <[(<[int NonInst0.t]>, int -> int) Type.eq -> unit]> expr =
  <[
    fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
      (<[(int) NonInst0.t]>, int -> int) Stdlib.Type.eq) -> ()
  ]>
|}]
let e = <[ fun (Equal : (int -> int, <[int NonInst0.t]>) Type.eq) -> () ]>
[%%expect{|
val e : <[(int -> int, <[int NonInst0.t]>) Type.eq -> unit]> expr =
  <[
    fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
      (int -> int, <[(int) NonInst0.t]>) Stdlib.Type.eq) -> ()
  ]>
|}]
(* Both aliasable under quotes/splices *)
let _ = <[ fun (Equal : ($(int NonInst2.t), <[int NonInst0.t]>) Type.eq) -> () ]>
[%%expect{|
- : <[($(int NonInst2.t), <[int NonInst0.t]>) Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (_, <[(int) NonInst0.t]>) Stdlib.Type.eq) -> ()
]>
|}]
let _ = <[ fun (Equal : (<[int NonInst0.t]>, $(int NonInst2.t)) Type.eq) -> () ]>
[%%expect{|
- : <[(<[int NonInst0.t]>, $(int NonInst2.t)) Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[(int) NonInst0.t]>, _) Stdlib.Type.eq) -> ()
]>
|}]
let _ = <[ fun (Equal : ($(int NonInst1.t) A.t, $(int NonInst1.t') A.t') Type.eq) -> () ]>
[%%expect{|
- : <[($(int NonInst1.t) A.t, $(int NonInst1.t') A.t') Type.eq -> unit]> expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (_ A.t, _ A.t')
    Stdlib.Type.eq) -> ()
]>
|}]
let _ = <[ fun (Equal : (<[int NonInst0.t]> A.t, <[int NonInst0.t']> A.t') Type.eq) -> () ]>
[%%expect{|
- : <[(<[int NonInst0.t]> A.t, <[int NonInst0.t']> A.t') Type.eq -> unit]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[(int) NonInst0.t]> A.t, <[(int) NonInst0.t']> A.t') Stdlib.Type.eq) ->
    ()
]>
|}]

(* Rigid: just comparing types with quotes *)

let _ = <[ fun (Equal : (<[int]> A.t, <[int]> A.t') Type.eq) -> () ]>
[%%expect{|
- : <[(<[int]> A.t, <[int]> A.t') Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[int]> A.t, <[int]> A.t') Stdlib.Type.eq) -> ()
]>
|}]
let _ = <[ fun (Equal : (<[<[int]>]> A.t, <[<[int]>]> A.t') Type.eq) -> () ]>
[%%expect{|
- : <[(<[<[int]>]> A.t, <[<[int]>]> A.t') Type.eq -> unit]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[<[int]>]> A.t, <[<[int]>]> A.t') Stdlib.Type.eq) -> ()
]>
|}]
(* Sanity check that these error with non-aliasable types *)
let _ = <[ fun (Equal : (int A.t, <[int]> A.t') Type.eq) -> () ]>
[%%expect{|
Line 1, characters 16-21:
1 | let _ = <[ fun (Equal : (int A.t, <[int]> A.t') Type.eq) -> () ]>
                    ^^^^^
Error: This pattern matches values of type "(int A.t, int A.t) Type.eq"
       but a pattern was expected which matches values of type
         "(int A.t, <[int]> A.t') Type.eq"
       Type "int A.t" is not compatible with type "<[int]> A.t'"
|}]
let _ = <[ fun (Equal : (<[int]> A.t, int A.t') Type.eq) -> () ]>
[%%expect{|
Line 1, characters 16-21:
1 | let _ = <[ fun (Equal : (<[int]> A.t, int A.t') Type.eq) -> () ]>
                    ^^^^^
Error: This pattern matches values of type "(<[int]> A.t, <[int]> A.t) Type.eq"
       but a pattern was expected which matches values of type
         "(<[int]> A.t, int A.t') Type.eq"
       Type "<[int]> A.t" is not compatible with type "int A.t'"
|}]
let _ = <[ fun (Equal : (<[<[int]>]> A.t, <[int]> A.t') Type.eq) -> () ]>
[%%expect{|
Line 1, characters 16-21:
1 | let _ = <[ fun (Equal : (<[<[int]>]> A.t, <[int]> A.t') Type.eq) -> () ]>
                    ^^^^^
Error: This pattern matches values of type
         "(<[<[int]>]> A.t, <[<[int]>]> A.t) Type.eq"
       but a pattern was expected which matches values of type
         "(<[<[int]>]> A.t, <[int]> A.t') Type.eq"
       Type "<[<[int]>]> A.t" is not compatible with type "<[int]> A.t'"
|}]
let _ = <[ fun (Equal : (<[int]> A.t, <[<[int]>]> A.t') Type.eq) -> () ]>
[%%expect{|
Line 1, characters 16-21:
1 | let _ = <[ fun (Equal : (<[int]> A.t, <[<[int]>]> A.t') Type.eq) -> () ]>
                    ^^^^^
Error: This pattern matches values of type "(<[int]> A.t, <[int]> A.t) Type.eq"
       but a pattern was expected which matches values of type
         "(<[int]> A.t, <[<[int]>]> A.t') Type.eq"
       Type "<[int]> A.t" is not compatible with type "<[<[int]>]> A.t'"
|}]
