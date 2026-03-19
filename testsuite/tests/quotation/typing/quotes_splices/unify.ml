(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on


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
[%%expect {|
module Inst0 : sig type t type t' end
module NonInst0 : sig type 'a t type 'a t' end
module Inst1 : sig type t type t' end
module NonInst1 : sig type 'a t type 'a t' end
module Inst2 : sig type t end
module NonInst2 : sig type 'a t end
|}]
#mark_toplevel_in_quotations


(** [unify]ing a variable under quotes/splices -- flexibility checks **)


(* Note that [generalize] type schemes have their quantified variables
   normalised to stage 0 (i.e. all of these would become ['a -> 'a]).
   This means in all tests with directly quoted variables we will eventually
   remove the quotes, as we cannot have top-level splices.
   However, unification should happen before this. *)

(* <['a]> ~ unit *)
let _ : <['a]> -> <['a]> = fun (() as x) -> x
[%%expect {|
- : unit -> unit = <fun>
|}]
(* <[<['a]>]> ~ unit *)
let _ : <[<['a]>]> -> <[<['a]>]> = fun (() as x) -> x
[%%expect {|
- : unit -> unit = <fun>
|}]
(* <[<[<['a]>]>]> ~ unit *)
let _ : <[<[<['a]>]>]> -> <[<[<['a]>]>]> = fun (() as x) -> x
[%%expect {|
- : unit -> unit = <fun>
|}]

(* In tests with spliced variables, the variable ['a] always occurs at stage 0,
   so we have no concerns about normalisation there. *)

(* $('a) ~ unit *)
let _ : <[ $('a) -> $('a) ]> expr  =
  <[fun (() as x) -> x]>
[%%expect {|
- : <[unit -> unit]> expr = <[fun () as x -> x]>
|}]
(* $($('a)) ~ unit *)
let _ : <[ <[ $($('a)) -> $($('a)) ]> expr ]> expr =
  <[<[fun (() as x) -> x]>]>
[%%expect {|
- : <[<[unit -> unit]> expr]> expr = <[<[fun () as x -> x]>]>
|}]
(* $($($('a))) ~ unit *)
let _ : <[ <[ <[ $($($('a))) -> $($($('a))) ]> expr ]> expr ]> expr =
  <[<[<[fun (() as x) -> x]>]>]>
[%%expect {|
- : <[<[<[unit -> unit]> expr]> expr]> expr = <[<[<[fun () as x -> x]>]>]>
|}]


(** [unify]ing with local type equations -- instantiability checks **)

(* One side instantiable and under quotes/splices *)

(* $t ~ s  when t instantiable *)
let _ = <[ fun (Equal : ($Inst1.t, int NonInst0.t) Type.eq)
               (x : Inst1.t expr) -> (x : <[int NonInst0.t]> expr) ]>
[%%expect {|
- : <[
     ($(Inst1.t), int NonInst0.t) Type.eq ->
     Inst1.t expr -> <[int NonInst0.t]> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (_, (int) NonInst0.t)
    Stdlib.Type.eq) (x : (Inst1.t) expr) -> (x : <[(int) NonInst0.t]> expr)
]>
|}]
(* t ~ $s  when s instantiable *)
let _ = <[ fun (Equal : (int NonInst0.t, $Inst1.t) Type.eq)
               (x : Inst1.t expr) -> (x : <[int NonInst0.t]> expr) ]>
[%%expect {|
- : <[
     (int NonInst0.t, $(Inst1.t)) Type.eq ->
     Inst1.t expr -> <[int NonInst0.t]> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : ((int) NonInst0.t, _)
    Stdlib.Type.eq) (x : (Inst1.t) expr) -> (x : <[(int) NonInst0.t]> expr)
]>
|}]
(* <[t]> ~ s  when t instantiable *)
let _ = <[ fun (Equal : (<[Inst0.t]>, int NonInst1.t) Type.eq)
               (x : Inst0.t) -> (x : $(int NonInst1.t)) ]>
[%%expect {|
- : <[(<[Inst0.t]>, int NonInst1.t) Type.eq -> Inst0.t -> $(int NonInst1.t)]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[Inst0.t]>, (int) NonInst1.t) Stdlib.Type.eq) (x : Inst0.t) -> (x : _)
]>
|}]
(* t ~ <[s]>  when s instantiable *)
let _ = <[ fun (Equal : (int NonInst1.t, <[Inst0.t]>) Type.eq)
               (x : Inst0.t) -> (x : $(int NonInst1.t)) ]>
[%%expect {|
- : <[(int NonInst1.t, <[Inst0.t]>) Type.eq -> Inst0.t -> $(int NonInst1.t)]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    ((int) NonInst1.t, <[Inst0.t]>) Stdlib.Type.eq) (x : Inst0.t) -> (x : _)
]>
|}]

(* Both sides instantiable and quotes/splices *)

(* $t ~ $s  and t, s instantiable *)
let _ = <[ fun (Equal : ($Inst1.t, $Inst1.t') Type.eq)
               (x : Inst1.t expr) -> (x : Inst1.t' expr) ]>
[%%expect {|
- : <[($(Inst1.t), $(Inst1.t')) Type.eq -> Inst1.t expr -> Inst1.t' expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (_, _) Stdlib.Type.eq)
    (x : (Inst1.t) expr) -> (x : (Inst1.t') expr)
]>
|}]
(* <[t]> ~ <[s]>  and t, s instantiable *)
let _ = <[ fun (Equal : (<[Inst0.t]>, <[Inst0.t']>) Type.eq)
               (x : <[Inst0.t]> expr) -> (x : <[Inst0.t']> expr) ]>
[%%expect {|
- : <[
     (<[Inst0.t]>, <[Inst0.t']>) Type.eq ->
     <[Inst0.t]> expr -> <[Inst0.t']> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[Inst0.t]>, <[Inst0.t']>) Stdlib.Type.eq) (x : <[Inst0.t]> expr) -> (x
    : <[Inst0.t']> expr)
]>
|}]
(* $t ~ <[s]>  and t, s instantiable *)
let _ = <[ fun (Equal : ($Inst2.t, <[Inst0.t']>) Type.eq)
               (x : <[Inst0.t']> expr) -> (x : $Inst2.t expr) ]>
[%%expect {|
- : <[
     ($(Inst2.t), <[Inst0.t']>) Type.eq ->
     <[Inst0.t']> expr -> $(Inst2.t) expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (_, <[Inst0.t']>)
    Stdlib.Type.eq) (x : <[Inst0.t']> expr) -> (x : _ expr)
]>
|}]
(* <[t]> ~ $s  and t, s instantiable *)
let _ = <[ fun (Equal : (<[Inst0.t']>, $Inst2.t) Type.eq)
               (x : <[Inst0.t']> expr) -> (x : $Inst2.t expr) ]>
[%%expect {|
- : <[
     (<[Inst0.t']>, $(Inst2.t)) Type.eq ->
     <[Inst0.t']> expr -> $(Inst2.t) expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (<[Inst0.t']>, _)
    Stdlib.Type.eq) (x : <[Inst0.t']> expr) -> (x : _ expr)
]>
|}]


(** Flexibility (linking) happens before instantiability (equations). **)

(* Introduces an anonymous universal variable,
   so that we can see if it gets equated. *)
let force_variables_equal_intro_left (x : 'a expr) (y : ('a, _) Type.eq) = ()
[%%expect {|
val force_variables_equal_intro_left : 'a expr -> ('a, 'b) Type.eq -> unit =
  <fun>
|}]
let force_variables_equal_intro_right (x : 'a expr) (y : (_, 'a) Type.eq) = ()
[%%expect {|
val force_variables_equal_intro_right : 'a expr -> ('b, 'a) Type.eq -> unit =
  <fun>
|}]

(* Quotes *)

(* <[t]> ~ s  links t to s when t flexible and s instantiable *)
let f (type a) (x : <[_]> expr) (eq : (<[_]>, a) Type.eq) =
  force_variables_equal_intro_left x eq;
  match eq with Equal -> x
[%%expect {|
val f : 'a expr -> ('a, 'a) Type.eq -> 'a expr = <fun>
|}]
(* <[t]> ~ s  equates s to t when s instantiable -- $0 escapes! *)
let f (type a) (x : <[_]> expr) (eq : (<[_ NonInst0.t]>, a) Type.eq) =
  force_variables_equal_intro_left x eq;
  match eq with Equal -> x
[%%expect {|
Line 3, characters 16-21:
3 |   match eq with Equal -> x
                    ^^^^^
Error: This pattern matches values of type
         "(<[$($0) NonInst0.t]>, <[$($0) NonInst0.t]>) Type.eq"
       but a pattern was expected which matches values of type
         "(<[$($0) NonInst0.t]>, a) Type.eq"
       The type constructor "$0" would escape its scope
|}]
(* s ~ <[t]>  links t to s when t flexible and s instantiable *)
let f (type a) (x : <[_]> expr) (eq : (a, <[_]>) Type.eq) =
  force_variables_equal_intro_right x eq;
  match eq with Equal -> x
[%%expect {|
val f : 'a expr -> ('a, 'a) Type.eq -> 'a expr = <fun>
|}]
(* s ~ <[t]>  equates s to t when s instantiable -- $0 escapes! *)
let f (type a) (x : <[_]> expr) (eq : (a, <[_ NonInst0.t]>) Type.eq) =
  force_variables_equal_intro_right x eq;
  match eq with Equal -> x
[%%expect {|
Line 3, characters 16-21:
3 |   match eq with Equal -> x
                    ^^^^^
Error: This pattern matches values of type "(a, a) Type.eq"
       but a pattern was expected which matches values of type
         "(a, <[$($0) NonInst0.t]>) Type.eq"
       The type constructor "$0" would escape its scope
|}]

(* Splices *)
(* Analogous to the quote tests, but everything needs to be quoted
   so that we can write a splice. *)

let force_variables_equal_intro_left' =
  <[ fun (x : 'a expr) (y : ('a, _) Type.eq) -> () ]>
[%%expect {|
val force_variables_equal_intro_left' :
  <[$('a) expr -> ($('a), $('b)) Type.eq -> unit]> expr =
  <[fun (x : 'a expr) (y : ('a, _) Stdlib.Type.eq) -> ()]>
|}]
let force_variables_equal_intro_right' =
  <[ fun (x : 'a expr) (y : (_, 'a) Type.eq) -> () ]>
[%%expect {|
val force_variables_equal_intro_right' :
  <[$('a) expr -> ($('b), $('a)) Type.eq -> unit]> expr =
  <[fun (x : 'a expr) (y : (_, 'a) Stdlib.Type.eq) -> ()]>
|}]

(* $t ~ s  links t to s when t flexible and s instantiable *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : ($(_), a) Type.eq) ->
      $force_variables_equal_intro_left' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
val f : <[$('a) expr -> ($('a), $('a)) Type.eq -> $('a) expr]> expr =
  <[
    fun (type a) (x : _ expr) (eq : (_, a) Stdlib.Type.eq) ->
      (fun (x__1 : 'a expr) (y : ('a, _) Stdlib.Type.eq) -> ()) x eq;
      match eq with | (Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) -> x
  ]>
|}]
(* $t ~ s  equates s to t when s instantiable -- $0 escapes! *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : ($(_ NonInst1.t), a) Type.eq) ->
      $force_variables_equal_intro_left' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
Line 4, characters 20-25:
4 |       match eq with Equal -> x
                        ^^^^^
Error: This pattern matches values of type
         "($(<[$0]> NonInst1.t), $(<[$0]> NonInst1.t)) Type.eq"
       but a pattern was expected which matches values of type
         "($(<[$0]> NonInst1.t), a) Type.eq"
       The type constructor "$0" would escape its scope
|}]
(* s ~ $t  links t to s when t flexible and s instantiable *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : (a, $(_)) Type.eq) ->
      $force_variables_equal_intro_right' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
val f : <[$('a) expr -> ($('a), $('a)) Type.eq -> $('a) expr]> expr =
  <[
    fun (type a) (x : _ expr) (eq : (a, _) Stdlib.Type.eq) ->
      (fun (x__1 : 'a expr) (y : (_, 'a) Stdlib.Type.eq) -> ()) x eq;
      match eq with | (Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) -> x
  ]>
|}]
(* s ~ $t  equates s to t when s instantiable -- $0 escapes! *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : (a, $(_ NonInst1.t)) Type.eq) ->
      $force_variables_equal_intro_right' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
Line 4, characters 20-25:
4 |       match eq with Equal -> x
                        ^^^^^
Error: This pattern matches values of type "(a, a) Type.eq"
       but a pattern was expected which matches values of type
         "(a, $(<[$0]> NonInst1.t)) Type.eq"
       The type constructor "$0" would escape its scope
|}]
