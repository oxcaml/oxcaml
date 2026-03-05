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
module Inst1 : sig
  type t  : <[value]>
  type t' : <[value]>
end = struct
  type t  = <[int -> int]>
  type t' = <[int -> int]>
end
module NonInst1 : sig
  type 'a t  : <[value]>
  type 'a t' : <[value]>
end = struct
  type 'a t  = <[int -> int]>
  type 'a t' = <[int -> int]>
end
module Inst2 : sig
  type t : <[<[value]>]>
end = struct
  type t = <[<[int -> int]>]>
end
module NonInst2 : sig
  type 'a t : <[<[value]>]>
end = struct
  type 'a t = <[<[int -> int]>]>
end
[%%expect {|
module Inst0 : sig type t type t' end
module NonInst0 : sig type 'a t type 'a t' end
module Inst1 : sig type t : <[value]> type t' : <[value]> end
module NonInst1 : sig type 'a t : <[value]> type 'a t' : <[value]> end
module Inst2 : sig type t : <[<[value]>]> end
module NonInst2 : sig type 'a t : <[<[value]>]> end
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
Line 11, characters 27-45:
11 | let _ : <['a]> -> <['a]> = fun (() as x) -> x
                                ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of <['a]> is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <['a]> must be representable
         because we must know concretely how to pass a function argument.
|}]
(* <[<['a]>]> ~ unit *)
let _ : <[<['a]>]> -> <[<['a]>]> = fun (() as x) -> x
[%%expect {|
Line 1, characters 35-53:
1 | let _ : <[<['a]>]> -> <[<['a]>]> = fun (() as x) -> x
                                       ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of <[<['a]>]> is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <[<['a]>]> must be representable
         because we must know concretely how to pass a function argument.
|}]
(* <[<[<['a]>]>]> ~ unit *)
let _ : <[<[<['a]>]>]> -> <[<[<['a]>]>]> = fun (() as x) -> x
[%%expect {|
Line 1, characters 43-61:
1 | let _ : <[<[<['a]>]>]> -> <[<[<['a]>]>]> = fun (() as x) -> x
                                               ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of <[<[<['a]>]>]> is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <[<[<['a]>]>]> must be representable
         because we must know concretely how to pass a function argument.
|}]

(* In tests with spliced variables, the variable ['a] always occurs at stage 0,
   so we have no concerns about normalisation there. *)

(* $('a) ~ unit *)
let _ : <[ $('a) -> $('a) ]> expr  =
  <[fun (() as x) -> x]>
[%%expect {|
Line 2, characters 4-22:
2 |   <[fun (() as x) -> x]>
        ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of $('a) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be representable
         because we must know concretely how to pass a function argument.
|}]
(* $($('a)) ~ unit *)
let _ : <[ <[ $($('a)) -> $($('a)) ]> expr ]> expr =
  <[<[fun (() as x) -> x]>]>
[%%expect {|
Line 2, characters 6-24:
2 |   <[<[fun (() as x) -> x]>]>
          ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of $($('a)) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $($('a)) must be representable
         because we must know concretely how to pass a function argument.
|}]
(* $($($('a))) ~ unit *)
let _ : <[ <[ <[ $($($('a))) -> $($($('a))) ]> expr ]> expr ]> expr =
  <[<[<[fun (() as x) -> x]>]>]>
[%%expect {|
Line 2, characters 8-26:
2 |   <[<[<[fun (() as x) -> x]>]>]>
            ^^^^^^^^^^^^^^^^^^
Error: Function arguments and returns must be representable.
       The layout of $($($('a))) is any
         because it's assigned a dummy kind that should have been overwritten.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $($($('a))) must be representable
         because we must know concretely how to pass a function argument.
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
Line 1, characters 25-36:
1 | let _ = <[ fun (Equal : (<[Inst0.t]>, int NonInst1.t) Type.eq)
                             ^^^^^^^^^^^
Error: This type "<[Inst0.t]>" should be an instance of type
         "('a : value_or_null)"
       The kind of <[Inst0.t]> is <[value]>
         because of the definition of t at line 2, characters 2-8.
       But the kind of <[Inst0.t]> must be a subkind of value_or_null
         because the 1st type argument of Type.eq has this kind.
|}]
(* t ~ <[s]>  when s instantiable *)
let _ = <[ fun (Equal : (int NonInst1.t, <[Inst0.t]>) Type.eq)
               (x : Inst0.t) -> (x : $(int NonInst1.t)) ]>
[%%expect {|
Line 1, characters 25-39:
1 | let _ = <[ fun (Equal : (int NonInst1.t, <[Inst0.t]>) Type.eq)
                             ^^^^^^^^^^^^^^
Error: This type "int NonInst1.t" should be an instance of type
         "('a : value_or_null)"
       The kind of int NonInst1.t is <[value]>
         because of the definition of t at line 23, characters 2-24.
       But the kind of int NonInst1.t must be a subkind of value_or_null
         because the 1st type argument of Type.eq has this kind.
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
Line 1, characters 25-36:
1 | let _ = <[ fun (Equal : (<[Inst0.t]>, <[Inst0.t']>) Type.eq)
                             ^^^^^^^^^^^
Error: This type "<[Inst0.t]>" should be an instance of type
         "('a : value_or_null)"
       The kind of <[Inst0.t]> is <[value]>
         because of the definition of t at line 2, characters 2-8.
       But the kind of <[Inst0.t]> must be a subkind of value_or_null
         because the 1st type argument of Type.eq has this kind.
|}]
(* $t ~ <[s]>  and t, s instantiable *)
let _ = <[ fun (Equal : ($Inst2.t, <[Inst0.t']>) Type.eq)
               (x : <[Inst0.t']> expr) -> (x : $Inst2.t expr) ]>
[%%expect {|
Line 1, characters 25-33:
1 | let _ = <[ fun (Equal : ($Inst2.t, <[Inst0.t']>) Type.eq)
                             ^^^^^^^^
Error: This type "$(Inst2.t)" should be an instance of type
         "('a : value_or_null)"
       The kind of $(Inst2.t) is <[value]>
         because of the definition of t at line 30, characters 2-24.
       But the kind of $(Inst2.t) must be a subkind of value_or_null
         because the 1st type argument of Type.eq has this kind.
|}]
(* <[t]> ~ $s  and t, s instantiable *)
let _ = <[ fun (Equal : (<[Inst0.t']>, $Inst2.t) Type.eq)
               (x : <[Inst0.t']> expr) -> (x : $Inst2.t expr) ]>
[%%expect {|
Line 1, characters 25-37:
1 | let _ = <[ fun (Equal : (<[Inst0.t']>, $Inst2.t) Type.eq)
                             ^^^^^^^^^^^^
Error: This type "<[Inst0.t']>" should be an instance of type
         "('a : value_or_null)"
       The kind of <[Inst0.t']> is <[value]>
         because of the definition of t' at line 3, characters 2-9.
       But the kind of <[Inst0.t']> must be a subkind of value_or_null
         because the 1st type argument of Type.eq has this kind.
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
Line 1, characters 39-44:
1 | let f (type a) (x : <[_]> expr) (eq : (<[_]>, a) Type.eq) =
                                           ^^^^^
Error: This type "<['a]>" should be an instance of type "('b : value_or_null)"
       The layout of <['a]> is any
         because there's a _ in the type.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <['a]> must be a sublayout of value
         because the 1st type argument of Type.eq has this layout.
|}]
(* <[t]> ~ s  equates s to t when s instantiable -- $0 escapes! *)
let f (type a) (x : <[_]> expr) (eq : (<[_ NonInst0.t]>, a) Type.eq) =
  force_variables_equal_intro_left x eq;
  match eq with Equal -> x
[%%expect {|
Line 1, characters 39-55:
1 | let f (type a) (x : <[_]> expr) (eq : (<[_ NonInst0.t]>, a) Type.eq) =
                                           ^^^^^^^^^^^^^^^^
Error: This type "<['a NonInst0.t]>" should be an instance of type
         "('b : value_or_null)"
       The kind of <['a NonInst0.t]> is <[value]>
         because of the definition of t at line 9, characters 2-11.
       But the kind of <['a NonInst0.t]> must be a subkind of value_or_null
         because the 1st type argument of Type.eq has this kind.
|}]
(* s ~ <[t]>  links t to s when t flexible and s instantiable *)
let f (type a) (x : <[_]> expr) (eq : (a, <[_]>) Type.eq) =
  force_variables_equal_intro_right x eq;
  match eq with Equal -> x
[%%expect {|
Line 1, characters 42-47:
1 | let f (type a) (x : <[_]> expr) (eq : (a, <[_]>) Type.eq) =
                                              ^^^^^
Error: This type "<['a]>" should be an instance of type "('b : value_or_null)"
       The layout of <['a]> is any
         because there's a _ in the type.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of <['a]> must be a sublayout of value
         because the 2nd type argument of Type.eq has this layout.
|}]
(* s ~ <[t]>  equates s to t when s instantiable -- $0 escapes! *)
let f (type a) (x : <[_]> expr) (eq : (a, <[_ NonInst0.t]>) Type.eq) =
  force_variables_equal_intro_right x eq;
  match eq with Equal -> x
[%%expect {|
Line 1, characters 42-58:
1 | let f (type a) (x : <[_]> expr) (eq : (a, <[_ NonInst0.t]>) Type.eq) =
                                              ^^^^^^^^^^^^^^^^
Error: This type "<['a NonInst0.t]>" should be an instance of type
         "('b : value_or_null)"
       The kind of <['a NonInst0.t]> is <[value]>
         because of the definition of t at line 9, characters 2-11.
       But the kind of <['a NonInst0.t]> must be a subkind of value_or_null
         because the 2nd type argument of Type.eq has this kind.
|}]

(* Splices *)
(* Analogous to the quote tests, but everything needs to be quoted
   so that we can write a splice. *)

let force_variables_equal_intro_left' =
  <[ fun (x : 'a expr) (y : ('a, _) Type.eq) -> () ]>
[%%expect {|
val force_variables_equal_intro_left' :
  ('a : <[value_or_null]>) ('b : <[value_or_null]>).
    <[$('a) expr -> ($('a), $('b)) Type.eq -> unit]> expr =
  <[fun (x : 'a expr) (y : ('a, _) Stdlib.Type.eq) -> ()]>
|}]
let force_variables_equal_intro_right' =
  <[ fun (x : 'a expr) (y : (_, 'a) Type.eq) -> () ]>
[%%expect {|
val force_variables_equal_intro_right' :
  ('a : <[value_or_null]>) ('b : <[value_or_null]>).
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
Line 2, characters 40-44:
2 |     fun (type a) (x : $(_) expr) (eq : ($(_), a) Type.eq) ->
                                            ^^^^
Error: This type "$('a)" should be an instance of type "('b : value_or_null)"
       The layout of $('a) is any
         because there's a _ in the type.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be a sublayout of value
         because the 1st type argument of Type.eq has this layout.
|}]
(* $t ~ s  equates s to t when s instantiable -- $0 escapes! *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : ($(_ NonInst1.t), a) Type.eq) ->
      $force_variables_equal_intro_left' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
Uncaught exception: Invalid_argument("option is None")

|}]
(* s ~ $t  links t to s when t flexible and s instantiable *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : (a, $(_)) Type.eq) ->
      $force_variables_equal_intro_right' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
Line 2, characters 43-47:
2 |     fun (type a) (x : $(_) expr) (eq : (a, $(_)) Type.eq) ->
                                               ^^^^
Error: This type "$('a)" should be an instance of type "('b : value_or_null)"
       The layout of $('a) is any
         because there's a _ in the type.
                 Please notify the Jane Street compilers group if you see this output.
       But the layout of $('a) must be a sublayout of value
         because the 2nd type argument of Type.eq has this layout.
|}]
(* s ~ $t  equates s to t when s instantiable -- $0 escapes! *)
let f = <[
    fun (type a) (x : $(_) expr) (eq : (a, $(_ NonInst1.t)) Type.eq) ->
      $force_variables_equal_intro_right' x eq;
      match eq with Equal -> x
  ]>
[%%expect {|
Uncaught exception: Invalid_argument("option is None")

|}]
