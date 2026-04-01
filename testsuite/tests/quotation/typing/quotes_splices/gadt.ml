(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Helpers *)

module M = struct
  type t
  type t'
end
[%%expect {|
module M : sig type t type t' end
|}]
#mark_toplevel_in_quotations

let sorry0 =       fun (f : _ Type.eq -> _) -> f (Obj.magic Type.Equal)
let sorry1 =    <[ fun (f : _ Type.eq -> _) -> f (Obj.magic Type.Equal) ]>
let sorry2 = <[ <[ fun (f : _ Type.eq -> _) -> f (Obj.magic Type.Equal) ]> ]>
[%%expect {|
val sorry0 : (('a, 'b) Type.eq -> 'c) -> 'c = <fun>
val sorry1 : <[(($('a), $('b)) Type.eq -> $('c)) -> $('c)]> expr =
  <[
    fun (f : (_, _) Stdlib.Type.eq -> _) ->
      f (Stdlib.Obj.magic Stdlib__Type.Equal)
  ]>
val sorry2 :
  <[<[(($($('a)), $($('b))) Type.eq -> $($('c))) -> $($('c))]> expr]> expr =
  <[
    <[
      fun (f : (_, _) Stdlib.Type.eq -> _) ->
        f (Stdlib.Obj.magic Stdlib__Type.Equal)
      ]>
  ]>
|}]


(** Sanity-check that we can use reasonable GADTs with staging **)

(* Basic typed language -- note variable stages are normalized *)
type _ t =
  | Int : int -> <[int]> t
  | Add : <[int]> t * <[int]> t -> <[int]> t
  | Is_zero : <[int]> t -> <[bool]> t
  | Bool : bool -> <[bool]> t
  | If : <[bool]> t * 'a t * 'a t -> 'a t
  | Pair : <['a]> t * <['b]> t -> <['a * 'b]> t
  | First : <['a * 'b]> t -> <['a]> t
  | Second : <[$('a) * $('b)]> t -> <[$('b)]> t
[%%expect {|
type _ t =
    Int : int -> <[int]> t
  | Add : <[int]> t * <[int]> t -> <[int]> t
  | Is_zero : <[int]> t -> <[bool]> t
  | Bool : bool -> <[bool]> t
  | If : <[bool]> t * 'a t * 'a t -> 'a t
  | Pair : 'a t * 'b t -> <[$('a) * $('b)]> t
  | First : <[$('a) * $('b)]> t -> 'a t
  | Second : <[$('a) * $('b)]> t -> 'b t
|}]

(* Compile tracking types under expressions *)
let rec compile : type a. a t -> a expr = function
  | Int i -> Quote.Expr.int i
  | Add (e, e') -> <[ $(compile e) + $(compile e') ]>
  | Is_zero e -> <[ Int.equal $(compile e) 0 ]>
  | Bool b -> Quote.Expr.bool b
  | If (c, t, f) -> <[ if $(compile c) then $(compile t) else $(compile f) ]>
  | Pair (e, e') -> <[ $(compile e), $(compile e') ]>
  | First e -> <[ fst $(compile e) ]>
  | Second e -> <[ snd $(compile e) ]>
[%%expect {|
val compile : 'a t -> 'a expr = <fun>
|}];;

compile (
  If (If (Is_zero (Add (Int 3, Int (-3))), Bool false, Bool true),
      Pair (Int 1, Bool true), Pair (Int 0, Bool false)))
[%%expect {|
- : <[int * bool]> expr =
<[
  if (if (Stdlib.Int.equal (3 + (-3)) 0) then false else true) then (1, true)
  else (0, false)
]>
|}]

(* Abstract interpretation; check refutations work *)
let rec maybe_non_negative : <[int]> t -> bool = function
  | Int i -> i >= 0
  | Add (e, e') -> maybe_non_negative e && maybe_non_negative e'
  | If (c, t, f) -> maybe_non_negative t && maybe_non_negative f
  | First e -> true
  | Second e -> true

[%%expect {|
val maybe_non_negative : <[int]> t -> bool = <fun>
|}]


(** Local constraints travelling across stages **)

(* In these tests, we consider passing a *proof* (a [Type.eq]) that [t = s]
   betweeen stages. We check this by passing a *subject* [x : t]
   and attempting to coerce it to [s]. We call [t] the *target*.

   There are six interesting variables:
   * [S_proof]: The stage of the *proof* value.
   * [T_proof]: The stage of the *target* in the *proof*'s equation.
   * [S_subj]:  The stage at which the *proof* was introduced.
   * [S_min]:   The earliest stage we entered since the *proof* was introduced.
   * [S_max]:   The latemost stage we entered since the *proof* was introduced.
   * [T_subj]:  The stage of the *target* in the *subject*'s type.

   Note [S_min] is upper-bounded by the stage of the *subject* value
   [S_subj >= S_min], at which we instantiate the proof.
   Similarly to [S_min], we have the latest stage entered [S_subj <= S_max],
   Some tests sanity-check this value does not influence results.

   We have these constraints for GADT inference to be sound with staging
   and mechanisms that ensure them:
   * [T_proof = T_subj]: Equations are only applicable at a fixed stage.
     For example, [t = s] at stage 1 <=/=> [t = s] at stage 0.
     We can only say that [t = s] at stage 1 <=> <[t]> = <[s]> at stage 0.
     Mechanism: Local constraints track a stage.
   * [S_proof <= S_min]: Proofs cannot time travel (go to the past).
     If a proof only exists at stage [n], we must only use it at [m >= n].
     Mechanism: Unification tracks initial stage.
   * [S_proof <= T_proof]: Proofs cannot mention types that might not exist
     anymore. For example, [$t = int] at stage 1 will not make sense after
     stage 0, since $t no longer exists and should not be instantiable.
     Note that a related problem is that some splice types are erased.
     Mechanism: Types before the pattern's stage are not instantiable.

   We annotate tests in the format [S_proof ~~> S_min @ T_proof <=> T_subj].

   In cases when it's significant that [S_min =/= S_subj] or [S_max =/= S_subj],
   we extend the notation to [S_proof ~~> x ~~> S_subj] for x in {S_min, S_max}.

   For [t] we use either a locally abstract type or the top-level [M.t],
   and for [s] we usually use a simple top-level type: [int] or [bool]. *)

(* Evidence stays in the same stage -- should always succeed:
   [S_proof = S_min] and [T_proof = T_subj] *)

(* 0 ~~> 1 ~~> 0  @  0 <=> 0 *)
let _ = fun (type t) (Equal : (t, int) Type.eq) (x : t) ->
    <[ $(Quote.Expr.int (x + 1)) * 2 ]>
[%%expect {|
- : ('t, int) Type.eq -> 't -> <[int]> expr = <fun>
|}]
(* 1 ~~> 0 ~~> 1  @  1 <=> 1 *)
let _ = <[
  fun (type t) (x : t) -> $(
    (fun (Equal : (<[t]> expr, <[int]> expr) Type.eq) -> <[ x + 1 ]>) |> ignore;
    <[()]>) ]>
[%%expect {|
- : <[$('t) -> unit]> expr = <[fun (type t) (x : t) -> ()]>
|}]
(* 1 ~~> 2 ~~> 1  @  1 <=> 1 *)
let _ = <[ fun (type t) (Equal : (t, int) Type.eq) (x : t) ->
    <[ $(Quote.Expr.int (x + 1)) * 2 ]> ]>
[%%expect {|
- : <[($('t), int) Type.eq -> $('t) -> <[int]> expr]> expr =
<[
  fun (type t) ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (t, int)
    Stdlib.Type.eq) (x : t) -> <[($(Stdlib.Quote.Expr.int (x + 1))) * 2]>
]>
|}]
(* 1 ~~> 2 ~~> 1  @ 2 <=> 2 *)
let _ = <[ <[
  fun (type t) (x : t) -> $(
    (fun (Equal : (<[t]> expr, <[int]> expr) Type.eq) -> <[ x + 1 ]>) |> ignore;
    <[()]>) ]> ]>
[%%expect {|
- : <[<[$($('t)) -> unit]> expr]> expr =
<[
  <[
    fun (type t) (x : t) ->
      $
        (Stdlib.ignore
           (fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
              (<[t]> expr, <[int]> expr) Stdlib.Type.eq) -> <[x + 1]>);
         <[()]>)
    ]>
]>
|}]
(* 2 ~~> 3 ~~> 2  @  2 <=> 2 *)
let _ = <[ <[ fun (type t) (Equal : (t, int) Type.eq) (x : t) ->
  <[ $(Quote.Expr.int (x + 1)) * 2 ]> ]> ]>
[%%expect {|
- : <[<[($($('t)), int) Type.eq -> $($('t)) -> <[int]> expr]> expr]> expr =
<[
  <[
    fun (type t) ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (t, int)
      Stdlib.Type.eq) (x : t) -> <[($(Stdlib.Quote.Expr.int (x + 1))) * 2]>
    ]>
]>
|}]
(* 0 ~~> 2 ~~> 0  @  0 <=> 0 *)
let _ = fun (type t) (Equal : (t, bool) Type.eq) (x : t) ->
    <[ <[ $($(
      match x with
      | true -> <[<[true]>]>
      | false -> <[<[false]>]>))
      |> not ]> ]>
[%%expect {|
- : ('t, bool) Type.eq -> 't -> <[<[bool]> expr]> expr = <fun>
|}]
(* 0 ~~> 2 ~~> 0  @  1 <=> 1 *)
let _ = fun (Equal : (<[M.t]> expr, <[bool]> expr) Type.eq) (x : <[M.t]> expr) ->
    <[ <[ $(
      match $x with
      | true -> <[true]>
      | false -> <[false]>)
      |> not ]> ]>
[%%expect {|
- : (<[M.t]> expr, <[bool]> expr) Type.eq ->
    <[M.t]> expr -> <[<[bool]> expr]> expr
= <fun>
|}]
(* 1 ~~> 3 ~~> 1  @  1 <=> 1  with locally abstract [t] *)
let _ = <[ fun (type t) (Equal : (t, bool) Type.eq) (x : t) ->
    <[ <[ $($(
      match x with
      | true -> <[<[true]>]>
      | false -> <[<[false]>]>))
      |> not ]> ]> ]>
[%%expect {|
- : <[($('t), bool) Type.eq -> $('t) -> <[<[bool]> expr]> expr]> expr =
<[
  fun (type t) ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (t, bool)
    Stdlib.Type.eq) (x : t) ->
    <[
      <[
        Stdlib.not
          ($($(match x with | true -> <[<[true]>]> | false -> <[<[false]>]>)))
        ]>
      ]>
]>
|}]
(* 1 ~~> 3 ~~> 1  @  1 <=> 1  with top-level [M.t] *)
let _ = <[ fun (Equal : (M.t, bool) Type.eq) (x : M.t) ->
    <[ <[ $($(
      match x with
      | true -> <[<[true]>]>
      | false -> <[<[false]>]>))
      |> not ]> ]> ]>
[%%expect {|
- : <[(M.t, bool) Type.eq -> M.t -> <[<[bool]> expr]> expr]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (M.t, bool)
    Stdlib.Type.eq) (x : M.t) ->
    <[
      <[
        Stdlib.not
          ($($(match x with | true -> <[<[true]>]> | false -> <[<[false]>]>)))
        ]>
      ]>
]>
|}]

(* 0 ~~> 0  @  1 <=> 1 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : <[M.t]> expr) -> <[ $x + 1 ]>
[%%expect {|
- : (<[M.t]> expr, <[int]> expr) Type.eq -> <[M.t]> expr -> <[int]> expr =
<fun>
|}]
(* 0 ~~> 0  @  2 <=> 2 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq)
            (x : <[<[M.t]> expr]> expr) -> <[ <[ $($x) + 1 ]> ]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[<[M.t]> expr]> expr -> <[<[int]> expr]> expr
= <fun>
|}]
(* 1 ~~> 1  @  2 <=> 2 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq)
               (x : <[M.t]> expr) -> <[ $x + 1 ]> ]>
[%%expect {|
- : <[(<[M.t]> expr, <[int]> expr) Type.eq -> <[M.t]> expr -> <[int]> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) (x : <[M.t]> expr) ->
    <[($x) + 1]>
]>
|}]

(* CR metaprogramming jbachurski: Error messages are often confusing here,
   as the expansion inside presented in the error is from a specific stage.
   See ticket 6726. *)

(* Evidence stays in the same *wrong* stage -- should always fail:
   [S_proof = S_min] and [T_proof =/= T_subj] *)

(* CR metaprogramming jbachurski: Tests succeed until constraints are staged. *)

(* 0 ~~> 0  @  1 <=> 0 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq)
            (x : M.t) -> x + 1
[%%expect{|
- : (<[M.t]> expr, <[int]> expr) Type.eq -> M.t -> int = <fun>
|}]

(* 1 ~~> 2 ~~> 1  @  1 <=> 0 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) (x : M.t) ->
    <[ $(Quote.Expr.int (x + 1)) * 2 ]> ]>
[%%expect {|
- : <[(<[M.t]> expr, <[int]> expr) Type.eq -> M.t -> <[int]> expr]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) (x : M.t) ->
    <[($(Stdlib.Quote.Expr.int (x + 1))) * 2]>
]>
|}]
(* 1 ~~> 2 ~~> 1  @  0 <=> 1 *)
let _ = <[ fun (Equal : (M.t, int) Type.eq) (x : <[M.t]> expr) ->
    <[ $x + 1 ]> ]>
[%%expect {|
- : <[(M.t, int) Type.eq -> <[M.t]> expr -> <[int]> expr]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (M.t, int)
    Stdlib.Type.eq) (x : <[M.t]> expr) -> <[($x) + 1]>
]>
|}]

(* 0 ~~> 0  @  1 <=> 0 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq)
            (x : M.t) -> x + 1
[%%expect {|
- : (<[M.t]> expr, <[int]> expr) Type.eq -> M.t -> int = <fun>
|}]
(* 0 ~~> 0  @  1 <=> 2 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq)
            (x : <[<[M.t]> expr]> expr) -> <[<[$($x) + 1]>]>
[%%expect {|
- : (<[M.t]> expr, <[int]> expr) Type.eq ->
    <[<[M.t]> expr]> expr -> <[<[int]> expr]> expr
= <fun>
|}]
(* 1 ~~> 1  @  1 <=> 2 *)
let _ = <[ fun (Equal : (M.t, int) Type.eq)
               (x : <[M.t]> expr) -> <[$x + 1]> ]>
[%%expect {|
- : <[(M.t, int) Type.eq -> <[M.t]> expr -> <[int]> expr]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (M.t, int)
    Stdlib.Type.eq) (x : <[M.t]> expr) -> <[($x) + 1]>
]>
|}]
(* 0 ~~> 0  @  2 <=> 0 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq)
            (x : M.t) -> x + 1
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq -> M.t -> int =
<fun>
|}]
(* 0 ~~> 0  @  2 <=> 1 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq)
            (x : <[M.t]> expr) -> <[$x + 1]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[M.t]> expr -> <[int]> expr
= <fun>
|}]
(* 0 ~~> 0  @  2 <=> 3 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq)
            (x : <[<[<[M.t]> expr]> expr]> expr) -> <[<[<[$($($x)) + 1]>]>]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[<[<[M.t]> expr]> expr]> expr -> <[<[<[int]> expr]> expr]> expr
= <fun>
|}]
(* 1 ~~> 1  @  2 <=> 1 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq)
               (x : M.t) -> x + 1 ]>
[%%expect {|
- : <[(<[M.t]> expr, <[int]> expr) Type.eq -> M.t -> int]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) (x : M.t) -> x + 1
]>
|}]
(* 1 ~~> 1  @  2 <=> 3 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq)
               (x : <[<[M.t]> expr]> expr) -> <[<[$($x) + 1]>]> ]>
[%%expect {|
- : <[
     (<[M.t]> expr, <[int]> expr) Type.eq ->
     <[<[M.t]> expr]> expr -> <[<[int]> expr]> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) (x : <[<[M.t]> expr]> expr)
    -> <[<[($($x)) + 1]>]>
]>
|}]

(* Evidence travels to the right stage in the future -- should always succeed:
   [S_proof < S_min] and [T_proof = T_subj] *)

(* 0 ~~> 1  @  1 <=> 1 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
     <[ fun (x : M.t) -> x + 1 ]>
[%%expect {|
- : (<[M.t]> expr, <[int]> expr) Type.eq -> <[M.t -> int]> expr = <fun>
|}]
(* 0 ~~> 1  @  2 <=> 2 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq) ->
     <[ fun (x : <[M.t]> expr) -> <[ $x + 1 ]> ]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[<[M.t]> expr -> <[int]> expr]> expr
= <fun>
|}]
(* 0 ~~> 2  @  2 <=> 2 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq) ->
  <[ <[ fun (x : M.t) -> x + 1 ]> ]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[<[M.t -> int]> expr]> expr
= <fun>
|}]

(* 1 ~~> 2  @  2 <=> 2 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        <[ fun (x : M.t) -> x + 1 ]> ]>
[%%expect {|
- : <[(<[M.t]> expr, <[int]> expr) Type.eq -> <[M.t -> int]> expr]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) ->
    <[fun (x : M.t) -> x + 1]>
]>
|}]

(* Introduce the value and then splice to introduce the evidence *)

(* 0 ~~> 1  @  1 <=> 1 *)
let _ = <[
  fun (type t) (x : t) -> $(
    (fun (Equal : (<[t]> expr, <[int]> expr) Type.eq) ->
      <[x + 1]>)
    |> sorry0) ]>
[%%expect{|
- : <[$('t) -> int]> expr = <[fun (type t) (x : t) -> x + 1]>
|}]
(* 1 ~~> 2  @  2 <=> 2 *)
let _ = <[ <[
  fun (type t) (x : t) -> $(
    (fun (Equal : (<[t]> expr, <[int]> expr) Type.eq) ->
      <[x + 1]>)
    |> $sorry1) ]> ]>
[%%expect{|
- : <[<[$($('t)) -> int]> expr]> expr =
<[
  <[
    fun (type t) (x : t) ->
      $
        ((fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
            (<[t]> expr, <[int]> expr) Stdlib.Type.eq) -> <[x + 1]>)
           |>
           (fun (f : (_, _) Stdlib.Type.eq -> _) ->
              f (Stdlib.Obj.magic Stdlib__Type.Equal)))
    ]>
]>
|}]
(* 0 ~~> 2  @  2 <=> 2 *)
let _ = <[ <[
  fun (type t) (x : t) -> $($(
    (fun (Equal : (<[<[t]> expr]> expr, <[<[int]> expr]> expr) Type.eq) ->
      <[<[x + 1]>]>)
    |> sorry0)) ]> ]>
[%%expect{|
- : <[<[$($('t)) -> int]> expr]> expr =
<[<[fun (type t) (x : t) -> $<[x + 1]>]>]>
|}]

(* Evidence travels to the wrong stage in the future -- should always fail:
   [S_proof < S_min] and [T_proof =/= T_subj] *)

(* CR metaprogramming jbachurski: Tests succeed until constraints are staged. *)

(* 0 ~~> 1  @  1 <=> 2 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
     <[ fun (x : <[M.t]> expr) -> <[$x + 1]> ]>
[%%expect {|
- : (<[M.t]> expr, <[int]> expr) Type.eq ->
    <[<[M.t]> expr -> <[int]> expr]> expr
= <fun>
|}]
(* 0 ~~> 2  @  1 <=> 2 *)
let _ = fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
  <[ <[ fun (x : M.t) -> x + 1 ]> ]>
[%%expect {|
- : (<[M.t]> expr, <[int]> expr) Type.eq -> <[<[M.t -> int]> expr]> expr =
<fun>
|}]
(* 1 ~~> 2  @  1 <=> 2 *)
let _ = <[ fun (Equal : (M.t, int) Type.eq) ->
        <[ fun (x : M.t) -> x + 1 ]> ]>
[%%expect {|
- : <[(M.t, int) Type.eq -> <[M.t -> int]> expr]> expr =
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (M.t, int)
    Stdlib.Type.eq) -> <[fun (x : M.t) -> x + 1]>
]>
|}]
(* 0 ~~> 1  @  2 <=> 1 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq) ->
     <[ fun (x : M.t) -> x + 1 ]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[M.t -> int]> expr
= <fun>
|}]
(* 0 ~~> 1  @  2 <=> 3 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq) ->
     <[ fun (x : <[<[M.t]> expr]> expr) -> <[<[$($x) + 1]>]> ]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[<[<[M.t]> expr]> expr -> <[<[int]> expr]> expr]> expr
= <fun>
|}]
(* 0 ~~> 2  @  2 <=> 3 *)
let _ = fun (Equal : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq) ->
  <[ <[ fun (x : <[M.t]> expr) -> <[$x + 1]> ]> ]>
[%%expect {|
- : (<[<[M.t]> expr]> expr, <[<[int]> expr]> expr) Type.eq ->
    <[<[<[M.t]> expr -> <[int]> expr]> expr]> expr
= <fun>
|}]
(* 0 ~~> 3  @  2 <=> 3 *)
let _ = fun (Equal : (<[<[<[M.t]> expr]> expr]> expr,
                      <[<[<[int]> expr]> expr]> expr) Type.eq) ->
  <[ <[ fun (x : M.t) -> x + 1 ]> ]>
[%%expect {|
- : (<[<[<[M.t]> expr]> expr]> expr, <[<[<[int]> expr]> expr]> expr) Type.eq ->
    <[<[M.t -> int]> expr]> expr
= <fun>
|}]
(* 1 ~~> 2  @  2 <=> 3 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        <[ fun (x : <[M.t]> expr) -> <[$x + 1]> ]> ]>
[%%expect {|
- : <[
     (<[M.t]> expr, <[int]> expr) Type.eq ->
     <[<[M.t]> expr -> <[int]> expr]> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) ->
    <[fun (x : <[M.t]> expr) -> <[($x) + 1]>]>
]>
|}]
(* 1 ~~> 3  @  2 <=> 3 *)
let _ = <[ fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
     <[ <[ fun (x : M.t) -> x + 1 ]> ]> ]>
[%%expect {|
- : <[(<[M.t]> expr, <[int]> expr) Type.eq -> <[<[M.t -> int]> expr]> expr]>
    expr
=
<[
  fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) :
    (<[M.t]> expr, <[int]> expr) Stdlib.Type.eq) ->
    <[<[fun (x : M.t) -> x + 1]>]>
]>
|}]

(* 0 ~~> 1  @  0 <=> 1 *)
let _ = <[
  fun (x : M.t) -> $(
    (fun (Equal : (M.t, int) Type.eq) ->
      <[x + 1]>)
    |> sorry0) ]>
[%%expect{|
- : <[M.t -> int]> expr = <[fun (x : M.t) -> x + 1]>
|}]

(* Evidence travels to the right stage in the past -- should always fail:
   [S_proof > S_min], [T_proof = T_subj] *)

(* 1 ~~> 0  @  1 <=> 1 *)
let _ = fun (x : <[M.t]> expr) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        $x + 1 ]>
[%%expect{|
- : <[M.t]> expr -> <[(M.t, int) Type.eq -> int]> expr = <fun>
|}]

(* 2 ~~> 0  @  2 <=> 2 *)
let _ = fun (x : <[<[M.t]> expr]> expr) ->
  <[ <[ fun (Equal : (M.t, int) Type.eq) ->
        $($x) + 1 ]> ]>
[%%expect{|
- : <[<[M.t]> expr]> expr -> <[<[(M.t, int) Type.eq -> int]> expr]> expr =
<fun>
|}]

(* 2 ~~> 1  @  2 <=> 2 *)
let _ =
     <[ fun (x : <[M.t]> expr) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        $x + 1 ]> ]>
[%%expect{|
- : <[<[M.t]> expr -> <[(M.t, int) Type.eq -> int]> expr]> expr =
<[
  fun (x : <[M.t]> expr) ->
    <[
      fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (M.t, int)
        Stdlib.Type.eq) -> ($x) + 1
      ]>
]>
|}]

(* Evidence travels to the right stage in the past, but the target is spliced
   -- should always fail.
   [S_proof > S_min], [S_proof > T_proof = T_subj].
   Should fail because of both time travel and instantiating a type
   that does not exist at a later stage. *)

(* CR quoted-kinds jbachurski: Annotate [t : <[value]>]. *)
(* 1 ~~> 0  @  0 <=> 0 *)
let _ = fun (type t) (x : t expr) -> <[
    (fun (Equal : ($t, int) Type.eq) ->
      $x + 1)
    |> $sorry1 ]>
[%%expect{|
Line 3, characters 7-8:
3 |       $x + 1)
           ^
Error: This expression has type "t expr" but an expression was expected of type
         "<[int]> expr"
       Type "t" is not compatible with type "<[int]>"
|}]
let _ = fun (type t) (x : t expr) -> <[
    (fun (Equal : ($t, int) Type.eq) ->
      let y = $x in (y : int) + 1)
    |> $sorry1 ]>
[%%expect{|
Line 3, characters 21-22:
3 |       let y = $x in (y : int) + 1)
                         ^
Error: This expression has type "$(t)" but an expression was expected of type
         "int"
|}]
(* 2 ~~> 1  @  1 <=> 1 *)
let _ = <[ fun (type t) (x : t expr) -> <[
    (fun (Equal : ($t, int) Type.eq) ->
      $x + 1)
    |> $($sorry2) ]> ]>
[%%expect{|
Line 3, characters 7-8:
3 |       $x + 1)
           ^
Error: This expression has type "t expr" but an expression was expected of type
         "<[int]> expr"
       Type "t" is not compatible with type "<[int]>"
|}]

(* Evidence travels to the wrong stage in the past -- should always fail:
   [S_proof > S_min], [T_proof =/= T_subj] *)
(* This is also time travel, but should fail due to the mis-staged equation. *)

(* CR metaprogramming jbachurski: Tests succeed until time travel is banned
   or constraints are staged. *)

(* 1 ~~> 0  @  1 <=> 0 *)
let _ = fun (x : M.t) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        $(Quote.Expr.int x) + 1 ]>
[%%expect{|
- : M.t -> <[(M.t, int) Type.eq -> int]> expr = <fun>
|}]

(* 2 ~~> 0  @  2 <=> 1 *)
let _ = fun (x : <[M.t]> expr) ->
  <[ <[ fun (Equal : (M.t, int) Type.eq) ->
        $(Quote.Expr.int $x) + 1 ]> ]>
[%%expect{|
- : <[M.t]> expr -> <[<[(M.t, int) Type.eq -> int]> expr]> expr = <fun>
|}]

(* 2 ~~> 1  @  2 <=> 1 *)
let _ =
     <[ fun (x : M.t) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        $(Quote.Expr.int x) + 1 ]> ]>
[%%expect{|
- : <[M.t -> <[(M.t, int) Type.eq -> int]> expr]> expr =
<[
  fun (x : M.t) ->
    <[
      fun ((Stdlib__Type.Equal : (_, _) Stdlib.Type.eq) : (M.t, int)
        Stdlib.Type.eq) -> ($(Stdlib.Quote.Expr.int x)) + 1
      ]>
]>
|}]

(* Repeated equations at different stages *)

(* CR metaprogramming jbachurski: Tests (might) have wrong outputs until
   constraints are staged. *)

(* Both proofs at stage 0 *)
(* succeeds, because we instantiate stage 1 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : <[M.t]> expr) -> <[ $x + 0 ]>
[%%expect {|
Line 2, characters 13-18:
2 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]
(* succeeds, because we instantiate stage 0 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : M.t) -> x ^ ""
[%%expect {|
Line 2, characters 13-18:
2 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]
(* fails, because we only instantiate stage 1 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : <[M.t]> expr) -> <[ $x ^ "" ]>
[%%expect {|
Line 2, characters 13-18:
2 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]
(* fails, because we only instantiate stage 0 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : M.t) -> x + 0
[%%expect {|
Line 2, characters 13-18:
2 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]

(* Both proofs at different stages *)
(* succeeds, because we instantiate stage 1 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        fun (x : M.t) -> x + 0 ]>
[%%expect {|
Line 2, characters 13-18:
2 |      <[ fun (Equal : (M.t, int) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type "(M.t, M.t) Type.eq"
       but a pattern was expected which matches values of type
         "(M.t, int) Type.eq"
       Type "M.t" = "string" is not compatible with type "int"
|}]
(* fails, because we instantiate stage 0, which travels time *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
        fun (x : <[M.t]> expr) ->
     <[ fun (Equal : (M.t, int) Type.eq) -> $x + 0 ]>
[%%expect {|
Line 3, characters 13-18:
3 |      <[ fun (Equal : (M.t, int) Type.eq) -> $x + 0 ]>
                 ^^^^^
Error: This pattern matches values of type "(M.t, M.t) Type.eq"
       but a pattern was expected which matches values of type
         "(M.t, int) Type.eq"
       Type "M.t" = "string" is not compatible with type "int"
|}]
(* fails, because we only instantiate stage 1 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        fun (x : M.t) -> x ^ "" ]>
[%%expect {|
Line 2, characters 13-18:
2 |      <[ fun (Equal : (M.t, int) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type "(M.t, M.t) Type.eq"
       but a pattern was expected which matches values of type
         "(M.t, int) Type.eq"
       Type "M.t" = "string" is not compatible with type "int"
|}]
(* fails, because we only instantiate stage 0 *)
let _ = fun (Equal : (M.t, string) Type.eq) ->
        fun (x : <[M.t]> expr) -> <[
        fun (Equal : (M.t, int) Type.eq) -> $x ^ "" ]>
[%%expect {|
Line 3, characters 13-18:
3 |         fun (Equal : (M.t, int) Type.eq) -> $x ^ "" ]>
                 ^^^^^
Error: This pattern matches values of type "(M.t, M.t) Type.eq"
       but a pattern was expected which matches values of type
         "(M.t, int) Type.eq"
       Type "M.t" = "string" is not compatible with type "int"
|}]

(* Conflicting proofs -- fails *)
let _ = fun (Equal : (<[M.t]> expr, <[string]> expr) Type.eq) ->
     <[ fun (Equal : (M.t, int) Type.eq) ->
        fun (x : M.t) -> (x + 0, x ^ "") ]>
[%%expect {|
Line 2, characters 13-18:
2 |      <[ fun (Equal : (M.t, int) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type "(M.t, M.t) Type.eq"
       but a pattern was expected which matches values of type
         "(M.t, int) Type.eq"
       Type "M.t" = "string" is not compatible with type "int"
|}]

(* Both proofs at stage 1 *)
(* succeeds, because we instantiate stage 2 *)
let _ = <[
        fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : <[M.t]> expr) -> <[ $x + 0 ]> ]>
[%%expect {|
Line 3, characters 13-18:
3 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]
(* succeeds, because we instantiate stage 1 *)
let _ =
     <[ fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : M.t) -> x ^ "" ]>
[%%expect {|
Line 3, characters 13-18:
3 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]
(* fails, because we only instantiate stage 2 *)
let _ =
     <[ fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : <[M.t]> expr) -> <[ $x ^ "" ]> ]>
[%%expect {|
Line 3, characters 13-18:
3 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]
(* fails, because we only instantiate stage 1 *)
let _ =
     <[ fun (Equal : (M.t, string) Type.eq) ->
        fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
        fun (x : M.t) -> x + 0 ]>
[%%expect {|
Line 3, characters 13-18:
3 |         fun (Equal : (<[M.t]> expr, <[int]> expr) Type.eq) ->
                 ^^^^^
Error: This pattern matches values of type
         "(<[M.t]> expr, <[M.t]> expr) Type.eq"
       but a pattern was expected which matches values of type
         "(<[M.t]> expr, <[int]> expr) Type.eq"
       Type "<[M.t]>" = "<[string]>" is not compatible with type "<[int]>"
       Type "string" is not compatible with type "int"
|}]


(** Time travel **)

(* CR metaprogramming jbachurski: Tests succeed until time travel is banned. *)

let time_traveller =
  <[ fun (type t) () ->
    $((fun (x : <[t]> expr) -> <[
        fun (Equal : (t, int) Type.eq) ->
          ($(x : <[int]> expr) : int) ]>) |> ignore;
        <[()]>) ]>
[%%expect {|
val time_traveller : <[unit -> unit]> expr = <[fun (type t) () -> ()]>
|}]

(* Magic *)
let weak_magic_with_time_travel (x : <[M.t]> expr) : <[M.t']> expr =
  let result = ref (None : <[M.t']> expr option) in
  <[ fun (Equal : (M.t, M.t') Type.eq) ->
     $(result := Some x; x) ]> |> ignore;
  !result |> Option.get
[%%expect {|
val weak_magic_with_time_travel : <[M.t]> expr -> <[M.t']> expr = <fun>
|}]
let magic_with_time_travel =
  <[ fun (type a b) (x : a) : b -> $(
      let result = ref (None : <[b]> expr option) in
      <[ fun (Equal : (a, b) Type.eq) ->
        $(result := Some <[x]>; <[x]>) ]> |> ignore;
      !result |> Option.get
  ) ]>
[%%expect {|
val magic_with_time_travel : <[$('a) -> $('b)]> expr =
  <[fun (type a) (type b) (x : a) -> (x : b)]>
|}]
let magic_with_time_travel_and_past_types (type a b) (x : a) : b =
  let result = ref (None : b option) in
  <[ fun (Equal : ($a, $b) Type.eq) ->
     $(result := Some x; <[()]>) ]> |> ignore;
  !result |> Option.get
[%%expect {|
Line 4, characters 22-23:
4 |      $(result := Some x; <[()]>) ]> |> ignore;
                          ^
Error: This expression has type "a" but an expression was expected of type "b"
|}]
(* Splices are not instantiable *)
let foo (type a) (c : a expr) = <[
  fun (x : $a) (Equal : ($a, int) Type.eq) ->
    $c + 42 ]>
let bad = foo <["abc"]>
[%%expect {|
Line 3, characters 5-6:
3 |     $c + 42 ]>
         ^
Error: This expression has type "a expr" but an expression was expected of type
         "<[int]> expr"
       Type "a" is not compatible with type "<[int]>"
|}]
