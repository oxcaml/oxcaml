(* TEST flags = "-extension layouts_alpha"; expect;
*)

(* REGRESSION PIN for the round_up GADT-equation shape (deletion-wave-2 resolution of the
   former "Stage C round_up ENGINE-DIVERGENCE finding", STAGE5F-NOTES.md).

   A GADT equation over a recursive with-bound type drives [Ctype.add_gadt_equation] ->
   [add_jkind_equation] -> [intersect_type_jkind] -> [Jkind.round_up] on the
   [jkind_of_abstract_type_declaration] equation jkind. [round_up] is now derived by the
   ikind engine ([Solver.round_up] in Round_up mode). The earlier "ikind under-computes"
   reading was mistaken: legacy [Ignore_best normalize] OVER-approximates deep-recursive
   with-bounds to top via fuel exhaustion, while the ikind computes the true (tighter)
   crossing; both are sound upper bounds ([t <= round_up t]) and the ikind is [<=] legacy.
   Under OXCAML_IKINDS_VALIDATE the leq-differential confirms [ikind_floor <= legacy_floor]
   on this shape.

   This test pins the shape: it must keep compiling clean, and clean under
   OXCAML_IKINDS_VALIDATE (leq-differential no disagreement). A regression that makes the
   ikind round_up EXCEED legacy (unsound) would trip the differential. *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

type _ kind =
  | KInt : int tree kind
  | KStr : string tree kind

(* The [function] match refines the existential index [a] to [int tree] / [string tree]
   via a GADT equation, exercising round_up on the recursive with-bound kind of [_ tree]. *)
let describe : type a. a kind -> a -> unit =
  fun k x ->
  match k with
  | KInt -> ignore (x : int tree)
  | KStr -> ignore (x : string tree)
;;

[%%expect
  {|
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree
type _ kind = KInt : int tree kind | KStr : string tree kind
val describe : 'a kind -> 'a -> unit = <fun>
|}]
