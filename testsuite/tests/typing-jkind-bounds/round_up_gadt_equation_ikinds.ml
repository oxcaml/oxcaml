(* TEST flags = "-extension layouts_alpha"; expect;
*)

(* REGRESSION PIN for the Stage C round_up engine-divergence (STAGE5F-NOTES.md, "Stage C
   round_up re-route: ENGINE-DIVERGENCE finding").

   A GADT equation over a recursive with-bound type drives [Ctype.add_gadt_equation] ->
   [add_jkind_equation] -> [intersect_type_jkind] -> [Jkind.round_up] on the
   [jkind_of_abstract_type_declaration] equation jkind. On this class the legacy
   [Ignore_best normalize] [round_up] folds the with- bounds UP into a bounds-free floor,
   which the ikind engine ([ckind_of_jkind] in NORMAL or ROUND_UP mode) does NOT reproduce
   (it yields the with-bounds- EXCLUDED base). So [round_up] was NOT re-routed to ikind in
   this wave; it keeps calling legacy [normalize], and [Base_and_axes.normalize] survives
   for it.

   This test pins the shape so a future attempt to derive [round_up] in the ikind engine
   has a concrete, minimal reproduction. It must keep PASSING with legacy [round_up]. If
   someone re-routes [round_up] and this compiles clean under OXCAML_IKINDS_VALIDATE with
   the differential re-armed, that's the signal the engine now reproduces the fold. *)

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
