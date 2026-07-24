(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Since the parameter of [option] has kind [any], an option whose payload is
   never constructed, projected, or matched with a payload-touching pattern
   gets value kind [val] instead of the variant kind [ 0 | 0 of val ] (the
   payload's jkind is still [any] at translation time; see
   option_value_kind_recovery.ml). This file checks that the weaker kind is
   harmless: each [*_option] function has an isomorphic [*_custom] control
   using ['a opt], whose [value] parameter kind reproduces the kinds [option]
   had before the change. The simplified code must be structurally identical
   across each pair; only kind annotations may differ. *)

type 'a opt =
  | Nothing
  | Just of 'a

(* ['a] is used only as an option payload, so [passthrough_option]'s return
   kind is [val] where [passthrough_custom]'s is [ 0 | 0 of val ]. Nothing
   consumes a function's return kind for options (Flambda 2 has no return
   unboxing, and callers recompute the result's kind from their own type
   instance), so the code is unchanged. *)
let[@inline never] passthrough_option (h : unit -> 'a option) = h ()

let[@inline never] passthrough_custom (h : unit -> 'a opt) = h ()

(* The callers instantiate the payload at [int] and derive the kind of the
   call result themselves; the option version even gets the *stronger* kind
   [ 0 | 0 of imm tagged ] for the result, since the [any]-kinded parameter
   lets the payload kind be recomputed from the instantiation. *)
let[@inline never] caller_option (h : unit -> int option) =
  match passthrough_option h with
  | None -> 0
  | Some x -> x + 1

let[@inline never] caller_custom (h : unit -> int opt) =
  match passthrough_custom h with
  | Nothing -> 0
  | Just x -> x + 1

(* A join of [None] with an unknown call result, where the joined value is
   only tested with [isint] (the [_] arm does not look inside [Some], so the
   payload stays at jkind [any] and [o]'s kind weakens to [val]). The variant
   kind could not have helped here: both switch arms are inhabited, and there
   is no [Some] construction for unboxing to remove. *)
let[@inline never] is_none_option p (h : unit -> 'a option) =
  let o = if p then None else h () in
  match o with
  | None -> true
  | _ -> false

let[@inline never] is_none_custom p (h : unit -> 'a opt) =
  let o = if p then Nothing else h () in
  match o with
  | Nothing -> true
  | _ -> false
