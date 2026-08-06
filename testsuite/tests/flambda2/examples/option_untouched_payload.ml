(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Now that the parameter of [option] has kind [any], an option whose payload is
   never constructed, projected, or matched with a payload-touching pattern gets
   value kind [val] instead of the variant kind [ 0 | 0 of val ] (the payload's
   jkind is still [any] at translation time; see option_value_kind_recovery.ml).
   This file checks that the weaker kind is harmless: replacing the [option]
   with its old version changes nothing. The simplified code must be
   structurally identical across each pair; only kind annotations may differ. *)

type ('a : value) value_option =
  | Nothing
  | Just of 'a

(* ['a] is used only as an option payload, so [passthrough_any]'s return
   kind is [val] where [passthrough_value]'s is [ 0 | 0 of val ]. Nothing
   consumes a function's return kind for options (Flambda 2 has no return
   unboxing, and callers recompute the result's kind from their own type
   instance), so the code is unchanged. *)
let[@inline never] passthrough_any (h : unit -> 'a option) = h ()

let[@inline never] passthrough_value (h : unit -> 'a value_option) = h ()

(* The callers instantiate the payload at [int] and derive the kind of the
   call result themselves; the option version even gets the *stronger* kind
   [ 0 | 0 of imm tagged ] for the result, since the [any]-kinded parameter
   lets the payload kind be recomputed from the instantiation. *)
let[@inline never] caller_any (h : unit -> int option) =
  match passthrough_any h with
  | None -> 0
  | Some x -> x + 1

let[@inline never] caller_value (h : unit -> int value_option) =
  match passthrough_value h with
  | Nothing -> 0
  | Just x -> x + 1

(* A join of [None] with an unknown call result, where the match on the joined
   value is shallow. The variant kind could not have helped here: both switch
   arms are inhabited, and there is no [Some] construction for unboxing to
   remove. *)
let[@inline never] is_none_any p (h : unit -> 'a option) =
  let o = if p then None else h () in
  match o with
  | None -> true
  | _ -> false

let[@inline never] is_none_value p (h : unit -> 'a value_option) =
  let o = if p then Nothing else h () in
  match o with
  | Nothing -> true
  | _ -> false
