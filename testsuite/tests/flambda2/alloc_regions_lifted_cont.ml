(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3 -flambda2-match-in-match";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Match-in-match lifts the continuations for the arms of the match in [f]
   (inlined into [h]) out of the handler that binds (and removes, since its
   actions are all [Transfer]) the allocation region of the inlined [p]. The
   [removed_alloc_regions] entries must be renamed along with the lifted
   parameters ([DE.rename_removed_alloc_regions]), so that the check actions
   in the lifted handlers still find the removed region.

   This also checks that the flow analysis sees the regions used only by check
   actions ([Flow.Acc.add_check_actions]): without it, the lifted region
   parameters would be removed as unused while still occurring in the check
   actions of the lifted handlers. *)
external (+) : int -> int -> int = "%addint"
type t = A | B | C of int

let[@inline] f g x y =
  let r =
    match x with
    | A -> g (y, y) + 1
    | B -> y
    | C a -> a + 1
  in
  r + 1

let[@inline] [@zero_alloc assume] p f g x y = f g x y

let h g x y =
  let z = match x with A -> B | B -> C y | C _ -> A in
  p f g z y
