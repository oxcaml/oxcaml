(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-dlambda -dcanonical-ids";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The parameter of [option] has kind [any], so an option's payload type can
   still have jkind [any] at translation time (e.g. ['a] used only as an
   option payload). [Typeopt.value_kind] then cannot determine the payload's
   representation and falls back to [Pgenval] for the whole option, instead of
   the variant kind [(consts (0)) (non_consts ([0: ?]))]. Anything that forces
   the payload to have a sort restores the precise kind: the sort variable
   defaults to [value] and the [Cstr_layout_variable] recompute path in
   [Typeopt.value_kind_variant] then succeeds.

   The functions below record exactly when each behavior applies. *)

(* ['a] is used only as an option payload, so it keeps jkind [any]: the return
   kind is just [val] (printed as no annotation at all). *)
let ret_phantom (h : unit -> 'a option) = h ()

(* [d : 'a] forces ['a] to have a sort, so the return kind is the precise
   variant kind. *)
let ret_sorted (h : unit -> 'a option) (d : 'a) = ignore d; h ()

(* The [Some _] pattern types its subpattern at ['a], which also forces the
   sort and restores the variant kind. *)
let match_some (h : unit -> 'a option) =
  match h () with
  | None -> h ()
  | Some _ -> h ()

(* A bare [_] arm never looks inside [Some], so ['a] keeps jkind [any] and the
   binding of [o] gets kind [val]. Note that code in this weakened class can
   only pass options around and test [isint] on them; it can neither build a
   [Some] nor project out of one, so the missing variant kind cannot inhibit
   any Flambda 2 transformation (see option_untouched_payload.ml). *)
let match_wild (h : unit -> 'a option) =
  let o = h () in
  match o with
  | None -> true
  | _ -> false
