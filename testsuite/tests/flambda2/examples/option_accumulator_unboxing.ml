(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Variant unboxing of a loop-carried option accumulator must not be affected
   by the parameter of [option] having kind [any]. Here ['a] is used at sort
   [value] (it is the return type of [h] and the type of [default]), so
   [Typeopt.value_kind] recomputes the precise variant kind for the option
   even though [option]'s parameter kind is [any].

   [last_custom] is an isomorphic control: ['a opt] has a [value] parameter
   kind, so it behaves exactly like [option] did before its parameter kind was
   changed to [any]. The simplified code of the two functions must be
   structurally identical (modulo kind annotations): in particular, in both,
   the accumulator must be unboxed into [is_int]/field continuation
   parameters, leaving no [Make_block] for [Some]/[Just] in the loop. *)

type 'a opt =
  | Nothing
  | Just of 'a

let[@inline never] last_option (h : int -> 'a) (default : 'a) n =
  let r = ref None in
  for i = 0 to n - 1 do
    r := Some (h i)
  done;
  match !r with
  | None -> default
  | Some x -> x

let[@inline never] last_custom (h : int -> 'a) (default : 'a) n =
  let r = ref Nothing in
  for i = 0 to n - 1 do
    r := Just (h i)
  done;
  match !r with
  | Nothing -> default
  | Just x -> x
