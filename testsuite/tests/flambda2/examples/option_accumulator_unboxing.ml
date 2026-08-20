(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Check that changing the parameter kind of [option] to [any] isn't interfering
   with unboxing through loops. These two functions must produce the same
   flambda (modulo types). In particular, neither form should actually construct
   a [Some]/[Just] for the loop accumulator. *)

let[@inline never] last_any (h : int -> 'a) (default : 'a) n =
  let r = ref None in
  for i = 0 to n - 1 do
    r := Some (h i)
  done;
  match !r with
  | None -> default
  | Some x -> x

type ('a : value) value_option =
  | Nothing
  | Just of 'a

let[@inline never] last_value (h : int -> 'a) (default : 'a) n =
  let r = ref Nothing in
  for i = 0 to n - 1 do
    r := Just (h i)
  done;
  match !r with
  | Nothing -> default
  | Just x -> x
