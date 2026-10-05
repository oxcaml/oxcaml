(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

type ('a : value) value_option =
  | Nothing
  | Just of 'a

(* Now that the parameter of [option] has kind [any], an option whose payload is
   never constructed, projected, or matched with a payload-touching pattern gets
   value kind [ 0 | 0 of any ] instead of [ 0 | 0 of val ]. *)

(* Returns [ 0 | 0 of any ] *)
let[@inline never] passthrough_any (h : unit -> 'a option) = h ()

(* Returns [ 0 | 0 of val ] *)
let[@inline never] passthrough_value (h : unit -> 'a value_option) = h ()

(* The option has kind [ 0 | 0 of any ] *)
let[@inline never] is_none_any p (h : unit -> 'a option) =
  let o = if p then None else h () in
  match o with
  | None -> true
  | _ -> false

(* The option has kind [ 0 | 0 of val ] *)
let[@inline never] is_none_value p (h : unit -> 'a value_option) =
  let o = if p then Nothing else h () in
  match o with
  | Nothing -> true
  | _ -> false

(* When computing value kinds for types containing [any], we instantiate the
   type, which can increase the precision *)

(* The option has kind [ 0 | 0 of imm tagged ] *)
let[@inline never] caller_any (h : unit -> int option) =
  match passthrough_any h with
  | None -> 0
  | Some x -> x + 1

(* The option has kind [ 0 | 0 of val ] *)
let[@inline never] caller_value (h : unit -> int value_option) =
  match passthrough_value h with
  | Nothing -> 0
  | Just x -> x + 1
