(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-raw;
   check-fexpr-dump;
 *)

(* This test is checking that we do not introduce spurious switches on %is_int
   for GADTs where all of the constant or non-constant constructors have been
   eliminated. *)

type _ t =
  | A : string t
  | B : string t
  | C : int -> int t
  | D : int -> int t

let extract_int (x : int t) =
  (* This should not contain calls to the %is_int primitive *)
  match x with
  | C s -> s
  | D s -> s + 1

let extract_string (x : string t) =
  (* This should not contain calls to the %is_int primitive *)
  match x with
  | A -> "A"
  | B -> "B"

