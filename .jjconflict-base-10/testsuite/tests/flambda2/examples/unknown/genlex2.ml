(* TEST
 compile_only = "true";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

(* Reduced from stdlib/genlex.ml *)

external raise : exn -> 'a = "%raise"

exception Error of string

let foo f g =
  match f with
  | 'a' ->
    let _ = try g () with Failure _ -> raise (Error "") in
    raise (Error "")
  | _ -> None
