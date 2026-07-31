(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 compile_only = "true";
 ocamlopt_opt_exit_status = "2";
 setup-ocamlopt.opt-build-env;
 ocamlopt.opt;
*)

(* A record with an [any]-field, used in functions that don't constrain the
   sort variable *)

external box_float : float# -> float = "%box_float"

type ('a : any) with_id = { id : int; value : 'a }

type t =
  #{ foo : int
   ; bar : string
   }

let g ({ id; value = t } : t with_id) =
  ignore (id : int);
  String.length t.#bar

let f ({ id; value = t } : _ with_id) =
  ignore (id : int);
  String.length t.#bar

let f' (r : _ with_id) =
  ignore (r.id : int);
  String.length r.value.#bar

let mk x =
  let r = { id = 0; value = x } in
  r

let use_mk () = mk #{ foo = 1; bar = "three" }

type ('a : any) cell = { mutable payload : 'a; tag : int }

let set_payload c x =
  c.payload <- x;
  c

let () =
  let r = use_mk () in
  Printf.printf "g: %d\n" (g r);
  Printf.printf "f: %d\n" (f r);
  Printf.printf "f': %d\n" (f' r);
  let c = { payload = #3.14; tag = 7 } in
  let c = set_payload c #2.72 in
  Printf.printf "set_payload: %.2f %d\n" (box_float c.payload) c.tag
