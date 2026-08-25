(* TEST
 reference =
   "${test_source_directory}/record_any_field_inference.reference";
 flambda2;
 {
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* Specializing [any]-fields in records *)

external box_float : float# -> float = "%box_float"

type ('a : any) with_id = { id : int; value : 'a }

type prod = #{ foo : int ; bar : string}

(* These functions determine the representation in slightly different ways. *)
let bar_len_1 ({ id; value = t } : prod with_id) =
  ignore (id : int);
  String.length t.#bar

let bar_len_2 ({ id; value = t } : _ with_id) =
  ignore (id : int);
  String.length t.#bar

let bar_len_3 (r : _ with_id) =
  ignore (r.id : int);
  String.length r.value.#bar

(* In each of these, [x] later gets specialized to a different sort *)
let mk_prod x = { id = 1; value = x }
let mk_string x = { id = 1; value = x }
let mk_float x = { id = 2; value = x }
let mk_product x = { id = 3; value = x }

let () =
  let r = mk_prod #{ foo = 1; bar = "three" } in
  Printf.printf "bar_len_1: %d\n" (bar_len_1 r);
  Printf.printf "bar_len_2: %d\n" (bar_len_2 r);
  Printf.printf "bar_len_3: %d\n" (bar_len_3 r);
  let a = mk_string "hi" in
  Printf.printf "mk_string: %d %s\n" a.id a.value;
  let b = mk_float #3.14 in
  Printf.printf "mk_float: %d %.2f\n" b.id (box_float b.value);
  let p = mk_product #{ foo = 4; bar = "four" } in
  Printf.printf "mk_product: %d %d %s\n" p.id p.value.#foo p.value.#bar


type ('a : any) cell = { mutable payload : 'a; tag : int }

(* [x] later gets specialized to [float64] *)
let set_payload c x = c.payload <- x; c

let () =
  let c = { payload = #3.14; tag = 7 } in
  let c = set_payload c #2.72 in
  Printf.printf "set_payload: %.2f %d\n" (box_float c.payload) c.tag
