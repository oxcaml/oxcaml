(* TEST
 reference =
   "${test_source_directory}/inline_record_any_field_inference.reference";
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

(* Specializing [any]-fields in inline records *)

type ('a : any) w = W of { id : int; value : 'a }

type t =
  #{ foo : int
   ; bar : string
   }

(* These functions determine the representation in slightly different ways. *)
let g (W { id; value = t } : t w) =
  ignore (id : int);
  String.length t.#bar

let f (W { id; value = t } : _ w) =
  ignore (id : int);
  String.length t.#bar

(* [x] later gets specialized to an unboxed product *)
let mk x = W { id = 1; value = x }

let use_mk () = mk #{ foo = 2; bar = "hello" }

let () =
  let w = use_mk () in
  Printf.printf "g: %d\n" (g w);
  Printf.printf "f: %d\n" (f w);
  (match w with
   | W { id; value } ->
     Printf.printf "match: %d %d\n" id (String.length value.#bar))
