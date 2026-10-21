(* TEST
 flambda2;
 flags = "-flambda2-kind-checks -dflambda-invariants";
 flags += " -dcmm-invariants -dcfg-invariants";
 {
   ocamlopt_flags = "-Oclassic";
   native;
 }{
   ocamlopt_flags = "-O3";
   native;
 }
*)

module Inline_record = struct
  type t =
    | A of { x : int32_u; y : float# }
    | B of { a : int; b : string }

  let f () =
    let (B r) = A { x = #0l; y = #0.1 } in
    String.length r.b

  let[@inline never] unknown_field y =
    let (B r) = A { x = #0l; y } in
    String.length r.b

  let compatible () =
    match B { a = 1; b = "abc" } with
    | B r -> String.length r.b
    | A _ -> 0
end

module Scalar = struct
  type t =
    | A of int
    | B of float#

  let f () = print_int (match B #0.1 with A x -> x | B _ -> 0)
  let compatible () = match A 42 with A x -> x | B _ -> 0
end

module Value_to_float = struct
  type t =
    | A of string
    | B of float#

  external box_float : float# -> float = "%box_float"

  let f () =
    match A "ok" with
    | A s -> String.length s
    | B x -> int_of_float (box_float x)

  let compatible () =
    match B #3. with
    | A s -> String.length s
    | B x -> int_of_float (box_float x)
end

let () =
  (match Inline_record.f () with
   | n -> Printf.printf "inline record: returned %d\n" n
   | exception Match_failure _ -> print_endline "inline record: Match_failure");
  (match Inline_record.unknown_field #0.1 with
   | n -> Printf.printf "unknown field: returned %d\n" n
   | exception Match_failure _ -> print_endline "unknown field: Match_failure");
  print_string "scalar: ";
  Scalar.f ();
  print_newline ();
  Printf.printf "compatible record: %d\n" (Inline_record.compatible ());
  Printf.printf "compatible int: %d\n" (Scalar.compatible ());
  Printf.printf "value to float: %d\n" (Value_to_float.f ());
  Printf.printf "compatible float: %d\n" (Value_to_float.compatible ())
