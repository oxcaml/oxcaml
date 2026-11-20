(* TEST
 reference = "${test_source_directory}/addr_imm_lib.reference";
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 {
   bytecode;
 } {
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   flags = "-O3";
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

let print_int_ln x = Printf.printf "%d\n" x
let print_i64_ln x = Printf.printf "%Ld\n" (Int64_u.to_int64 x)

type pair = #{ a : int64_u; b : int64_u }

type imm_record = { i_int : int; i_str : string; i_pair : pair }

let imm_record () =
  { i_int = 5; i_str = "efgh"; i_pair = #{ a = #7L; b = #6L } }

let () =
  print_endline "of_idx";
  let r = imm_record () in
  print_int_ln (Addr_imm.get (Addr_imm.of_idx r (.i_int)));
  print_int_ln (Addr_imm.get_read (Addr_imm.of_idx_read r (.i_int)));
  print_int_ln (Addr_imm.get_write (Addr_imm.of_idx_write r (.i_int)));
  print_endline
    (Addr_imm.get_immutable (Addr_imm.of_idx_immutable r (.i_str)));
  print_i64_ln (Addr_imm.get (Addr_imm.of_idx r (.i_pair.#a)));
  print_i64_ln
    (Addr_imm.get_immutable (Addr_imm.of_idx_immutable r (.i_pair.#b)));
  print_newline ()

let () =
  print_endline "local of_idx";
  let r =
    stack_ { i_int = 5; i_str = "efgh"; i_pair = #{ a = #7L; b = #6L } }
  in
  print_int_ln (Addr_imm.get (Addr_imm.of_idx_local r (.i_int)));
  print_endline
    (Addr_imm.get_immutable (Addr_imm.of_idx_immutable_local r (.i_str)));
  print_i64_ln (Addr_imm.get (Addr_imm.of_idx_local r (.i_pair.#a)));
  print_i64_ln
    (Addr_imm.get_immutable (Addr_imm.of_idx_immutable_local r (.i_pair.#b)));
  print_newline ()
