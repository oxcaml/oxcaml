(* TEST
 reference = "${test_source_directory}/addr_lib.reference";
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

type mut_record =
  { mutable m_int : int; mutable m_str : string; mutable m_pair : pair }

type imm_record = { i_str : string; i_pair : pair }

let mut_record () =
  { m_int = 1; m_str = "abcd"; m_pair = #{ a = #3L; b = #2L } }

let () =
  print_endline "of_idx";
  let r = mut_record () in
  let addr = Addr.of_idx r (.m_int) in
  print_int_ln (Addr.get addr);
  Addr.set addr 42;
  print_int_ln r.m_int;
  let addr = Addr.of_idx_read r (.m_int) in
  print_int_ln (Addr.get_read addr);
  let addr = Addr.of_idx_write r (.m_int) in
  Addr.set addr 43;
  print_int_ln r.m_int;
  let addr = Addr.of_idx r (.m_pair.#b) in
  print_i64_ln (Addr.get addr);
  Addr.set addr (-#1L);
  print_i64_ln r.m_pair.#b;
  let addr = Addr.of_idx_read r (.m_pair) in
  print_i64_ln (Addr.get_read addr).#a;
  print_newline ()

let () =
  print_endline "set";
  let r = mut_record () in
  let addr = Addr.of_idx r (.m_str) in
  print_endline (Addr.get addr);
  Addr.set addr "wxyz";
  print_endline r.m_str;
  print_endline (Addr.get addr);
  print_newline ()

let () =
  print_endline "of_imm";
  let r = { i_str = "efgh"; i_pair = #{ a = #7L; b = #6L } } in
  let addr = Addr.of_imm (Addr_imm.of_idx_read r (.i_str)) in
  print_endline (Addr.get_read addr);
  let addr = Addr.of_imm (Addr_imm.of_idx_read r (.i_pair.#b)) in
  print_i64_ln (Addr.get_read addr);
  print_newline ()

let () =
  print_endline "local of_idx";
  let r =
    stack_ { m_int = 1; m_str = "abcd"; m_pair = #{ a = #3L; b = #2L } }
  in
  let addr = Addr.of_idx_local r (.m_int) in
  Addr.set addr 42;
  print_int_ln r.m_int;
  let addr = Addr.of_idx_local r (.m_str) in
  Addr.set addr "wxyz";
  print_endline r.m_str;
  let addr = Addr.of_idx_local r (.m_pair.#a) in
  Addr.set addr #10L;
  print_i64_ln r.m_pair.#a;
  let r = stack_ { i_str = "efgh"; i_pair = #{ a = #7L; b = #6L } } in
  let addr = Addr_imm.of_idx_read_local r (.i_pair.#b) in
  print_i64_ln (Addr.get_read (Addr.of_imm_local addr));
  print_newline ()
