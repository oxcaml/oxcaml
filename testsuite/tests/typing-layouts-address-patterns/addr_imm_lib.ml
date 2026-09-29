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
  print_endline "of_idx and deepen";
  let r = imm_record () in
  print_int_ln (Addr_imm.get (Addr_imm.of_idx r (.i_int)));
  print_int_ln (Addr_imm.get__read (Addr_imm.of_idx__read r (.i_int)));
  print_int_ln (Addr_imm.get__write (Addr_imm.of_idx__write r (.i_int)));
  print_endline
    (Addr_imm.get__immutable (Addr_imm.of_idx__immutable r (.i_str)));
  let addr = Addr_imm.of_idx r (.i_pair) in
  let addr = Addr_imm.deepen addr ~f:(fun i -> (.idx_imm(i).#a)) in
  print_i64_ln (Addr_imm.get addr);
  let addr = Addr_imm.of_idx__read r (.i_pair) in
  let addr = Addr_imm.deepen__read addr ~f:(fun i -> (.idx_imm(i).#b)) in
  print_i64_ln (Addr_imm.get__read addr);
  let addr = Addr_imm.of_idx__write r (.i_pair) in
  let addr = Addr_imm.deepen__write addr ~f:(fun i -> (.idx_imm(i).#a)) in
  print_i64_ln (Addr_imm.get__write addr);
  let addr = Addr_imm.of_idx__immutable r (.i_pair) in
  let addr = Addr_imm.deepen__immutable addr ~f:(fun i -> (.idx_imm(i).#b)) in
  print_i64_ln (Addr_imm.get__immutable addr);
  print_newline ()

let () =
  print_endline "local";
  let r =
    stack_ { i_int = 5; i_str = "efgh"; i_pair = #{ a = #7L; b = #6L } }
  in
  print_int_ln (Addr_imm.get (Addr_imm.of_idx__local r (.i_int)));
  print_endline
    (Addr_imm.get__immutable (Addr_imm.of_idx__immutable__local r (.i_str)));
  let addr = Addr_imm.of_idx__local r (.i_pair) in
  let addr = Addr_imm.deepen__local addr ~f:(fun i -> (.idx_imm(i).#a)) in
  print_i64_ln (Addr_imm.get addr);
  let addr = Addr_imm.of_idx__immutable__local r (.i_pair) in
  let addr =
    Addr_imm.deepen__immutable__local addr ~f:(fun i -> (.idx_imm(i).#b))
  in
  print_i64_ln (Addr_imm.get__immutable addr);
  print_newline ()
