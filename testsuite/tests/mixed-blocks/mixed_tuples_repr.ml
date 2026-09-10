(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 compiler_reference = "${test_source_directory}/mixed_tuples_repr.compiler.reference";
 compiler_reference2 = "${test_source_directory}/mixed_tuples_repr.compiler.reference";
 {
   reference = "${test_source_directory}/mixed_tuples_repr.native.reference";
   native;
 }{
   reference = "${test_source_directory}/mixed_tuples_repr.bytecode.reference";
   bytecode;
 }
*)

module Float_u = Stdlib_upstream_compatible.Float_u

let print_block_kind name x =
  let uniform_or_mixed = Obj.Uniform_or_mixed.of_block (Obj.repr x) in
  match Obj.Uniform_or_mixed.repr uniform_or_mixed with
  | Uniform ->
    assert (Obj.Uniform_or_mixed.is_uniform uniform_or_mixed);
    assert (not (Obj.Uniform_or_mixed.is_mixed uniform_or_mixed));
    Printf.printf "%s: uniform\n" name
  | Mixed { scannable_prefix_len } ->
    assert (Obj.Uniform_or_mixed.is_mixed uniform_or_mixed);
    assert (not (Obj.Uniform_or_mixed.is_uniform uniform_or_mixed));
    Printf.printf "%s: mixed (scannable_prefix_len = %d)\n" name
      scannable_prefix_len

(* Type declarations, construction, destruction *)

type t1 = int * #(string * bool)
type t2 = unit# * unit# * int
type t3 = int * float# * #((unit * string) * unit#)

let equal_t1 ((x, #(y, z)) : t1) ((x', #(y', z')) : t1) =
  Int.equal x x' && String.equal y y' && Bool.equal z z'

(* Uniform block after flattening *)
let () =
  let reconstructed =
    match 4, #("hi", false) with
    | x, #(y, z) -> x, #(y, z)
  in
  assert (equal_t1 reconstructed (4, #("hi", false)));
  print_block_kind "t1" reconstructed

(* Singleton block *)
let () = print_block_kind "t2" ((#(), #(), 42) : t2)

(* all void mixed tuple *)

type all_unit_u = unit# * unit# * #(unit# * unit#)
let () = print_block_kind "all_unit_u" ((#(), #(), #(#(), #())) : all_unit_u)

let equal_t3 ((a, b, #((c, d), #())) : t3)
    ((a', b', #((c', d'), #())) : t3) =
  Int.equal a a' && Float_u.equal b b'
  && Unit.equal c c' && String.equal d d'

let () =
  let reconstructed =
    match (42, #4.0, #(((), "hi"), #())) with
    | (a, b, #((c, d), e)) -> (a, b, #((c, d), e))
  in
  assert (equal_t3 reconstructed (42, #4.0, #(((), "hi"), #())));
  print_block_kind "t3" reconstructed


(* [any] in mixed tuples *)

type ('a : any) t4 = 'a * int

let equal_t4_float ((f, n) : float# t4) ((f', n') : float# t4) =
  Float_u.equal f f' && Int.equal n n'

let () =
  let v4_uniform : int t4 = (6, 7) in
  let v4_mixed : float# t4 = (#6.0, 7) in
  assert (v4_uniform = (6, 7));
  assert (equal_t4_float v4_mixed (#6.0, 7));
  print_block_kind "int t4" v4_uniform;
  print_block_kind "float# t4" v4_mixed
