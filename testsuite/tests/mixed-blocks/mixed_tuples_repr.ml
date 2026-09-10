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

type nested_unboxed_tuple_gets_flattened = int * #(string * bool)
type void_void_int = unit# * unit# * int
type large_nested_mixed = int * float# * #((unit * string) * unit#)

let equal_flattened
    ((x, #(y, z)) : nested_unboxed_tuple_gets_flattened)
    ((x', #(y', z')) : nested_unboxed_tuple_gets_flattened) =
  Int.equal x x' && String.equal y y' && Bool.equal z z'

(* Uniform block after flattening *)
let () =
  let reconstructed =
    match 4, #("hi", false) with
    | x, #(y, z) -> x, #(y, z)
  in
  assert (equal_flattened reconstructed (4, #("hi", false)));
  print_block_kind "nested_unboxed_tuple_gets_flattened" reconstructed

(* Singleton block *)
let () =
  print_block_kind "void_void_int" ((#(), #(), 42) : void_void_int)

(* all void mixed tuple *)

type all_unit_u = unit# * unit# * #(unit# * unit#)
let () = print_block_kind "all_unit_u" ((#(), #(), #(#(), #())) : all_unit_u)

let equal_large ((a, b, #((c, d), #())) : large_nested_mixed)
    ((a', b', #((c', d'), #())) : large_nested_mixed) =
  Int.equal a a' && Float_u.equal b b'
  && Unit.equal c c' && String.equal d d'

let () =
  let reconstructed =
    match (42, #4.0, #(((), "hi"), #())) with
    | (a, b, #((c, d), e)) -> (a, b, #((c, d), e))
  in
  assert (equal_large reconstructed (42, #4.0, #(((), "hi"), #())));
  print_block_kind "large_nested_mixed" reconstructed


(* [any] in mixed tuples *)

type ('a : any) any_and_int = 'a * int

let equal_any_float ((f, n) : float# any_and_int)
    ((f', n') : float# any_and_int) =
  Float_u.equal f f' && Int.equal n n'

let () =
  let uniform : int any_and_int = (6, 7) in
  let mixed : float# any_and_int = (#6.0, 7) in
  assert (uniform = (6, 7));
  assert (equal_any_float mixed (#6.0, 7));
  print_block_kind "mixed any tuple inst with int" uniform;
  print_block_kind "mixed any tuple inst with float#" mixed
