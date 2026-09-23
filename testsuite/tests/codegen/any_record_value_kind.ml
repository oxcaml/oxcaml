(* TEST
 flags += " -O3 -extension layouts_beta";
 only-default-codegen;
 flat-float-array;
 expect.opt;
*)

type ('a : any) t = { field : 'a }

external box_float : float# -> float = "%box_float"
external is_int : 'a -> bool = "%obj_is_int"

let record_is_int (x : _ t) = is_int x
[%%expect_asm X86_64{|
record_is_int:
  movl  $1, %eax
  ret
|}]

type ('a : any) variant = A of 'a | B of int

let variant_is_int (x : _ variant) = is_int x
[%%expect_asm X86_64{|
variant_is_int:
  movl  $1, %eax
  ret
|}]

type ('a : any) option = None | Some of 'a

let option_is_int (x : _ option) = is_int x
[%%expect_asm X86_64{|
option_is_int:
  andl  $1, %eax
  leaq  1(%rax,%rax), %rax
  ret
|}]

let rebuild_int (src : int t) : int t = { field = src.field }
[%%expect_asm X86_64{|
rebuild_int:
  ret
|}]

let rebuild_float (src : float# t) : float# t = { field = src.field }
[%%expect_asm X86_64{|
rebuild_float:
  ret
|}]

(* Keep the array access polymorphic until inlining. *)
let first (r : int array t) =
  let[@inline always] first (type a) (a : a array) = Array.unsafe_get a 0 in
  first r.field
[%%expect_asm X86_64{|
first:
  movq  (%rax), %rax
  movq  (%rax), %rax
  ret
|}]

type _ tuple_repr =
  | Two : (int * int) tuple_repr
  | Three : (int * int * int) tuple_repr

let tuple_join_is_int : type a. a tuple_repr * a -> bool = function
  | Two, x -> is_int x
  | Three, x -> is_int x
[%%expect_asm X86_64{|
tuple_join_is_int:
  andl  $1, %ebx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

type ('a : any) repr = Int : int repr | Float : float# repr

let record_join_is_int : type (a : any). a repr * a t -> bool = function
  | Int, x -> is_int x
  | Float, x -> is_int x
[%%expect_asm X86_64{|
record_join_is_int:
  andl  $1, %ebx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

type float_record = { f : float# }
type int64_record = { i : int64_u }
type _ mixed_repr =
  | Float : float_record mixed_repr
  | Int64 : int64_record mixed_repr

let mixed_join_is_int : type a. a mixed_repr * a -> bool = function
  | Float, x -> is_int x
  | Int64, x -> is_int x
[%%expect_asm X86_64{|
mixed_join_is_int:
  andl  $1, %ebx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

type float_one = { x : float }
type float_two = { y : float; z : float }
type _ float_repr = One : float_one float_repr | Two : float_two float_repr

let float_join_is_int : type a. a float_repr * a -> bool = function
  | One, x -> is_int x
  | Two, x -> is_int x
[%%expect_asm X86_64{|
float_join_is_int:
  andl  $1, %ebx
  leaq  1(%rbx,%rbx), %rax
  ret
|}]

let () =
  assert (not (record_is_int { field = #42.0 }));
  assert (not (variant_is_int (A #42.0)));
  assert (not (variant_is_int (B 42)));
  assert (option_is_int None);
  assert (not (option_is_int (Some #42.0)));
  assert ((rebuild_int { field = 42 }).field = 42);
  assert (box_float (rebuild_float { field = #42.0 }).field = 42.0);
  assert (first { field = [| 42 |] } = 42);
  assert (not (tuple_join_is_int (Two, (1, 2))));
  assert (not (tuple_join_is_int (Three, (1, 2, 3))));
  assert (not (record_join_is_int (Int, { field = 42 })));
  assert (not (record_join_is_int (Float, { field = #42.0 })));
  assert (not (mixed_join_is_int (Float, { f = #42.0 })));
  assert (not (mixed_join_is_int (Int64, { i = #42L })));
  assert (not (float_join_is_int (One, { x = 1.0 })));
  assert (not (float_join_is_int (Two, { y = 1.0; z = 2.0 })))
[%%expect{|
|}]
