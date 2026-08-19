(* TEST
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

module F = Stdlib_upstream_compatible.Float_u
module I32 = Stdlib_upstream_compatible.Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u
module N = Stdlib_upstream_compatible.Nativeint_u

type product = #(int * F.t * string * I64.t)

let check_int expected actual = assert (Int.equal expected actual)

let check_string expected actual = assert (String.equal expected actual)

let check_float_u expected actual =
  assert (Float.equal expected (F.to_float actual))

let check_int32_u expected actual =
  assert (String.equal expected (I32.to_string actual))

let check_int64_u expected actual =
  assert (String.equal expected (I64.to_string actual))

let check_nativeint_u expected actual =
  assert (String.equal expected (N.to_string actual))

let check_product (#(i, f, s, i64) : product) =
  check_int 7 i;
  check_float_u 3.5 f;
  check_string "product" s;
  check_int64_u "9" i64

let[@inline never] return_int () = 11

let[@inline never] return_string () = "value"

let[@inline never] return_float_u () = #1.25

let[@inline never] return_int32_u () = #32l

let[@inline never] return_int64_u () = #64L

let[@inline never] return_nativeint_u () = #128n

let[@inline never] return_product () : product = #(7, #3.5, "product", #9L)

let[@inline never] raise_any : type (a : any). unit -> a =
  fun () -> assert false

let expect_raises_int (f : unit -> int) =
  try
    let _ : int = f () in
    assert false
  with _ -> ()

let expect_raises_string (f : unit -> string) =
  try
    let _ : string = f () in
    assert false
  with _ -> ()

let expect_raises_float_u (f : unit -> F.t) =
  try
    let _ : F.t = f () in
    assert false
  with _ -> ()

let expect_raises_int32_u (f : unit -> I32.t) =
  try
    let _ : I32.t = f () in
    assert false
  with _ -> ()

let expect_raises_int64_u (f : unit -> I64.t) =
  try
    let _ : I64.t = f () in
    assert false
  with _ -> ()

let expect_raises_nativeint_u (f : unit -> N.t) =
  try
    let _ : N.t = f () in
    assert false
  with _ -> ()

let expect_raises_product (f : unit -> product) =
  try
    let _ : product = f () in
    assert false
  with _ -> ()

let raise_as_int : unit -> int = raise_any

let raise_as_string : unit -> string = raise_any

let raise_as_float_u : unit -> F.t = raise_any

let raise_as_int32_u : unit -> I32.t = raise_any

let raise_as_int64_u : unit -> I64.t = raise_any

let raise_as_nativeint_u : unit -> N.t = raise_any

let raise_as_product : unit -> product = raise_any

let () =
  expect_raises_int raise_as_int;
  expect_raises_string raise_as_string;
  expect_raises_float_u raise_as_float_u;
  expect_raises_int32_u raise_as_int32_u;
  expect_raises_int64_u raise_as_int64_u;
  expect_raises_nativeint_u raise_as_nativeint_u;
  expect_raises_product raise_as_product

let[@inline never] forward_unit : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f ()

let[@inline never] forward_after_opaque_branch
    : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> if Sys.opaque_identity true then f () else raise_any ()

let[@inline never] forward_through_argument
    : type (a : any).
      ((unit -> a) -> unit -> a) -> (unit -> a) -> unit -> a =
  fun forward f () -> forward f ()

let[@inline never] choose_forward
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> unit -> a =
  fun b f g () -> if Sys.opaque_identity b then f () else g ()

let[@inline never] raise_or_forward
    : type (a : any). bool -> (unit -> a) -> unit -> a =
  fun b f () -> if Sys.opaque_identity b then raise_any () else f ()

let forward_value : ('a : any). (unit -> 'a) -> unit -> 'a = forward_unit

let forward_as_int : (unit -> int) -> unit -> int = forward_value

let forward_as_string : (unit -> string) -> unit -> string = forward_value

let forward_as_float_u : (unit -> F.t) -> unit -> F.t = forward_value

let forward_as_int32_u : (unit -> I32.t) -> unit -> I32.t = forward_value

let forward_as_int64_u : (unit -> I64.t) -> unit -> I64.t = forward_value

let forward_as_nativeint_u : (unit -> N.t) -> unit -> N.t = forward_value

let forward_as_product : (unit -> product) -> unit -> product = forward_value

let () =
  check_int 11 (forward_as_int return_int ());
  check_string "value" (forward_as_string return_string ());
  check_float_u 1.25 (forward_as_float_u return_float_u ());
  check_int32_u "32" (forward_as_int32_u return_int32_u ());
  check_int64_u "64" (forward_as_int64_u return_int64_u ());
  check_nativeint_u "128" (forward_as_nativeint_u return_nativeint_u ());
  check_product (forward_as_product return_product ())

let () =
  check_int 11 (forward_after_opaque_branch return_int ());
  check_float_u 1.25 (forward_after_opaque_branch return_float_u ());
  check_product (forward_after_opaque_branch return_product ())

let () =
  check_int 11 (forward_through_argument forward_unit return_int ());
  check_float_u 1.25
    (forward_through_argument forward_after_opaque_branch return_float_u ());
  check_product (forward_through_argument forward_unit return_product ())

let () =
  check_int 11 (choose_forward true return_int raise_any ());
  check_string "value" (choose_forward false raise_any return_string ());
  check_float_u 1.25 (choose_forward true return_float_u raise_any ());
  check_product (choose_forward false raise_any return_product ());
  check_int 11 (raise_or_forward false return_int ());
  check_float_u 1.25 (raise_or_forward false return_float_u ());
  expect_raises_product (raise_or_forward true return_product)

type forwarder =
  { run : ('a : any). (unit -> 'a) -> unit -> 'a
  }

let stored_forwarder = { run = forward_unit }

let[@inline never] make_capturing_forwarder tag =
  let tag = Sys.opaque_identity tag in
  { run =
      (fun f () ->
        ignore (Sys.opaque_identity tag);
        forward_unit f ())
  }

let[@inline never] use_poly_forwarder
    (forward : ('a : any). (unit -> 'a) -> unit -> 'a) =
  check_int 11 (forward return_int ());
  check_string "value" (forward return_string ());
  check_float_u 1.25 (forward return_float_u ());
  check_product (forward return_product ())

let () =
  check_int 11 (stored_forwarder.run return_int ());
  check_float_u 1.25 (stored_forwarder.run return_float_u ());
  check_product (stored_forwarder.run return_product ());
  let capturing_forwarder = make_capturing_forwarder "captured" in
  check_string "value" (capturing_forwarder.run return_string ());
  check_int64_u "64" (capturing_forwarder.run return_int64_u ());
  use_poly_forwarder stored_forwarder.run;
  use_poly_forwarder capturing_forwarder.run
