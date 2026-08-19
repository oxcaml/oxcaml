(* TEST
 readonly_files = "\
   layout_any_return_provider.mli \
   layout_any_return_provider.ml \
   layout_any_return_middle.mli \
   layout_any_return_middle.ml \
 ";
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 flambda2;
 {
   setup-ocamlopt.byte-build-env;
   module = "layout_any_return_provider.mli";
   ocamlopt.byte;
   module = "layout_any_return_provider.ml";
   ocamlopt.byte;
   module = "layout_any_return_middle.mli";
   ocamlopt.byte;
   module = "layout_any_return_middle.ml";
   ocamlopt.byte;
   module = "test.ml";
   ocamlopt.byte;
   module = "";
   program = "test.opt";
   all_modules = "layout_any_return_provider.cmx layout_any_return_middle.cmx test.cmx";
   ocamlopt.byte;
   program = "${test_build_directory}/test.opt";
   run;
 }{
   setup-ocamlc.byte-build-env;
   module = "layout_any_return_provider.mli";
   ocamlc.byte;
   module = "layout_any_return_provider.ml";
   ocamlc.byte;
   module = "layout_any_return_middle.mli";
   ocamlc.byte;
   module = "layout_any_return_middle.ml";
   ocamlc.byte;
   module = "test.ml";
   ocamlc.byte;
   module = "";
   program = "test.byte";
   all_modules = "\
     layout_any_return_provider.cmo \
     layout_any_return_middle.cmo \
     test.cmo \
   ";
   ocamlc.byte;
   program = "${test_build_directory}/test.byte";
   run;
 }
*)

module P = Layout_any_return_provider
module M = Layout_any_return_middle
module F = Stdlib_upstream_compatible.Float_u
module I32 = Stdlib_upstream_compatible.Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u
module N = Stdlib_upstream_compatible.Nativeint_u

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

let check_product (#(i, f, s, i64) : P.product) =
  check_int 17 i;
  check_float_u 2.5 f;
  check_string "cmx" s;
  check_int64_u "19" i64

let[@inline never] return_int () = 101

let[@inline never] return_string () = "signature"

let[@inline never] return_float_u () = #2.25

let[@inline never] return_int32_u () = #132l

let[@inline never] return_int64_u () = #164L

let[@inline never] return_nativeint_u () = #228n

let[@inline never] return_product () : P.product = #(17, #2.5, "cmx", #19L)

let test_forward (forward : ('a : any). (unit -> 'a) -> unit -> 'a) =
  check_int 101 (forward return_int ());
  check_string "signature" (forward return_string ());
  check_float_u 2.25 (forward return_float_u ());
  check_int32_u "132" (forward return_int32_u ());
  check_int64_u "164" (forward return_int64_u ());
  check_nativeint_u "228" (forward return_nativeint_u ());
  check_product (forward return_product ())

let provider_as_int : (unit -> int) -> unit -> int = P.forward

let provider_as_float_u : (unit -> F.t) -> unit -> F.t = P.forward

let provider_as_product : (unit -> P.product) -> unit -> P.product = P.forward

let middle_as_string : (unit -> string) -> unit -> string = M.forward_imported

let middle_as_int64_u : (unit -> I64.t) -> unit -> I64.t =
  M.forward_imported

let middle_as_product : (unit -> P.product) -> unit -> P.product =
  M.forward_imported

let expect_raises_int (f : unit -> int) =
  try
    let _ : int = f () in
    assert false
  with _ -> ()

let expect_raises_float_u (f : unit -> F.t) =
  try
    let _ : F.t = f () in
    assert false
  with _ -> ()

let expect_raises_product (f : unit -> P.product) =
  try
    let _ : P.product = f () in
    assert false
  with _ -> ()

let () =
  check_int 101 (provider_as_int return_int ());
  check_float_u 2.25 (provider_as_float_u return_float_u ());
  check_product (provider_as_product return_product ());
  check_string "signature" (middle_as_string return_string ());
  check_int64_u "164" (middle_as_int64_u return_int64_u ());
  check_product (middle_as_product return_product ())

let () =
  test_forward P.forward;
  test_forward M.forward_imported;
  test_forward P.Direct.run;
  test_forward P.Made.run;
  test_forward M.From_signature.run;
  test_forward M.From_functor.run

let () =
  test_forward P.stored.run;
  let provider_forwarder = P.make "provider" in
  test_forward provider_forwarder.run;
  test_forward M.stored_imported.run;
  test_forward M.made_imported.run

(* Over-application of an imported return-any function whose result
   instantiates to a function type: the full application's result is a
   closure, so Simplify's over-application split must give it a concrete
   value arity rather than the callee's unknown result arity. *)
let () =
  check_int 42 (P.apply1 (fun () x -> x + 1) 41);
  check_int 42 (P.forward (fun () x -> x * 2) () 21);
  let succ_fn = P.apply1 (fun () x -> x + 1) in
  check_int 8 (succ_fn (Sys.opaque_identity 7))

let () =
  check_int 101 (P.choose true return_int P.raise_any ());
  check_string "signature" (P.choose false P.raise_any return_string ());
  check_product (M.choose_imported true return_product P.raise_any ());
  check_float_u 2.25 (M.choose_imported false M.raise_imported return_float_u ())

let () =
  expect_raises_int P.raise_any;
  expect_raises_float_u P.Direct.fail;
  expect_raises_product M.raise_imported;
  expect_raises_product M.From_functor.fail

let () =
  let module Local = P.Make (M.From_signature) in
  test_forward Local.run;
  expect_raises_int Local.fail
