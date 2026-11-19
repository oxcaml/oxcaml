(* TEST
 flags = "-S";
 arch_arm64;
 reference = "${test_source_directory}/arm64_store_release.reference";
 native;
*)

(* Regression test for the arm64 store codegen (oxcaml #6538/#7790).

   Non-value 64-bit stores (Bytes/Buffer/Bigarray) are raw data outside the
   OCaml value model and need no memory-model fence: they compile to a plain
   [str], with neither the [dmb ishld] barrier nor the store-release [stlr].
   Ordinary assignments to a mutable value field keep the [stlr].

   The companion .run script checks the emitted assembly: no [dmb ishld]
   anywhere, no [stlr] in the Bytes/Bigarray stores, and [stlr] still present
   for the value store. *)

let store_bytes b i v = Bytes.set_int64_le b i v
[@@inline never]

let store_bigarray
      (a : (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t)
      i v =
  a.{i} <- v
[@@inline never]

let store_buffer buf v = Buffer.add_int64_le buf v
[@@inline never]

let store_value (r : int ref) v = r := v
[@@inline never]

let () =
  let b = Bytes.create 16 in
  store_bytes b 0 42L;
  let a = Bigarray.Array1.create Bigarray.int64 Bigarray.c_layout 2 in
  store_bigarray a 0 7L;
  let buf = Buffer.create 16 in
  store_buffer buf 3L;
  let r = ref 0 in
  store_value r 5;
  assert (Bytes.get_int64_le b 0 = 42L);
  assert (a.{0} = 7L);
  assert (Buffer.length buf = 8);
  assert (!r = 5)
