(* Scalar byte-copy oracles for the access-local positive-offset prototype. *)
external get : bytes -> int -> int64x2# = "%caml_bytes_getu128u#"
external set : bytes -> int -> int64x2# -> unit = "%caml_bytes_setu128u#"
external get_checked : bytes -> int -> int64x2# = "%caml_bytes_getu128#"
external set_checked : bytes -> int -> int64x2# -> unit = "%caml_bytes_setu128#"

let[@inline never] load_positive a out o = set out 0 (get a (o + 16))
let[@inline never] store_positive a out o = set out (o + 16) (get a 0)
let[@inline never] load_checked a out o = set out 0 (get_checked a (o + 16))
let[@inline never] store_checked a out o = set_checked out (o + 16) (get a 0)
let[@inline never] load_negative a out o = set out 0 (get a (o - 16))
let[@inline never] load_wrapped a out o = set out 0 (get a (o + min_int))
let[@inline never] store_wrapped a out o = set out (o + min_int) (get a 0)

let[@inline never] shared_index a b out o =
  let p = o + 16 in
  set out p (get a p);
  set out (p + 16) (get b p)

let[@inline never] guarded a out o =
  if o < -16 || o > Bytes.length a - 32 then invalid_arg "guarded";
  set out 0 (get a (o + 16))

let calls = ref 0
let[@inline never] effectful_offset o = incr calls; o
let[@inline never] effectful a out o =
  let p = effectful_offset o + 16 in
  set out p (get a p)

let[@inline never] ordinary_conversion o = Int64.of_int (o + 16)

let[@inline never] shared_conversion a out o =
  let p = o + 16 in
  (try set out 0 (get_checked a p) with Invalid_argument _ -> ());
  Int64.of_int p

let scalar_copy src source_offset dst dest_offset =
  let saved = Array.init 16 (fun i -> Bytes.get src (source_offset + i)) in
  Array.iteri (fun i c -> Bytes.set dst (dest_offset + i) c) saved

let input n seed =
  Bytes.init n (fun i -> Char.chr ((i * 17 + i / 256 + seed) land 255))

let expect_invalid f =
  match f () with
  | () -> failwith "expected bounds failure"
  | exception Invalid_argument _ -> ()

let () =
  let a = input 128 3 and b = input 128 29 in
  List.iter (fun o ->
    List.iter (fun load ->
      let out = input 128 91 in
      let expected = Bytes.copy out in
      scalar_copy a (o + 16) expected 0;
      load a out o;
      assert (Bytes.equal out expected)) [load_positive; load_checked; guarded];
    List.iter (fun store ->
      let out = input 128 91 in
      let expected = Bytes.copy out in
      scalar_copy a 0 expected (o + 16);
      store a out o;
      assert (Bytes.equal out expected)) [store_positive; store_checked];
    (* Actual source/destination overlap, with a snapshot scalar oracle. *)
    let alias = Bytes.copy a in
    let expected = Bytes.copy a in
    scalar_copy expected (o + 16) expected 0;
    load_positive alias alias o;
    assert (Bytes.equal alias expected);
    let alias = Bytes.copy a in
    let expected = Bytes.copy a in
    scalar_copy expected 0 expected (o + 16);
    store_positive alias alias o;
    assert (Bytes.equal alias expected)) [-16; -15; -1; 0; 1; 15; 16; 63; 95; 96];
  List.iter (fun o ->
    let out = input 128 91 in
    let expected = Bytes.copy out in
    scalar_copy a (o + 16) expected (o + 16);
    scalar_copy b (o + 16) expected (o + 32);
    shared_index a b out o;
    assert (Bytes.equal out expected)) [-16; -1; 0; 1; 63; 80];
  List.iter (fun o ->
    let out = input 128 91 in
    let expected = Bytes.copy out in
    scalar_copy a (o - 16) expected 0;
    load_negative a out o;
    assert (Bytes.equal out expected)) [16; 17; 31; 32; 128];
  List.iter (fun f ->
    let out = input 128 91 in
    let expected = Bytes.copy out in
    scalar_copy a 0 expected 0;
    f a out min_int;
    assert (Bytes.equal out expected)) [load_wrapped; store_wrapped];
  List.iter (fun f ->
    List.iter (fun o ->
      let out = input 128 91 in
      let before = Bytes.copy out in
      expect_invalid (fun () -> f a out o);
      assert (Bytes.equal out before))
      [min_int; -17; 97; max_int - 15; max_int])
    [load_checked; store_checked];
  let out = input 128 91 in
  calls := 0;
  effectful a out 3;
  assert (!calls = 1);
  assert (Bytes.sub out 19 16 = Bytes.sub a 19 16);
  assert (ordinary_conversion (Sys.opaque_identity (max_int - 15))
          = -4611686018427387904L);
  List.iter (fun o ->
    let out = input 128 91 in
    let expected = Bytes.copy out in
    let p = o + 16 in
    if p >= 0 && p <= 112 then scalar_copy a p expected 0;
    assert (shared_conversion a out o = Int64.of_int p);
    assert (Bytes.equal out expected))
    (Sys.opaque_identity [-16; 0; 96; 97; max_int - 15; max_int]);
  List.iter (fun n ->
    let short = input n 17 in
    let out = input 128 91 in
    if n < 16 then begin
      expect_invalid (fun () -> load_checked short out (-16));
      expect_invalid (fun () -> store_checked a short (-16))
    end else begin
      load_checked short out (-16);
      assert (Bytes.sub out 0 16 = Bytes.sub short 0 16);
      store_checked a short (-16);
      assert (Bytes.sub short 0 16 = Bytes.sub a 0 16)
    end) [0; 1; 15; 16; 17];
  print_endline "Positive/negative offsets, checked failures, shared indices, aliasing, effect count and wrapped non-memory conversion passed."
