open Stdlib

let () = Printexc.record_backtrace true

[@@@ocaml.warning "-unused-value-declaration"]

(* Arrays whose elements are 512-bit vectors ([int64x8#]/[float64x8#], layout
   [vec512]) or AVX512 mask registers ([mask#], layout [mask]).  These exercise
   the [Punboxedvectorarray Unboxed_vec512] / [Punboxedmaskarray] array kinds:
   allocation, get, set, length and bounds checks, plus the
   [%box_mask]/[%unbox_mask] primitives used to move mask values in and out of
   such arrays. *)

external box_int64x8 : int64x8# -> int64x8 = "%box_vec512"
external unbox_int64x8 : int64x8 -> int64x8# = "%unbox_vec512"

external int64x8_of_int64s :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64x8
  = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external w0 : int64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external w1 : int64x8 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]
external w2 : int64x8 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]
external w3 : int64x8 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]
external w4 : int64x8 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]
external w5 : int64x8 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]
external w6 : int64x8 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]
external w7 : int64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

(* Layout-polymorphic array primitives, instantiated at the unboxed element
   types.  These lower to [Punboxedvectorarray_{ref,set}] and
   [Punboxedmaskarray_{ref,set}] respectively. *)
external v_get : int64x8# array -> int -> int64x8# = "%array_safe_get"
external v_set : int64x8# array -> int -> int64x8# -> unit = "%array_safe_set"
external v_len : int64x8# array -> int = "%array_length"

external box_mask : mask# -> mask = "%box_mask"
external unbox_mask : mask -> mask# = "%unbox_mask"

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

external m_get : mask# array -> int -> mask# = "%array_safe_get"
external m_set : mask# array -> int -> mask# -> unit = "%array_safe_set"
external m_len : mask# array -> int = "%array_length"

let eqi l r = if l <> r then Printf.printf "%016Lx <> %016Lx\n" l r

(* [int64x8#] (vec512-layout) value carrying eight distinct lanes. *)
let vec a b c d e f g h = unbox_int64x8 (int64x8_of_int64s a b c d e f g h)

let check_vec (e : int64x8#) (a : int64x8#) =
  let e = box_int64x8 e and a = box_int64x8 a in
  eqi (w0 e) (w0 a);
  eqi (w1 e) (w1 a);
  eqi (w2 e) (w2 a);
  eqi (w3 e) (w3 a);
  eqi (w4 e) (w4 a);
  eqi (w5 e) (w5 a);
  eqi (w6 e) (w6 a);
  eqi (w7 e) (w7 a)

let assert_oob thunk =
  try
    thunk ();
    Printf.printf "expected index out of bounds\n"
  with
  | Invalid_argument s when s = "index out of bounds" -> ()
  | Invalid_argument s -> failwith s
;;

(* ---- Arrays of [int64x8#] (vec512 layout) ---- *)

let () =
  let v0 = vec 0L 1L 2L 3L 4L 5L 6L 7L in
  let v1 = vec 10L 11L 12L 13L 14L 15L 16L 17L in
  let v2 = vec 20L 21L 22L 23L 24L 25L 26L 27L in
  let a = [| v0; v1; v2 |] in
  if v_len a <> 3 then Printf.printf "bad length %d\n" (v_len a);
  check_vec v0 (v_get a 0);
  check_vec v1 (v_get a 1);
  check_vec v2 (v_get a 2);
  (* set *)
  let v3 = vec 30L 31L 32L 33L 34L 35L 36L 37L in
  v_set a 1 v3;
  check_vec v3 (v_get a 1);
  check_vec v0 (v_get a 0);
  check_vec v2 (v_get a 2);
  (* bounds *)
  assert_oob (fun () -> let _ = v_get a 3 in ());
  assert_oob (fun () -> let _ = v_get a (-1) in ());
  assert_oob (fun () -> v_set a 3 v0);
  assert_oob (fun () -> v_set a (-1) v0)
;;

(* Keep a vec512 array live across many minor collections, exercising the
   non-scannable [Unboxed_vec512] array tag during GC. *)
let () =
  let a =
    (* Assure the array is not statically allocated *)
    let[@inline never] mk x =
      [| vec x 1L 2L 3L 4L 5L 6L 7L;
         vec 1L 2L 3L 4L 5L 6L 7L 8L;
         vec 2L 3L 4L 5L 6L 7L 8L 9L;
         vec 3L 4L 5L 6L 7L 8L 9L 10L;
         vec 4L 5L 6L 7L 8L 9L 10L 11L;
         vec 5L 6L 7L 8L 9L 10L 11L 12L;
         vec 6L 7L 8L 9L 10L 11L 12L 13L;
         vec 7L 8L 9L 10L 11L 12L 13L 14L |]
    in
    mk 0L
  in
  for _ = 1 to 1000 do
    Gc.minor ()
  done;
  Gc.compact ();
  for i = 0 to v_len a - 1 do
    let b = Int64.of_int i in
    check_vec
      (vec b (Int64.add b 1L) (Int64.add b 2L) (Int64.add b 3L)
         (Int64.add b 4L) (Int64.add b 5L) (Int64.add b 6L) (Int64.add b 7L))
      (v_get a i)
  done
;;

(* ---- Arrays of [float64x8#] (vec512 layout) ---- *)

external box_float64x8 : float64x8# -> float64x8 = "%box_vec512"
external unbox_float64x8 : float64x8 -> float64x8# = "%unbox_vec512"

external float64x8_of_int64s :
  int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64 -> int64
  -> float64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external fw0 : float64x8 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]
external fw7 : float64x8 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external f_get : float64x8# array -> int -> float64x8# = "%array_safe_get"
external f_set : float64x8# array -> int -> float64x8# -> unit = "%array_safe_set"

let () =
  let fvec x = unbox_float64x8 (float64x8_of_int64s x x x x x x x x) in
  let a = [| fvec 1L; fvec 2L; fvec 3L |] in
  eqi (fw0 (box_float64x8 (f_get a 0))) 1L;
  eqi (fw7 (box_float64x8 (f_get a 2))) 3L;
  f_set a 0 (fvec 9L);
  eqi (fw0 (box_float64x8 (f_get a 0))) 9L;
  assert_oob (fun () -> let _ = f_get a 3 in ())
;;

(* ---- Arrays of [mask#] (mask layout) ---- *)

let m bits = unbox_mask (mask_of_int64 bits)
let bits_of mu = int64_of_mask (box_mask mu)

(* box/unbox round-trip *)
let () =
  let mu = m (Sys.opaque_identity 0xFEDCBA9876543210L) in
  eqi (bits_of mu) 0xFEDCBA9876543210L
;;

let () =
  let a = [| m 0xAAL; m 0x55L; m 0xF0L; m 0x0FL |] in
  if m_len a <> 4 then Printf.printf "bad length %d\n" (m_len a);
  eqi (bits_of (m_get a 0)) 0xAAL;
  eqi (bits_of (m_get a 1)) 0x55L;
  eqi (bits_of (m_get a 2)) 0xF0L;
  eqi (bits_of (m_get a 3)) 0x0FL;
  (* set *)
  m_set a 1 (m 0x1234L);
  eqi (bits_of (m_get a 1)) 0x1234L;
  eqi (bits_of (m_get a 0)) 0xAAL;
  eqi (bits_of (m_get a 3)) 0x0FL;
  (* bounds *)
  assert_oob (fun () -> let _ = m_get a 4 in ());
  assert_oob (fun () -> let _ = m_get a (-1) in ());
  assert_oob (fun () -> m_set a 4 (m 1L));
  assert_oob (fun () -> m_set a (-1) (m 1L))
;;

(* Masks kept live across minor collections in a (non-scannable) mask array. *)
let () =
  let a =
    (* Assure the array is not statically allocated *)
    let[@inline never] mk x =
      [| m x; m 1L; m 2L; m 3L; m 4L; m 5L; m 6L; m 7L;
         m 8L; m 9L; m 10L; m 11L; m 12L; m 13L; m 14L; m 15L |]
    in
    mk 0L
  in
  for _ = 1 to 1000 do
    Gc.minor ()
  done;
  Gc.compact ();
  for i = 0 to m_len a - 1 do
    eqi (bits_of (m_get a i)) (Int64.of_int i)
  done
;;
