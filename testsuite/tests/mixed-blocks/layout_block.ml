(* TEST
 flags = "-extension layouts_alpha";
 {
   native;
 }{
   bytecode;
 }
*)

(* Mixed blocks with more than fifteen values before their flat fields.
   In native code such a block starts with a layout word, pointing to a
   layout block which says which words the GC may scan, and its fields
   follow that. *)

external float_u_of_float : float -> float# = "%unbox_float"
external float_of_float_u : float# -> float = "%box_float"

external[@layout_poly] get_idx_mut :
  'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b = "%get_idx"
external[@layout_poly] set_idx_mut :
  'a ('b : any). 'a -> ('a, 'b) idx_mut -> 'b -> unit = "%set_idx"

let failures = ref 0

let check what ok =
  if not ok
  then begin
    incr failures;
    Printf.printf "FAILED: %s\n%!" what
  end

(* A fresh string, which only a correctly scanned block keeps alive. *)
let str i = String.concat "" ["s"; string_of_int i]

let stir () =
  ignore (Sys.opaque_identity (Array.init 5000 str));
  Gc.full_major ();
  Gc.compact ()

(* Twenty values, then flat fields. *)
type r =
  { v0 : string; v1 : string; v2 : string; v3 : string; v4 : string;
    v5 : string; v6 : string; v7 : string; v8 : string; v9 : string;
    v10 : string; v11 : string; v12 : string; v13 : string; v14 : string;
    v15 : string; v16 : string; v17 : string; v18 : string;
    mutable v19 : string;
    f : float#;
    mutable i : float#;
    mutable a : int [@atomic];
    g : float# }

let[@inline never] make n =
  { v0 = str n; v1 = str (n + 1); v2 = str (n + 2); v3 = str (n + 3);
    v4 = str (n + 4); v5 = str (n + 5); v6 = str (n + 6); v7 = str (n + 7);
    v8 = str (n + 8); v9 = str (n + 9); v10 = str (n + 10);
    v11 = str (n + 11); v12 = str (n + 12); v13 = str (n + 13);
    v14 = str (n + 14); v15 = str (n + 15); v16 = str (n + 16);
    v17 = str (n + 17); v18 = str (n + 18); v19 = str (n + 19);
    f = float_u_of_float (float_of_int n);
    i = float_u_of_float (float_of_int (n + 1));
    a = n + 2;
    g = float_u_of_float (float_of_int (n + 3)) }

let[@inline never] check_r what (r : r) n ~v19 ~i ~a =
  let values =
    [| r.v0; r.v1; r.v2; r.v3; r.v4; r.v5; r.v6; r.v7; r.v8; r.v9; r.v10;
       r.v11; r.v12; r.v13; r.v14; r.v15; r.v16; r.v17; r.v18 |]
  in
  Array.iteri
    (fun k v -> check (Printf.sprintf "%s: v%d" what k) (v = str (n + k)))
    values;
  check (what ^ ": v19") (r.v19 = v19);
  check (what ^ ": f") (float_of_float_u r.f = float_of_int n);
  check (what ^ ": i") (float_of_float_u r.i = float_of_int i);
  check (what ^ ": a") (r.a = a);
  check (what ^ ": g") (float_of_float_u r.g = float_of_int (n + 3))

let test_heap () =
  let rs = Array.init 100 (fun k -> make (100 * k)) in
  stir ();
  Array.iteri
    (fun k r ->
      let n = 100 * k in
      check_r "heap" r n ~v19:(str (n + 19)) ~i:(n + 1)
        ~a:(n + 2))
    rs;
  Array.iteri
    (fun k r ->
      r.v19 <- str (k + 7);
      r.i <- float_u_of_float (float_of_int (k + 8));
      r.a <- k + 9)
    rs;
  stir ();
  Array.iteri
    (fun k r ->
      check_r "heap, after writes" r (100 * k) ~v19:(str (k + 7))
        ~i:(k + 8) ~a:(k + 9))
    rs

(* A constant block, allocated statically in native code. *)
let static =
  { v0 = "s0"; v1 = "s1"; v2 = "s2"; v3 = "s3"; v4 = "s4"; v5 = "s5";
    v6 = "s6"; v7 = "s7"; v8 = "s8"; v9 = "s9"; v10 = "s10"; v11 = "s11";
    v12 = "s12"; v13 = "s13"; v14 = "s14"; v15 = "s15"; v16 = "s16";
    v17 = "s17"; v18 = "s18"; v19 = "s19";
    f = #0.; i = #1.; a = 2; g = #3. }

let test_static () =
  stir ();
  check_r "static" (Sys.opaque_identity static) 0 ~v19:"s19" ~i:1 ~a:2

let test_local () =
  let local_ r = make 500 in
  stir ();
  check "local: f" (float_of_float_u r.f = 500.);
  check "local: v0" (r.v0 = str 500);
  check "local: v19" (r.v19 = str 519);
  check "local: g" (float_of_float_u r.g = 503.)

(* A constructor argument, so the block has a non-zero tag. *)
type v =
  | A
  | B of string * string * string * string * string * string * string
         * string * string * string * string * string * string * string
         * string * string * string * float# * string
  | C of float#

let[@inline never] make_b n =
  B (str n, str (n + 1), str (n + 2), str (n + 3), str (n + 4), str (n + 5),
     str (n + 6), str (n + 7), str (n + 8), str (n + 9), str (n + 10),
     str (n + 11), str (n + 12), str (n + 13), str (n + 14), str (n + 15),
     str (n + 16), float_u_of_float (float_of_int n), str (n + 17))

let test_variant () =
  let vs = Array.init 100 (fun k -> if k mod 3 = 0 then A else make_b k) in
  stir ();
  Array.iteri
    (fun k v ->
      match v with
      | A -> check "variant: A" (k mod 3 = 0)
      | C _ -> check "variant: C" false
      | B (s0, _, _, _, _, _, _, _, _, _, _, _, _, _, _, s15, s16, f, s17) ->
        check "variant: B"
          (k mod 3 <> 0
           && s0 = str k && s15 = str (k + 15) && s16 = str (k + 16)
           && s17 = str (k + 17) && float_of_float_u f = float_of_int k))
    vs

(* Block indices are offsets from the start of the block, so allow for the
   layout word. *)
type ix =
  { x0 : string; x1 : string; x2 : string; x3 : string; x4 : string;
    x5 : string; x6 : string; x7 : string; x8 : string; x9 : string;
    x10 : string; x11 : string; x12 : string; x13 : string; x14 : string;
    x15 : string; x16 : string;
    mutable xv : string;
    mutable xf : float#;
    mutable xp : #(string * float#);
    xg : float# }

let[@inline never] make_ix n =
  { x0 = str n; x1 = str (n + 1); x2 = str (n + 2); x3 = str (n + 3);
    x4 = str (n + 4); x5 = str (n + 5); x6 = str (n + 6); x7 = str (n + 7);
    x8 = str (n + 8); x9 = str (n + 9); x10 = str (n + 10);
    x11 = str (n + 11); x12 = str (n + 12); x13 = str (n + 13);
    x14 = str (n + 14); x15 = str (n + 15); x16 = str (n + 16);
    xv = str (n + 17);
    xf = float_u_of_float (float_of_int n);
    xp = #(str (n + 18), float_u_of_float (float_of_int (n + 1)));
    xg = float_u_of_float (float_of_int (n + 2)) }

let test_idx () =
  let r = make_ix 700 in
  stir ();
  check "idx: get xv" (get_idx_mut r (.xv) = str 717);
  check "idx: get xf" (float_of_float_u (get_idx_mut r (.xf)) = 700.);
  let #(s, f) = get_idx_mut r (.xp) in
  check "idx: get xp" (s = str 718 && float_of_float_u f = 701.);
  set_idx_mut r (.xv) (str 1);
  set_idx_mut r (.xf) #2.;
  set_idx_mut r (.xp) #(str 3, #4.);
  stir ();
  let #(s, f) = r.xp in
  check "idx: set"
    (r.x0 = str 700 && r.x16 = str 716 && r.xv = str 1
     && float_of_float_u r.xf = 2. && s = str 3 && float_of_float_u f = 4.
     && float_of_float_u r.xg = 702.)

(* Recursive values: the block is first allocated as a dummy by the
   runtime, which gives it a layout block of its own until the block is
   filled in. A collection in between must cope with that. *)
let[@inline never] gc_str n =
  Gc.full_major ();
  str n

type c =
  { c0 : string; c1 : string; c2 : string; c3 : string; c4 : string;
    c5 : string; c6 : string; c7 : string; c8 : string; c9 : string;
    c10 : string; c11 : string; c12 : string; c13 : string; c14 : string;
    c15 : string; c16 : string;
    self : c;
    cf : float# }

let[@inline never] make_c n =
  let rec c =
    { c0 = gc_str n; c1 = str (n + 1); c2 = str (n + 2); c3 = str (n + 3);
      c4 = str (n + 4); c5 = str (n + 5); c6 = str (n + 6); c7 = str (n + 7);
      c8 = str (n + 8); c9 = str (n + 9); c10 = str (n + 10);
      c11 = str (n + 11); c12 = str (n + 12); c13 = str (n + 13);
      c14 = str (n + 14); c15 = str (n + 15); c16 = str (n + 16);
      self = c;
      cf = float_u_of_float (float_of_int n) }
  in
  c

let test_rec () =
  let cs = Array.init 10 make_c in
  stir ();
  Array.iteri
    (fun k c ->
      check "rec"
        (c.self == c && c.self.c0 = str k && c.c16 = str (k + 16)
         && float_of_float_u c.self.cf = float_of_int k))
    cs

type f8 =
  #(float# * float# * float# * float# * float# * float# * float# * float#)

type f64 = #(f8 * f8 * f8 * f8 * f8 * f8 * f8 * f8)

(* A recursive value with a long flat suffix, for which the layout block
   uses a repeat. *)
type d =
  { d0 : string; d1 : string; d2 : string; d3 : string; d4 : string;
    d5 : string; d6 : string; d7 : string; d8 : string; d9 : string;
    d10 : string; d11 : string; d12 : string; d13 : string; d14 : string;
    d15 : string; d16 : string;
    dself : d;
    dw : f64;
    df : float# }

let[@inline never] make_d n =
  let f = float_u_of_float (float_of_int n) in
  let f8 : f8 = #(f, f, f, f, f, f, f, f) in
  let f64 : f64 = #(f8, f8, f8, f8, f8, f8, f8, f8) in
  let rec d =
    { d0 = gc_str n; d1 = str (n + 1); d2 = str (n + 2); d3 = str (n + 3);
      d4 = str (n + 4); d5 = str (n + 5); d6 = str (n + 6); d7 = str (n + 7);
      d8 = str (n + 8); d9 = str (n + 9); d10 = str (n + 10);
      d11 = str (n + 11); d12 = str (n + 12); d13 = str (n + 13);
      d14 = str (n + 14); d15 = str (n + 15); d16 = str (n + 16);
      dself = d;
      dw = f64;
      df = float_u_of_float (float_of_int (n + 1)) }
  in
  d

let test_rec_long () =
  let ds = Array.init 10 make_d in
  stir ();
  Array.iteri
    (fun k d ->
      let #(_, _, _, _, _, _, _, #(_, _, _, _, _, _, _, w)) = d.dw in
      check "rec, long"
        (d.dself == d && d.dself.d0 = str k && d.d16 = str (k + 16)
         && float_of_float_u w = float_of_int k
         && float_of_float_u d.df = float_of_int (k + 1)))
    ds

(* A block too large for the minor heap. *)
type big =
  { b0 : string; b1 : string; b2 : string; b3 : string; b4 : string;
    b5 : string; b6 : string; b7 : string; b8 : string; b9 : string;
    b10 : string; b11 : string; b12 : string; b13 : string; b14 : string;
    b15 : string; b16 : string; b17 : string;
    w : f64; x : f64; y : f64; z : f64;
    last : float# }

let[@inline never] make_big n =
  let f = float_u_of_float (float_of_int n) in
  let f8 : f8 = #(f, f, f, f, f, f, f, f) in
  let f64 : f64 = #(f8, f8, f8, f8, f8, f8, f8, f8) in
  { b0 = str n; b1 = str (n + 1); b2 = str (n + 2); b3 = str (n + 3);
    b4 = str (n + 4); b5 = str (n + 5); b6 = str (n + 6); b7 = str (n + 7);
    b8 = str (n + 8); b9 = str (n + 9); b10 = str (n + 10);
    b11 = str (n + 11); b12 = str (n + 12); b13 = str (n + 13);
    b14 = str (n + 14); b15 = str (n + 15); b16 = str (n + 16);
    b17 = str (n + 17);
    w = f64; x = f64; y = f64; z = f64;
    last = float_u_of_float (float_of_int (n + 1)) }

let test_big () =
  let bs = Array.init 20 make_big in
  stir ();
  Array.iteri
    (fun k b ->
      let #(_, _, _, _, _, _, _, #(_, _, _, _, _, _, _, z)) = b.z in
      let #(#(w, _, _, _, _, _, _, _), _, _, _, _, _, _, _) = b.w in
      check "big"
        (b.b0 = str k && b.b17 = str (k + 17)
         && float_of_float_u w = float_of_int k
         && float_of_float_u z = float_of_int k
         && float_of_float_u b.last = float_of_int (k + 1)))
    bs

let () =
  test_heap ();
  test_static ();
  test_local ();
  test_variant ();
  test_idx ();
  test_rec ();
  test_rec_long ();
  test_big ();
  if !failures = 0 then print_endline "ok"
