(* TEST
 flags = "-w -181";
 flambda2;
 {
   native;
 } {
   flags = "-w -181 -O3";
   native;
 } {
   flags = "-w -181 -Oclassic";
   native;
 } {
   bytecode;
 }
*)

(* Runtime behaviour of tranche-2 erased representations: construction, match,
   [Obj] identity of payload-unboxed constructors, and [Marshal] round-trips.
   Compiled native (default / -O3 / -Oclassic) and bytecode. *)

(* [@repr immediate] alongside an ordinary boxed constructor. *)
type bignum = Small of int [@repr immediate] | Big of string

let bignum_val = function Small n -> n | Big s -> String.length s

(* immediate + pointer. *)
type int_or_err = I of int [@repr immediate] | E of string [@repr pointer]

let ioe = function I n -> n | E s -> String.length s

(* null + pointer with a boxed-record payload. *)
type box = { a : int; b : string }
type boxed_or_null = Null_ptr [@repr null] | Ptr of box [@repr pointer]

let bon = function Null_ptr -> -1 | Ptr r -> r.a

(* null + immediate + pointer. *)
type three = N [@repr null] | Im of int [@repr immediate] | Pt of string [@repr pointer]

let three_val = function N -> 0 | Im n -> n | Pt s -> String.length s

(* null + immediate + ordinary boxed (the tree). *)
type tree = Empty [@repr null] | Leaf of int [@repr immediate] | Branch of tree * tree

let rec tree_sum = function
  | Empty -> 0
  | Leaf n -> n
  | Branch (l, r) -> tree_sum l + tree_sum r

let () =
  (* immediate construction + match *)
  assert (bignum_val (Small 3) = 3);
  assert (bignum_val (Big "abcd") = 4);
  (* immediate is the raw int: Obj identity with the payload *)
  assert (Obj.repr (Small 7) == Obj.repr 7);
  assert (Obj.is_int (Obj.repr (Small 0)));
  assert (not (Obj.is_int (Obj.repr (Big "x"))));

  (* immediate + pointer *)
  assert (ioe (I 5) = 5);
  assert (ioe (E "xy") = 2);
  let s = "hello" in
  assert (Obj.repr (E s) == Obj.repr s);   (* pointer is the payload identity *)
  assert (Obj.is_int (Obj.repr (I 9)));
  assert (not (Obj.is_int (Obj.repr (E "z"))));

  (* null + pointer, boxed record.  [boxed_or_null] is value_or_null
     (nullable), so [Obj.repr] cannot apply; check payload identity via the
     match instead. *)
  let r = { a = 42; b = "x" } in
  assert (bon (Ptr r) = 42);
  assert (bon Null_ptr = -1);
  assert (match Ptr r with Ptr r' -> r' == r | Null_ptr -> false);

  (* null + immediate + pointer *)
  assert (three_val N = 0);
  assert (three_val (Im 11) = 11);
  assert (three_val (Pt "abcde") = 5);

  (* tree: null + immediate + ordinary boxed *)
  let t = Branch (Leaf 5, Branch (Empty, Leaf 7)) in
  assert (tree_sum t = 12);
  assert (tree_sum Empty = 0);

  (* Marshal round-trips through the erased representations. *)
  let roundtrip (v : bignum) =
    (Marshal.from_string (Marshal.to_string v []) 0 : bignum)
  in
  (match roundtrip (Small 12345) with
   | Small n -> assert (n = 12345)
   | Big _ -> assert false);
  (match roundtrip (Big "hello") with
   | Big s -> assert (s = "hello")
   | Small _ -> assert false);
  let roundtrip_three (v : three) =
    (Marshal.from_string (Marshal.to_string v []) 0 : three)
  in
  (match roundtrip_three (Pt "ptr") with
   | Pt s -> assert (s = "ptr")
   | N | Im _ -> assert false);
  (match roundtrip_three (Im 99) with
   | Im n -> assert (n = 99)
   | N | Pt _ -> assert false)
;;
