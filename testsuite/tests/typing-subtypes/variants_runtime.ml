(* TEST
 flags = "-extension subtypes";
*)

(* Runtime behavior of variant subtypes: coercions are no-ops, constructors
   inherit the supertype's (possibly sparse) tags, and pattern matching
   compiles correctly on sparse tag sets. *)

type letter = A | B | C | D | E
type vowel :> letter = A | E
type consonant :> letter = B | C | D
type semicircular_consonant :> consonant = C

type shape = Pt | Ln of int | Sq of int * int | Cr of { r : int } | Dot
type round :> shape = Ln of int | Cr of { r : int } | Dot

let check name b =
  print_string name;
  print_string (if b then ": OK" else ": FAIL");
  print_newline ()

(* Coercion preserves identity *)
let () =
  let v : vowel = E in
  check "coerce identity" ((v : vowel :> letter) = (E : letter))

(* Transitive coercion through a chain of subtypes *)
let () =
  let c : semicircular_consonant = C in
  check "transitive coerce"
    ((c : semicircular_consonant :> letter) = (C : letter))

(* Total match on vowel values (sparse constant tags 0 and 4) *)
let vowel_name (v : vowel) = match v with A -> "A" | E -> "E"

let () =
  List.iter
    (fun v ->
      print_string "vowel match: ";
      print_string (vowel_name v);
      print_newline ())
    ([A; E] : vowel list)

(* Total match on round values, extracting payloads: block tags are
   sparse (Ln = 0, Cr = 2) and so are constant tags (Dot = 1) *)
let round_value (x : round) =
  match x with Ln n -> n | Cr { r } -> r | Dot -> -1

let () = check "round Ln" (round_value (Ln 5) = 5)
let () = check "round Cr" (round_value (Cr { r = 7 }) = 7)
let () = check "round Dot" (round_value Dot = -1)

(* Coerced block constructor compares equal to the supertype's *)
let () =
  check "coerce block equal"
    (((Ln 5 : round) : round :> shape) = (Ln 5 : shape))

(* Structural compare sees the inherited tags: E (4) > A (0) *)
let () = check "compare vowel" (compare (E : vowel) (A : vowel) > 0)

(* Inherited runtime representations, observed via Obj *)
let () = check "repr vowel E" ((Obj.magic (E : vowel) : int) = 4)
let () = check "repr round Dot" ((Obj.magic (Dot : round) : int) = 1)
let () = check "tag round Cr" (Obj.tag (Obj.repr (Cr { r = 1 } : round)) = 2)

(* Marshalling round-trip of a block constructor with a sparse tag *)
let () =
  let s = Marshal.to_string (Ln 5 : round) [] in
  check "marshal round-trip" ((Marshal.from_string s 0 : round) = Ln 5)

(* A big supertype, so subtypes can have many constructors with sparse
   tags *)
type big =
  | K00 | K01 | K02 | K03 | K04 | K05 | K06 | K07 | K08 | K09
  | K10 | K11 | K12 | K13 | K14 | K15 | K16 | K17 | K18 | K19
  | K20 | K21 | K22 | K23 | K24 | K25 | K26 | K27 | K28 | K29
  | K30 | K31 | K32 | K33 | K34 | K35 | K36 | K37 | K38 | K39
  | V0 of int
  | V1 of int

type bigsub :> big = K00 | K05 | K37 | V1 of int

(* Partial matches with defaults on a small, very sparse subtype *)
let f (x : bigsub) = match x with K00 -> 0 | _ -> 1

let () =
  List.iter
    (fun x ->
      print_string "bigsub f: ";
      print_int (f x);
      print_newline ())
    ([K00; K05; K37; V1 3] : bigsub list)

let g (x : bigsub) = match x with K05 -> 5 | K37 -> 37 | _ -> -1

let () =
  List.iter
    (fun x ->
      print_string "bigsub g: ";
      print_int (g x);
      print_newline ())
    ([K00; K05; K37; V1 9] : bigsub list)

(* A subtype with 34 constructors, so a one-armed match leaves >= 32
   constructors for the wildcard: exercises the switch-compilation
   default-action path on sparse inherited tags *)
type bigsub2 :> big =
  | K00 | K01 | K02 | K04 | K05 | K06 | K08 | K09
  | K10 | K11 | K12 | K14 | K15 | K16 | K17 | K18 | K19
  | K20 | K22 | K23 | K24 | K25 | K26 | K27 | K28
  | K30 | K31 | K32 | K33 | K34 | K36 | K37 | K38 | K39

let h (x : bigsub2) = match x with K00 -> true | _ -> false

let () = check "bigsub2 h K00" (h K00)
let () = check "bigsub2 h K04" (not (h K04))
let () = check "bigsub2 h K39" (not (h K39))

(* Payloads of various shapes in a sparse-tagged subtype: a float, a mixed
   block (int + float), and a mutable inline record. PInt (block tag 0) is
   dropped, so the kept block tags 1/2/3 are sparse. *)
type payload =
  | PInt of int
  | PFloat of float
  | PMixed of { i : int; f : float }
  | PMut of { mutable m : int }
  | PNone

type psub :> payload =
  | PFloat of float
  | PMixed of { i : int; f : float }
  | PMut of { mutable m : int }
  | PNone

let () =
  check "psub PFloat tag" (Obj.tag (Obj.repr (PFloat 1.0 : psub)) = 1);
  check "psub PMixed tag"
    (Obj.tag (Obj.repr (PMixed { i = 1; f = 2.0 } : psub)) = 2);
  check "psub PMut tag" (Obj.tag (Obj.repr (PMut { m = 1 } : psub)) = 3);
  check "psub PNone repr" ((Obj.magic (PNone : psub) : int) = 0)

let psub_show (x : psub) =
  match x with
  | PFloat f -> "F" ^ string_of_float f
  | PMixed { i; f } -> "M" ^ string_of_int i ^ "/" ^ string_of_float f
  | PMut { m } -> "R" ^ string_of_int m
  | PNone -> "N"

let () =
  List.iter
    (fun x ->
      print_string "psub: "; print_string (psub_show x); print_newline ())
    ([PFloat 3.5; PMixed { i = 2; f = 1.5 }; PMut { m = 10 }; PNone]
     : psub list)

(* Coercion is a no-op for float and mixed-block payloads too *)
let () =
  check "coerce float payload"
    (((PFloat 3.5 : psub) : psub :> payload) = (PFloat 3.5 : payload));
  check "coerce mixed payload"
    (match ((PMixed { i = 2; f = 1.5 } : psub) : psub :> payload) with
     | PMixed { i; f } -> i = 2 && f = 1.5
     | _ -> false)

(* Mutating a mutable payload is visible through the coerced view (the
   coercion shares the block) *)
let () =
  let r = (PMut { m = 0 } : psub) in
  (match r with PMut s -> s.m <- 42 | _ -> ());
  check "mutate payload"
    (match (r : psub :> payload) with PMut { m } -> m = 42 | _ -> false)

(* More direct tag-value assertions on sparse inherited tags *)
let () =
  check "repr vowel A" ((Obj.magic (A : vowel) : int) = 0);
  check "tag round Ln" (Obj.tag (Obj.repr (Ln 5 : round)) = 0);
  check "repr bigsub K05" ((Obj.magic (K05 : bigsub) : int) = 5);
  check "repr bigsub K37" ((Obj.magic (K37 : bigsub) : int) = 37);
  check "tag bigsub V1" (Obj.tag (Obj.repr (V1 3 : bigsub)) = 1)

(* Parameterized subtype: kept block constructors keep their tags (L = 0,
   N = 1; the dropped constant M would be const tag 0) *)
type 'a s = L of 'a | M | N of 'a * int
type 'a t :> 'a s = L of 'a | N of 'a * int

let () =
  check "tag param L" (Obj.tag (Obj.repr (L 0 : int t)) = 0);
  check "tag param N" (Obj.tag (Obj.repr (N (0, 0) : int t)) = 1)
