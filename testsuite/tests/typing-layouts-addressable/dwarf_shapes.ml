(* TEST
 flags = "-extension layouts_alpha -g -gno-upstream-dwarf -gdwarf-pedantic";
 native;
*)

(* Check that type shapes accept fields at made-addressable kinds: layouts
   recorded in shapes keep their [Addressable] wrappers, so the pedantic
   layout comparisons must accept a wrapped layout wherever they accept its
   body. *)

type t : bits8 addressable

type v = V of t

type t2 : (bits64 & bits8) addressable

type v2 = V2 of t2 * string

(* [@@unboxed] records and variants compare the field layout against the
   expected layout of the whole type. *)

type r_unboxed = { x : t } [@@unboxed]

let use_r (x : r_unboxed) = x

type v_unboxed = V_unboxed of t [@@unboxed]

let use_v (x : v_unboxed) = x

(* Recursive types compare layouts through [Rec_var]. *)

type rec_record =
  { hd : t;
    tl : rec_record option
  }

let use_rec (x : rec_record) = x
