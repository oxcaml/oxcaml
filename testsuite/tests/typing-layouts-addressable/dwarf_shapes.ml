(* TEST
 flags = "-g -gno-upstream-dwarf -gdwarf-pedantic";
 native;
*)

(* A compile-only regression test: [-gdwarf-pedantic] makes the layout
   consistency checks in the DWARF shape machinery fatal. Layouts recorded in
   shapes keep their [Addressable] wrappers, so those checks must accept a
   wrapped layout wherever they accept its body. (Tests of actual debugger
   output live in oxcaml/tests/backend/oxcaml_dwarf, gated on a custom
   lldb.) *)

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

(* Mixed constructors: a [void] field makes the constructor mixed. *)

type void_addr : void addressable

type v_void = V_void of void_addr * string

let use_v_void (x : v_void) = x
