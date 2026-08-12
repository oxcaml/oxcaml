(* TEST
 ocamlc_byte_exit_status = "0";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The attribute is only used on boxed variant constructors whose arguments are
   all void; anywhere else it is a misplaced attribute (warning 53). Expect
   tests run at toplevel and don't surface warning 53, hence this separate
   test. *)

type unit_u : void mod everything

(* A boxed constructor that isn't all-void. *)
type t1 = A of int [@immediate_all_void_constructor] | B

(* A nullary constructor. *)
type t2 = C [@immediate_all_void_constructor] | D

(* A constructor with an inline record. *)
type t3 = E of { x : int } [@immediate_all_void_constructor]

(* An unboxed variant. *)
type t4 = F of unit_u [@immediate_all_void_constructor] [@@unboxed]

(* An unrelated context. *)
let[@immediate_all_void_constructor] f x = x
