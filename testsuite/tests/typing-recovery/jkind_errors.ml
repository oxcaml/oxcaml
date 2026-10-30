(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

kind_ k

(* Unknown kind modifier *)
type t1 : value non_pointer bogus

(* [box] on an abstract kind *)
type t2 : k box

(* Abstract kind in a product *)
type t3 : k & value

(* [with] on a right kind *)
type ('a : immutable_data with int) t4
let f4 (x : ('a : value mod portable with 'b)) (y : 'b) = ()

(* Both a kind annotation and a kind attribute *)
type t5 : value = int [@@immediate]

(* Unimplemented kind syntax. Unlike the errors above, this drops the type
   declaration, so we don't use these types below. *)
type t6 : kind_of_ int
type t7 : _

(* [t1] to [t5] and [f4] are still bound. *)
type u1 = t1
type u2 = t2
type u3 = t3
type u4 = int t4
let _ = f4
type u5 = t5

(* This error should still be raised, despite the prior errors. *)
let t : string = 10
