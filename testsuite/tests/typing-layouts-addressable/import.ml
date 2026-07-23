(* TEST
 readonly_files = "import_defs.ml";
 flags = "-extension layout_poly_alpha -extension layouts_beta";
 setup-ocamlc.byte-build-env;
 module = "import_defs.ml";
 ocamlc.byte;
 expect;
*)

#directory "ocamlc.byte";;
#load "import_defs.cmo";;

(* Addressability must survive the cmi round trip: checks against imported
   kinds must agree with the same checks in the defining unit. *)

(* [Import_defs.f]'s bound is the join of [bits8] and [bits8 addressable];
   the defining unit accepts both [t8_plain] and [t8_marked] (see
   [in_unit_plain]/[in_unit_marked] there), so the importer must too. *)

let import_plain () = Import_defs.f (assert false : Import_defs.t8_plain)
[%%expect{|
val import_plain : unit -> Import_defs.t8_plain = <fun>
|}]

let import_marked () = Import_defs.f (assert false : Import_defs.t8_marked)
[%%expect{|
val import_marked : unit -> Import_defs.t8_plain = <fun>
|}]

(* [x] and [x addressable] are incomparable for a rigid layout variable
   [x]; both of these inclusions are rejected in the defining unit and
   must also be rejected here. *)

module F (M : Import_defs.S_marked) : Import_defs.S_plain = M
[%%expect{|
module F : functor (M : Import_defs.S_marked) -> Import_defs.S_plain
|}]

module G (M : Import_defs.S_plain) : Import_defs.S_marked = M
[%%expect{|
module G : functor (M : Import_defs.S_plain) -> Import_defs.S_marked
|}]
