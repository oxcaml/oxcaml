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
Line 1, characters 60-61:
1 | module F (M : Import_defs.S_marked) : Import_defs.S_plain = M
                                                                ^
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l. ('a : l addressable). 'a -> 'a end
       is not included in
         Import_defs.S_plain
       Values do not match:
         val g : layout_ l. ('a : l addressable). 'a -> 'a
       is not included in
         val g : layout_ l. ('a : l). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is '_representable_layout_1.
       But the layout of 'a must be a sublayout of
           '_representable_layout_1 addressable.
       File "import_defs.ml", line 24, characters 2-39: Expected declaration
       File "import_defs.ml", line 28, characters 2-51: Actual declaration
|}]

module G (M : Import_defs.S_plain) : Import_defs.S_marked = M
[%%expect{|
Line 1, characters 60-61:
1 | module G (M : Import_defs.S_plain) : Import_defs.S_marked = M
                                                                ^
Error: Signature mismatch:
       Modules do not match:
         sig val g : layout_ l. ('a : l). 'a -> 'a end
       is not included in
         Import_defs.S_marked
       Values do not match:
         val g : layout_ l. ('a : l). 'a -> 'a
       is not included in
         val g : layout_ l. ('a : l addressable). 'a -> 'a
       The type "'a -> 'a" is not compatible with the type "'b -> 'b"
       The layout of 'a is '_representable_layout_2 addressable.
       But the layout of 'a must be a sublayout of '_representable_layout_2.
       File "import_defs.ml", line 28, characters 2-51: Expected declaration
       File "import_defs.ml", line 24, characters 2-39: Actual declaration
|}]
