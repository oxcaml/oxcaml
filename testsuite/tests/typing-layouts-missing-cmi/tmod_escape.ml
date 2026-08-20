(* TEST
 readonly_files = "tmod_m.ml tmod_defs.ml";
 setup-ocamlc.byte-build-env;
 module = "tmod_m.ml";
 ocamlc.byte;
 module = "tmod_defs.ml";
 ocamlc.byte;
 script = "rm -f tmod_m.cmi";
 script;
 expect;
*)

(* The unboxed representation of [Tmod_m.s Tmod_defs.t] is [Tmod_m.s] under the
   constructor-local bound (a [Tmod] node). With tmod_m.cmi removed, expansion
   bottoms out at the missing cmi, and the fallback result must be a type with
   a usable cached kind (the [t] application itself), not the in-progress
   [Tmod] wrapper, whose kind can only be estimated as [any]. *)

#directory "ocamlc.byte";;
#load "tmod_defs.cmo";;

(* Passing the value through works with or without the cmi. *)
let f x = Tmod_defs.mk x;;
[%%expect {|
val f : Tmod_m.s Tmod_defs.t -> Tmod_m.s Tmod_defs.t = <fun>
|}]

(* Using it as an array element requires a representable layout, which is
   satisfied by the declared kind of [Tmod_defs.t]. *)
let g a = Tmod_defs.mk a.(0);;
[%%expect {|
Line 1, characters 23-24:
1 | let g a = Tmod_defs.mk a.(0);;
                           ^
Error: A representable layout is required here.
       The layout of (Tmod_m.s @@ portable) is any
         because the .cmi file for Tmod_m.s is missing.
       But the layout of (Tmod_m.s @@ portable) must be representable
         because it's the type of an array element.
       No .cmi file found containing Tmod_m.s.
       Hint: Adding "tmod_m" to your dependencies might help.
|}]
