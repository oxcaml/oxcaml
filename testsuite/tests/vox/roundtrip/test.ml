(* TEST
 readonly_files = "roundtrip_defs.mli";
 setup-ocamlc.byte-build-env;
 module = "roundtrip_defs.mli";
 ocamlc.byte;
 expect;
*)

(* Write a refined signature to a .cmi, read it back, print it.  The
   cheapest guard on the representation: a [Subst] or [Btype] gap shows up
   here first. *)

#directory "ocamlc.byte";;

#show Roundtrip_defs;;
[%%expect{|
module Roundtrip_defs :
  sig
    type nat = int{ _ >= 0 }
    type dep = x:int{ x > 0 } -> int{ _ >= x }
    val sub : s:string -> int{ _ < (String.length s) } -> char
    val labelled : ~x:int{ x > 0 } -> unit
    type wf = { size : int{ _ >= 0 }; }
    type pos = Pos of int{ _ > 0 }
  end
|}]

(* The imported types are the same types: unification across the .cmi,
   with binders freshened on import *)
let l : (x:int{ x > 0 } -> int{ _ >= x }) list = ([] : Roundtrip_defs.dep list);;
[%%expect{|
val l : (x:int{ x > 0 } -> int{ _ >= x }) list = []
|}]

let l : int{ _ >= 0 } list = ([] : Roundtrip_defs.nat list);;
[%%expect{|
val l : int{ _ >= 0 } list = []
|}]
