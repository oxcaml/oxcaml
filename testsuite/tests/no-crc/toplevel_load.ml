(* TEST
 readonly_files = "c.ml";
 setup-ocamlc.byte-build-env;
 flags = "-no-crc";
 module = "c.ml";
 ocamlc.byte;
 flags = "";
 expect;
*)

(* Loading a [-no-crc] object into a toplevel started without [-no-crc]. *)

#directory "ocamlc.byte";;
#load "c.cmo";;
[%%expect{|
The file ocamlc.byte/c.cmo was compiled with -no-crc;
it can only be loaded by a toplevel started with -no-crc.
|}]
