(* TEST
   flags = "-extension layouts_alpha";
   readonly_files = "addressable_across_files.mli addressable_across_files.ml";
   setup-ocamlc.byte-build-env;
   module = "addressable_across_files.mli";
   ocamlc.byte;
   module = "addressable_across_files.ml";
   ocamlc.byte;
   module = "test_across_files.ml";
   ocamlc.byte;
*)

(* Consume kinds written with the [addressable] operator through a cmi. *)

open Addressable_across_files

type ok = t_base requires_addressable

type ok2 = t_product requires_addressable

type ok3 = t_abstract requires_addressable
