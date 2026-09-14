(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_predef_aliases.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_predef_aliases.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* Aliases of predefined types whose shapes carry a uid (structurally defined
   predefs such as [unit], [bool], [option]; unlike e.g. [int]). Occurrences
   of the aliases must resolve to the alias declarations rather than being
   dropped or attributed to the predefs (this regressed once when declaration
   shapes kept the uid of the expanded manifest). *)

type t = unit
let _ : t = ()

type u = bool
let f (x : u) : t = ignore x

type 'a opt = 'a option
let g : int opt -> unit = fun _ -> ()
