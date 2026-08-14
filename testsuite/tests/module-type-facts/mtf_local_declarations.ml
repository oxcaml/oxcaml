(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "mtf_local_declarations.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -decls mtf_local_declarations.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* Modules and module types bound inside an expression have a uid, and that
   uid is registered in the declaration map of the [.cmt] like any other
   declaration, so that a check recorded against a local module can be
   attributed to it.  The uids of the module type expressions themselves are
   not declarations and do not appear here. *)

module type S = sig type t end

module M = struct type t = int end

let ascribed () =
  let module Local : S = M in
  ()

let anonymous () =
  let module Snapshot = struct type t = char end in
  ()

let nested () =
  let module Outer = struct
    module Inner : S = M
  end in
  ()

module F (X : S) = struct
  let inside () =
    let module Parameter : S = X in
    ()
end
