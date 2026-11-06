(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "link_constructors_labels.mli link_constructors_labels.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -decls -uid-deps link_constructors_labels.cmti \
           link_constructors_labels.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

(* Checking an implementation against its interface links the uids of
   constructors and labels, not just those of the types that contain them.
   Merlin relies on these links to find the uses of a constructor or label
   from its declaration in the interface. *)

type v = A | B of int

type r = { f : int; g : string }

type u = #{ h : int }

(* Labels of inline records, including those of exceptions and extension
   constructors. *)

type i = C of { l : int }

exception E of { x : int }

type o = ..

type o += K of { y : int }

type p = P1 | P2 of { pl : int }

(* The interface has no constructor to link [Abs] to. *)
type abs = Abs

(* Module type equality links declarations to declarations. *)
module type S = sig
  type t = D | E of { el : int }
end

(* A definition links to the declaration it is checked against, also within
   a single file. *)
module M : S = struct
  type t = D | E of { el : int }
end

(* Functor parameters are in negative position. *)
module F (X : sig type t = FA end) = struct end
