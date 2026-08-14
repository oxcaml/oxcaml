(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* The constraint of a first-class module is typed as [Tmodtype_package],
   which records the path of the package type so that the check against that
   module type can be recorded as a fact.  [Printtyped] (used by
   [-dtypedtree]) and [Untypeast] treat it like the implicit constraint it
   replaced, so it must not appear in the printed typedtree, and untyping a
   package must give back a structure that types to the same facts. *)

open Mtf_facts

let filename = "package.ml"

let source =
  {|
module type S = sig type t end
module M = struct type t = int end
let packed = (module M : S)
let repack (module X : S) = (module X : S)
|}

let () = heading "the package constraints recorded in the typedtree"

(* [Tmodtype_package] records the path of the package type; [Tast_iterator]
   reaches the ones inside expressions. *)
let () =
  let structure = structure_of_source ~filename source in
  let iterator =
    { Tast_iterator.default_iterator with
      module_expr =
        (fun iterator (expression : Typedtree.module_expr) ->
          (match expression.mod_desc with
           | Tmod_constraint (_, _, Tmodtype_package path, _) ->
               Printf.printf "package constraint: %s\n" (Path.name path)
           | Tmod_constraint
               (_, _, (Tmodtype_implicit | Tmodtype_explicit _), _)
           | Tmod_structure _ | Tmod_ident _ | Tmod_functor _ | Tmod_apply _
           | Tmod_apply_unit _ | Tmod_unpack _ -> ());
          Tast_iterator.default_iterator.module_expr iterator expression)
    }
  in
  iterator.structure iterator structure

let () = heading "and printed as the constraint it replaced"

(* [Printtyped], and hence [-dtypedtree], sees through the package constraint
   exactly like the implicit constraint it replaced, so no constraint node is
   printed. *)
let () =
  let structure = structure_of_source ~filename source in
  let printed = Format.asprintf "%a" Printtyped.implementation structure in
  let mentions needle =
    let length = String.length needle in
    let rec search index =
      index + length <= String.length printed
      && (String.sub printed index length = needle || search (index + 1))
    in
    search 0
  in
  Printf.printf "prints Tmod_constraint: %b\nprints Tmodtype_package: %b\n"
    (mentions "Tmod_constraint") (mentions "Tmodtype_package");
  Printf.printf "prints Texp_pack: %b\n" (mentions "Texp_pack")

let () = heading "untyping a package keeps the same facts"

(* [Untypeast] must give back a structure that types to the same facts: the
   same checks, of the same kinds, against the same module types.  Comparing
   the facts rather than the printed source keeps the test independent of the
   layout [Pprintast] chooses. *)
let () =
  let facts_lines source =
    let structure = structure_of_source ~filename source in
    let facts = facts_of_structure ~filename structure in
    let labels = declaration_labels (`Implementation structure) in
    (* Sites are dropped: untyping and reprinting moves the source around. *)
    fact_lines ~sites:false (printer labels) facts
  in
  let untyped =
    Format.asprintf "%a" Pprintast.structure
      (Untypeast.untype_structure (structure_of_source ~filename source))
  in
  let original = facts_lines source in
  let round_tripped = facts_lines untyped in
  List.iter print_endline original;
  Printf.printf "same facts after untyping: %b\n"
    (List.equal String.equal original round_tripped)
