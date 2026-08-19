(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* The two typedtree fields the facts are read from that no other test in this
   directory reaches: the uid of a module bound in an expression, and the path
   a first-class module constraint records.  See [mtf_facts.ml] for the output
   format. *)

open Mtf_facts

let () = heading "a module bound in an expression is a declaration of its own"

(* [Texp_letmodule] carries a uid, which is what a check against the local
   module is attributed to, and which is registered in the declaration map of
   the [.cmt] like the uid of a toplevel binding. *)
let () =
  let filename = "local.ml" in
  let structure =
    structure_of_source ~filename
      {|
module type S = sig type t end
module M = struct type t = int end
let f () = let module Local : S = M in ()
|}
  in
  let bindings = ref [] in
  Cmt_format.iter_declarations (Implementation structure)
    ~f:(fun uid (declaration : Typedtree.item_declaration) ->
      match declaration with
      | Module_binding { mb_name = { txt = Some name; _ }; _ } ->
          bindings := (uid, name) :: !bindings
      | Module_binding { mb_name = { txt = None; _ }; _ }
      | Value _ | Value_binding _ | Type _ | Constructor _
      | Extension_constructor _ | Label _ | Module _ | Module_substitution _
      | Module_type _ | Class _ | Class_type _ | Jkind _ -> ());
  let bindings = List.rev !bindings in
  Printf.printf "module bindings declared: %s\n"
    (String.concat " " (List.map snd bindings));
  (* The names of the local bindings come from the declaration map, so a check
     printed against [Local] is a check against the uid registered there. *)
  print_facts
    (printer (declaration_labels (`Implementation structure) @ bindings))
    (facts_of_structure ~filename structure)

let () = heading "a first-class module records the path of its package type"

(* The constraint of a first-class module is typed as [Tmodtype_package],
   which records the path of the package type so that the check against that
   module type can be recorded as a fact.  [Untypeast] treats it like the
   implicit constraint it replaced, so untyping a package must give back a
   structure that types to the same facts. *)
let () =
  let filename = "package.ml" in
  let source =
    {|
module type S = sig type t end
module M = struct type t = int end
let packed = (module M : S)
let repack (module X : S) = (module X : S)
|}
  in
  let iterator =
    { Tast_iterator.default_iterator with
      module_expr =
        (fun iterator (expression : Typedtree.module_expr) ->
          (match expression.mod_desc with
           | Tmod_constraint
               (_, _, Tmodtype_package { package_module_type_path }, _) ->
               Printf.printf "package constraint: %s\n"
                 (Path.name package_module_type_path)
           | Tmod_constraint
               (_, _, (Tmodtype_implicit | Tmodtype_explicit _), _)
           | Tmod_structure _ | Tmod_ident _ | Tmod_functor _ | Tmod_apply _
           | Tmod_apply_unit _ | Tmod_unpack _ -> ());
          Tast_iterator.default_iterator.module_expr iterator expression)
    }
  in
  let facts_lines source =
    let structure = structure_of_source ~filename source in
    let facts = facts_of_structure ~filename structure in
    let labels = declaration_labels (`Implementation structure) in
    fact_lines (printer labels) facts
  in
  let structure = structure_of_source ~filename source in
  iterator.structure iterator structure;
  let untyped =
    Format.asprintf "%a" Pprintast.structure
      (Untypeast.untype_structure structure)
  in
  List.iter print_endline (facts_lines source);
  Printf.printf "same facts after untyping: %b\n"
    (List.equal String.equal (facts_lines source) (facts_lines untyped))
