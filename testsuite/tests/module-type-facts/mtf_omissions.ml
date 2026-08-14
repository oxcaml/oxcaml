(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Every reason a fact can be omitted.  An omission records that something
   about a module type could not be named, so that a consumer of the facts
   knows its view of that family is partial rather than complete.

   [unresolved-module] is reachable from source, but the other three reasons
   describe module types the typechecker does not produce for well-typed
   source: an unresolvable module type path, an applicative path where a
   module type is expected, and a functor parameter whose expectation was not
   recorded.  Those are reached here by mutating the typedtree, so that the
   handling of each reason stays covered even though no source produces it. *)

open Mtf_facts

(* Mutations of the ascription of the single module binding of a structure. *)

let map_first_module_binding ~f (structure : Typedtree.structure) =
  let mapped = ref false in
  let str_items =
    List.map
      (fun (item : Typedtree.structure_item) ->
        match item.str_desc with
        | Tstr_module binding when not !mapped ->
            mapped := true;
            { item with str_desc = Tstr_module (f binding) }
        | Tstr_module _ | Tstr_eval _ | Tstr_value _ | Tstr_primitive _
        | Tstr_type _ | Tstr_typext _ | Tstr_exception _ | Tstr_recmodule _
        | Tstr_modtype _ | Tstr_open _ | Tstr_class _ | Tstr_class_type _
        | Tstr_include _ | Tstr_attribute _ | Tstr_jkind _ ->
            item)
      structure.str_items
  in
  if not !mapped then failwith "expected a module binding";
  { structure with str_items }

let map_last_module_binding ~f structure =
  let mapped =
    map_first_module_binding ~f
      { structure with
        Typedtree.str_items = List.rev structure.Typedtree.str_items
      }
  in
  { mapped with Typedtree.str_items = List.rev mapped.Typedtree.str_items }

(* Replaces the path of the module type a module is ascribed. *)
let map_ascription_path ~f binding =
  let module_type (module_type : Typedtree.module_type) =
    match module_type.mty_desc with
    | Tmty_ident (path, longident) ->
        { module_type with mty_desc = Tmty_ident (f path, longident) }
    | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _
    | Tmty_alias _ | Tmty_strengthen _ ->
        failwith "expected an ascription against a named module type"
  in
  let expression (expression : Typedtree.module_expr) =
    match expression.mod_desc with
    | Tmod_constraint
        (inner, inner_type, Tmodtype_explicit (expected, modes), coercion) ->
        { expression with
          mod_desc =
            Tmod_constraint
              ( inner,
                inner_type,
                Tmodtype_explicit (module_type expected, modes),
                coercion )
        }
    | Tmod_constraint _ | Tmod_structure _ | Tmod_ident _ | Tmod_functor _
    | Tmod_apply _ | Tmod_apply_unit _ | Tmod_unpack _ ->
        failwith "expected an ascription"
  in
  { binding with Typedtree.mb_expr = expression binding.Typedtree.mb_expr }

(* Forgets the uid of the module type expected of the parameter of the functor
   being applied. *)
let forget_parameter_expectation binding =
  let module_type (module_type : Types.module_type) =
    match module_type with
    | Mty_functor (Named (identifier, parameter, _, parameter_mode), result,
                   result_mode) ->
        Types.Mty_functor
          ( Named (identifier, parameter, None, parameter_mode),
            result,
            result_mode )
    | Mty_functor (Unit, _, _)
    | Mty_ident _ | Mty_signature _ | Mty_alias _ | Mty_strengthen _ ->
        failwith "expected a functor with a named parameter"
  in
  let expression (expression : Typedtree.module_expr) =
    match expression.mod_desc with
    | Tmod_apply (functor_, argument, argument_mode, result_mode, coercion) ->
        let functor_ =
          { functor_ with Typedtree.mod_type = module_type functor_.mod_type }
        in
        { expression with
          mod_desc =
            Tmod_apply (functor_, argument, argument_mode, result_mode,
                        coercion)
        }
    | Tmod_constraint _ | Tmod_structure _ | Tmod_ident _ | Tmod_functor _
    | Tmod_apply_unit _ | Tmod_unpack _ ->
        failwith "expected an application"
  in
  { binding with Typedtree.mb_expr = expression binding.Typedtree.mb_expr }

let report ~filename ~mutate source =
  let structure = mutate (structure_of_source ~filename source) in
  let facts = facts_of_structure ~filename structure in
  let labels = declaration_labels (`Implementation structure) in
  print_facts ~sites:false (printer labels) facts

let ascription = {|
module type S = sig type t end
module M : S = struct type t = int end
|}

let () = heading "a module type of a structure that is not a path"

let () = report_implementation ~sites:false ~filename:"unresolved.ml"
  {|
module type S = sig type t end
module type T = module type of struct type t = int end
module M : T = struct type t = int end
|}

let () = heading "a module type path that does not resolve"

let () =
  report ~filename:"absent.ml" ascription
    ~mutate:
      (map_first_module_binding
         ~f:
           (map_ascription_path ~f:(fun (_ : Path.t) ->
                Path.Pident (Ident.create_local "Absent"))))

let () = heading "an applicative path where a module type is expected"

let () =
  report ~filename:"applied.ml" ascription
    ~mutate:
      (map_first_module_binding
         ~f:
           (map_ascription_path ~f:(fun path -> Path.Papply (path, path))))

let () = heading "a functor parameter whose expectation was not recorded"

let () =
  report ~filename:"noexpectation.ml"
    ~mutate:(map_last_module_binding ~f:forget_parameter_expectation)
    {|
module type S = sig type t end
module F (X : sig module M : S end) = struct type u = X.M.t end
module A = struct module M : S = struct type t = int end end
module FA = F (A)
|}
