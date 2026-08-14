(* Helpers shared by the tests in this directory.

   The module-type facts of a compilation unit are keyed by uids, and uid
   stamps depend on how many idents the typechecker happened to allocate
   before them.  These helpers therefore print facts using the names of the
   declarations the uids belong to, so that the expected output records the
   contract (which kinds of checks and dependencies relate which
   declarations) rather than ident numbering.  Uids that belong to no named
   declaration, i.e. the module types written inline in the source, are
   printed as [#1], [#2], ... numbered in order of first appearance.

   A declaration [X] of the signature ascribed to [M] is named [M.X'], to
   tell it apart from the declaration [M.X] of the implementation it is
   checked against, and the module type expected of the parameter [X] of a
   functor [F] is named [param(F.X)]. *)

module Facts = Module_implementation_facts
module Uid = Shape.Uid

(* Naming of uids *)

type printer = {
  labels : (Uid.t * string) list;
  mutable anonymous : (Uid.t * string) list;
}

let printer labels = { labels; anonymous = [] }

let find_label labels uid =
  Option.map snd
    (List.find_opt (fun (labelled, _) -> Uid.equal labelled uid) labels)

let uid_name t uid =
  match find_label t.labels uid with
  | Some name -> name
  | None ->
      match find_label t.anonymous uid with
      | Some name -> name
      | None ->
          match uid with
          | Uid.Compilation_unit name -> name
          | Uid.Item _ | Uid.Internal | Uid.Predef _ | Uid.Unboxed_version _ ->
              let name =
                Printf.sprintf "#%d" (List.length t.anonymous + 1)
              in
              t.anonymous <- t.anonymous @ [ (uid, name) ];
              name

let is_named t uid = Option.is_some (find_label t.labels uid)

let unit_name uid =
  match uid with
  | Uid.Compilation_unit name -> Some name
  | Uid.Item _ | Uid.Internal | Uid.Predef _ | Uid.Unboxed_version _ -> None

(* [Filename.basename] keeps the output independent of the directory the test
   runs in. *)
let string_of_location (location : Location.t) =
  let start = location.loc_start and finish = location.loc_end in
  Printf.sprintf "%s:%d:%d-%d:%d"
    (Filename.basename start.pos_fname)
    start.pos_lnum
    (start.pos_cnum - start.pos_bol)
    finish.pos_lnum
    (finish.pos_cnum - finish.pos_bol)

let rec string_of_context t (context : Facts.Context.t) =
  match context with
  | Def uid -> uid_name t uid
  | Body uid -> "body(" ^ uid_name t uid ^ ")"
  | App (functor_, argument) ->
      string_of_context t functor_ ^ "(" ^ string_of_context t argument ^ ")"
  | Proj (Def unit_, uid) when unit_name unit_ <> None && is_named t uid ->
      uid_name t uid
  | Proj (inner, uid) ->
      let inner = string_of_context t inner in
      let projected = uid_name t uid in
      if is_named t uid && String.starts_with ~prefix:(inner ^ ".") projected
      then projected
      else inner ^ "." ^ projected
  | Site (unit_, artifact, occurrence) ->
      Printf.sprintf "site(%s.%s#%d)"
        (Compilation_unit.full_path_as_string unit_)
        (match artifact with Implementation -> "ml" | Interface -> "mli")
        occurrence

let string_of_key t (key : Facts.Key.t) =
  match key with
  | Named (Def unit_, uid) when unit_name unit_ <> None && is_named t uid ->
      uid_name t uid
  | Named (context, uid) -> uid_name t uid ^ "@" ^ string_of_context t context
  | Anon uid -> "<" ^ uid_name t uid ^ ">"

(* When locations are hidden, a node that is only a location prints as
   [<location>], so that output stays comparable across sources that differ
   only in layout. *)
let string_of_node ?(locations = true) t (node : Facts.Node.t) =
  match node with
  | Uid uid -> uid_name t uid
  | Location (_, location) ->
      if locations then string_of_location location else "<location>"

let string_of_check_kind : Facts.Check.Kind.t -> string = function
  | Ascription -> "ascription"
  | Argument -> "argument"
  | Package -> "package"
  | Interface -> "interface"

let string_of_dependency_reason : Facts.Dependency.Reason.t -> string =
  function
  | Definition -> "definition"
  | Alias -> "alias"
  | Include -> "include"
  | With_constraint -> "with"
  | Destructive_substitution -> "subst"
  | Module_type_of -> "typeof"
  | Strengthening -> "strengthen"
  | Functor_type -> "functor-type"
  | Instance -> "instance"
  | Argument_member -> "argument-member"
  | Interface -> "interface"

let string_of_omission_reason : Facts.Omission.Reason.t -> string = function
  | Unresolved_module_type -> "unresolved-module-type"
  | Unresolved_module -> "unresolved-module"
  | Unsupported_path -> "unsupported-path"
  | Missing_parameter_expectation -> "missing-parameter-expectation"

(* Printing of facts *)

let check_kinds : Facts.Check.Kind.t list =
  [ Ascription; Argument; Package; Interface ]

let dependency_reasons : Facts.Dependency.Reason.t list =
  [ Definition; Alias; Include; With_constraint; Destructive_substitution;
    Module_type_of; Strengthening; Functor_type; Instance; Argument_member;
    Interface ]

let omission_reasons : Facts.Omission.Reason.t list =
  [ Unresolved_module_type; Unresolved_module; Unsupported_path;
    Missing_parameter_expectation ]

let check_line ?(sites = true) t
    ({ implementation; expectation; kind; site } : Facts.Check.t) =
  Printf.sprintf "check %s %s : %s%s" (string_of_check_kind kind)
    (string_of_node ~locations:sites t implementation)
    (string_of_key t expectation)
    (if sites then " " ^ string_of_location site else "")

let dependency_line t ({ derived; source; reason } : Facts.Dependency.t) =
  Printf.sprintf "dep %s -%s-> %s" (string_of_key t derived)
    (string_of_dependency_reason reason) (string_of_key t source)

let equality_line t ({ left; right } : Facts.Context_equality.t) =
  Printf.sprintf "equal %s = %s" (string_of_context t left)
    (string_of_context t right)

let omission_line t ({ affected; source; reason } : Facts.Omission.t) =
  Printf.sprintf "omission: affected=%s source=%s reason=%s"
    (match affected with Some key -> string_of_key t key | None -> "none")
    (match source with Some uid -> uid_name t uid | None -> "none")
    (string_of_omission_reason reason)

(* The facts of a unit, one per line, in the order they are stored. *)
let fact_lines ?sites t (facts : Facts.t) =
  List.map (check_line ?sites t) facts.checks
  @ List.map (dependency_line t) facts.dependencies
  @ List.map (equality_line t) facts.equalities
  @ List.map (omission_line t) facts.omissions

let print_checks ?sites t (facts : Facts.t) =
  List.iter (fun check -> print_endline (check_line ?sites t check))
    facts.checks

let print_dependencies t (facts : Facts.t) =
  List.iter
    (fun dependency -> print_endline (dependency_line t dependency))
    facts.dependencies

(* The checks of kind [Interface], and the [Interface] dependencies of a named
   declaration on a named declaration of an interface, pair a declaration of
   the [.ml] of a unit with the corresponding declaration of its [.mli]. *)
let interface_checks (facts : Facts.t) =
  List.filter
    (fun ({ kind; _ } : Facts.Check.t) ->
      match kind with
      | Interface -> true
      | Ascription | Argument | Package -> false)
    facts.checks

(* A uid the typechecker allocated while typing an interface, i.e. one that
   belongs to some [.mli]. *)
let from_interface (uid : Uid.t) =
  match uid with
  | Item { from = Unit_info.Intf; _ } -> true
  | Compilation_unit _ | Item _ | Internal | Predef _ | Unboxed_version _ ->
      false

let interface_pairs (facts : Facts.t) =
  List.filter
    (fun ({ derived; source; reason } : Facts.Dependency.t) ->
      match reason, derived, source with
      | Interface, Named _, Named (_, uid) -> from_interface uid
      | ( ( Interface | Definition | Alias | Include | With_constraint
          | Destructive_substitution | Module_type_of | Strengthening
          | Functor_type | Instance | Argument_member ),
          _,
          _ ) ->
          false)
    facts.dependencies

let print_interface_pairs t facts =
  List.iter
    (fun ({ derived; source; _ } : Facts.Dependency.t) ->
      Printf.printf "pair %s <- %s\n" (string_of_key t derived)
        (string_of_key t source))
    (interface_pairs facts)

let print_equalities t (facts : Facts.t) =
  List.iter (fun equality -> print_endline (equality_line t equality))
    facts.equalities

let print_omissions t (facts : Facts.t) =
  List.iter (fun omission -> print_endline (omission_line t omission))
    facts.omissions

(* The digest records how many facts of each kind and reason were extracted,
   so that a test can pin down the shape of the whole fact set without
   spelling out every identity. *)
let print_digest (facts : Facts.t) =
  let counts to_string values members =
    let count value =
      List.length (List.filter (fun member -> member = value) members)
    in
    String.concat " "
      (List.filter_map
         (fun value ->
           match count value with
           | 0 -> None
           | count -> Some (Printf.sprintf "%s %d" (to_string value) count))
         values)
  in
  let relation name members breakdown =
    match breakdown with
    | "" -> Printf.sprintf "%s %d" name (List.length members)
    | breakdown ->
        Printf.sprintf "%s %d (%s)" name (List.length members) breakdown
  in
  Printf.printf "digest: %s %s %s %s\n"
    (relation "checks" facts.checks
       (counts string_of_check_kind check_kinds
          (List.map (fun (check : Facts.Check.t) -> check.kind) facts.checks)))
    (relation "deps" facts.dependencies
       (counts string_of_dependency_reason dependency_reasons
          (List.map
             (fun (dependency : Facts.Dependency.t) -> dependency.reason)
             facts.dependencies)))
    (relation "equalities" facts.equalities "")
    (relation "omissions" facts.omissions
       (counts string_of_omission_reason omission_reasons
          (List.map
             (fun (omission : Facts.Omission.t) -> omission.reason)
             facts.omissions)))

let print_facts ?(sites = true) t facts =
  print_digest facts;
  print_checks ~sites t facts;
  print_dependencies t facts;
  print_equalities t facts;
  print_omissions t facts

(* Names of the module and module type declarations of a typedtree, used as
   labels for the uids appearing in its facts. *)

let declaration_labels ?(prefix = "") typedtree =
  let labels = ref [] in
  (* The module type expected of a functor parameter is often written inline;
     naming it after the parameter is more readable than [#1].  These labels
     come last so that a parameter whose expectation is a named module type
     keeps the name of that module type. *)
  let parameters = ref [] in
  let qualify path name = if path = "" then name else path ^ "." ^ name in
  (* Declarations of an ascribed signature are primed, to tell them apart from
     the declarations of the implementation they are checked against. *)
  let segment ~expected name = if expected then name ^ "'" else name in
  let add uid path = labels := (uid, prefix ^ path) :: !labels in
  let name_or_underscore = function Some name -> name | None -> "_" in
  let rec structure path (structure : Typedtree.structure) =
    List.iter (structure_item path) structure.str_items
  and structure_item path (item : Typedtree.structure_item) =
    match item.str_desc with
    | Tstr_module binding -> module_binding path binding
    | Tstr_recmodule bindings -> List.iter (module_binding path) bindings
    | Tstr_modtype declaration ->
        module_type_declaration ~expected:false path declaration
    | Tstr_include { incl_mod; _ } -> module_expr path incl_mod
    | Tstr_eval _ | Tstr_value _ | Tstr_primitive _ | Tstr_type _
    | Tstr_typext _ | Tstr_exception _ | Tstr_open _ | Tstr_class _
    | Tstr_class_type _ | Tstr_attribute _ | Tstr_jkind _ -> ()
  and module_binding path (binding : Typedtree.module_binding) =
    let path =
      qualify path
        (match binding.mb_id with
         | Some ident -> Ident.name ident
         | None -> "_")
    in
    add binding.mb_uid path;
    module_expr path binding.mb_expr
  and module_expr path (expression : Typedtree.module_expr) =
    match expression.mod_desc with
    | Tmod_structure body -> structure path body
    | Tmod_functor (parameter, body, _) ->
        functor_parameter path parameter;
        module_expr path body
    | Tmod_constraint (inner, _, constraint_, _) ->
        module_expr path inner;
        (match constraint_ with
         | Tmodtype_explicit (expected, _) ->
             module_type ~expected:true path expected
         | Tmodtype_implicit | Tmodtype_package _ -> ())
    | Tmod_apply (functor_, argument, _, _, _) ->
        module_expr path functor_;
        module_expr path argument
    | Tmod_apply_unit (functor_, _) -> module_expr path functor_
    | Tmod_ident _ | Tmod_unpack _ -> ()
  and functor_parameter path (parameter : Typedtree.functor_parameter) =
    match parameter with
    | Unit -> ()
    | Named (_, name, expectation, _) ->
        let path = qualify path (name_or_underscore name.txt) in
        parameters :=
          (expectation.mty_uid, prefix ^ "param(" ^ path ^ ")") :: !parameters;
        module_type ~expected:false path expectation
  and module_type ~expected path (body : Typedtree.module_type) =
    match body.mty_desc with
    | Tmty_signature items -> signature ~expected path items
    | Tmty_typeof subject -> module_expr path subject
    | Tmty_functor (parameter, result, _) ->
        functor_parameter path parameter;
        module_type ~expected path result
    | Tmty_with (inner, _) | Tmty_strengthen (inner, _, _) ->
        module_type ~expected path inner
    | Tmty_ident _ | Tmty_alias _ -> ()
  and signature ~expected path (signature : Typedtree.signature) =
    List.iter (signature_item ~expected path) signature.sig_items
  and signature_item ~expected path (item : Typedtree.signature_item) =
    match item.sig_desc with
    | Tsig_module declaration -> module_declaration ~expected path declaration
    | Tsig_recmodule declarations ->
        List.iter (module_declaration ~expected path) declarations
    | Tsig_modtype declaration | Tsig_modtypesubst declaration ->
        module_type_declaration ~expected path declaration
    | Tsig_include (include_, _) ->
        module_type ~expected path include_.incl_mod
    | Tsig_value _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
    | Tsig_exception _ | Tsig_modsubst _ | Tsig_open _ | Tsig_class _
    | Tsig_class_type _ | Tsig_attribute _ | Tsig_jkind _ -> ()
  and module_declaration ~expected path
      (declaration : Typedtree.module_declaration) =
    let path =
      qualify path
        (segment ~expected (name_or_underscore declaration.md_name.txt))
    in
    add declaration.md_uid path;
    module_type ~expected path declaration.md_type
  and module_type_declaration ~expected path
      (declaration : Typedtree.module_type_declaration) =
    let path = qualify path (segment ~expected declaration.mtd_name.txt) in
    add declaration.mtd_uid path;
    match declaration.mtd_type with
    | Some body -> module_type ~expected path body
    | None -> ()
  in
  (match typedtree with
   | `Implementation items -> structure "" items
   | `Interface items -> signature ~expected:false "" items);
  List.rev !labels @ List.rev !parameters

(* Names of the module and module type declarations of a signature that was
   loaded from a [.cmi], used as labels for cross-unit facts. *)
let signature_labels ~prefix signature =
  let labels = ref [] in
  let rec items prefix signature =
    List.iter (item prefix) signature
  and item prefix (item : Types.signature_item) =
    match item with
    | Sig_modtype (id, declaration, _) ->
        let name = prefix ^ Ident.name id in
        labels := (declaration.mtd_uid, name) :: !labels;
        (match declaration.mtd_type with
         | Some (Mty_signature signature) -> items (name ^ ".") signature
         | Some (Mty_ident _ | Mty_functor _ | Mty_alias _ | Mty_strengthen _)
         | None -> ())
    | Sig_module (id, _, declaration, _, _) ->
        let name = prefix ^ Ident.name id in
        labels := (declaration.md_uid, name) :: !labels;
        (match declaration.md_type with
         | Mty_signature signature -> items (name ^ ".") signature
         | Mty_ident _ | Mty_functor _ | Mty_alias _ | Mty_strengthen _ -> ())
    | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_class _ | Sig_class_type _
    | Sig_jkind _ -> ()
  in
  items prefix signature;
  List.rev !labels

(* Typechecking of sources *)

let compilation_unit_of_filename filename =
  Compilation_unit.of_string
    (String.capitalize_ascii (Filename.remove_extension filename))

let set_current_unit filename =
  Env.set_current_unit
    (Unit_info.make_dummy ~input_name:filename
       (compilation_unit_of_filename filename))

let lexbuf_of_source ~filename source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf filename;
  Location.input_name := filename;
  lexbuf

let structure_of_source ~filename source =
  set_current_unit filename;
  let ast = Parse.implementation (lexbuf_of_source ~filename source) in
  let structure, _, _, _, _, _ =
    Typemod.type_structure (Lazy.force Env.initial) ast
  in
  structure

let signature_of_source ~filename source =
  set_current_unit filename;
  let ast = Parse.interface (lexbuf_of_source ~filename source) in
  let signature, (_ : Typedtree.argument_interface option) =
    Typemod.type_interface ~sourcefile:filename
      (compilation_unit_of_filename filename)
      (Lazy.force Env.initial) ast
  in
  signature

let facts_of_structure ?(module_pairs = []) ?(modtype_pairs = []) ~filename
    structure =
  Facts.of_implementation
    (compilation_unit_of_filename filename)
    ~module_pairs ~modtype_pairs ~unit_interface_check:false
    ~argument_interface:None structure

let facts_of_signature ~filename signature =
  Facts.of_interface
    (compilation_unit_of_filename filename)
    ~argument_interface:None signature

(* Facts stored in artifacts *)

let labels_of_annots ?prefix (annots : Cmt_format.binary_annots) =
  match annots with
  | Implementation structure ->
      declaration_labels ?prefix (`Implementation structure)
  | Interface signature -> declaration_labels ?prefix (`Interface signature)
  | Packed _ | Partial_implementation _ | Partial_interface _ -> []

(* Report the facts of one implementation source, naming the uids after the
   declarations of the unit. *)
let report_implementation ?sites ~filename source =
  let structure = structure_of_source ~filename source in
  let facts = facts_of_structure ~filename structure in
  let labels = declaration_labels (`Implementation structure) in
  print_facts ?sites (printer labels) facts

(* Report the facts of one interface source. *)
let report_interface ?sites ~filename source =
  let signature = signature_of_source ~filename source in
  let facts = facts_of_signature ~filename signature in
  let labels = declaration_labels (`Interface signature) in
  print_facts ?sites (printer labels) facts

let heading title = Printf.printf "== %s\n" title
