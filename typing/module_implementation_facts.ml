(* Extracts module-type check and dependency facts from the typedtree of one
   compilation unit and freezes them for storage in artifacts and indexes. *)

open Typedtree
module Uid = Shape.Uid

let compare_pair compare_a compare_b (a1, b1) (a2, b2) =
  let c = compare_a a1 a2 in
  if c <> 0 then c else compare_b b1 b2

module Artifact = struct
  type t =
    | Implementation
    | Interface

  let int_of_t = function Implementation -> 0 | Interface -> 1

  let compare left right = Int.compare (int_of_t left) (int_of_t right)

  let extension = function Implementation -> "ml" | Interface -> "mli"
end

module Context = struct
  type t =
    | Def of Uid.t
    | App of t * t
    | Proj of t * Uid.t
    | Body of Uid.t
    | Site of Compilation_unit.t * Artifact.t * int

  let rec compare left right =
    match left, right with
    | Def left, Def right -> Uid.compare left right
    | Def _, (App _ | Proj _ | Body _ | Site _) -> -1
    | App _, Def _ -> 1
    | App (f1, a1), App (f2, a2) ->
      compare_pair compare compare (f1, a1) (f2, a2)
    | App _, (Proj _ | Body _ | Site _) -> -1
    | Proj _, (Def _ | App _) -> 1
    | Proj (c1, u1), Proj (c2, u2) ->
      compare_pair compare Uid.compare (c1, u1) (c2, u2)
    | Proj _, (Body _ | Site _) -> -1
    | Body _, (Def _ | App _ | Proj _) -> 1
    | Body left, Body right -> Uid.compare left right
    | Body _, Site _ -> -1
    | Site _, (Def _ | App _ | Proj _ | Body _) -> 1
    | Site (u1, a1, o1), Site (u2, a2, o2) ->
      let c = Compilation_unit.compare u1 u2 in
      if c <> 0
      then c
      else
        let c = Artifact.compare a1 a2 in
        if c <> 0 then c else Int.compare o1 o2

  let equal left right = compare left right = 0

  let rec print fmt = function
    | Def uid -> Uid.print fmt uid
    | App (f, a) -> Format.fprintf fmt "%a(%a)" print f print a
    | Proj (context, uid) ->
      Format.fprintf fmt "%a.%a" print context Uid.print uid
    | Body uid -> Format.fprintf fmt "body(%a)" Uid.print uid
    | Site (unit_, artifact, occurrence) ->
      Format.fprintf fmt "site(%s.%s#%d)"
        (Compilation_unit.full_path_as_string unit_)
        (Artifact.extension artifact)
        occurrence
end

module Key = struct
  type t =
    | Named of
        { context : Context.t;
          family_uid : Uid.t
        }
    | Anon of { key_uid : Uid.t }

  let compare left right =
    match left, right with
    | ( Named { context = c1; family_uid = u1 },
        Named { context = c2; family_uid = u2 } ) ->
      compare_pair Context.compare Uid.compare (c1, u1) (c2, u2)
    | Named _, Anon _ -> -1
    | Anon _, Named _ -> 1
    | Anon { key_uid = left }, Anon { key_uid = right } ->
      Uid.compare left right

  let equal left right = compare left right = 0

  let print fmt = function
    | Named { context; family_uid } ->
      Format.fprintf fmt "%a@@%a" Uid.print family_uid Context.print context
    | Anon { key_uid } -> Format.fprintf fmt "<%a>" Uid.print key_uid

  let family = function
    | Named { family_uid; _ } -> Some family_uid
    | Anon _ -> None
end

module Node = struct
  type t =
    | Uid of Uid.t
    | Location of Compilation_unit.t * Location.t

  let compare left right =
    match left, right with
    | Uid left, Uid right -> Uid.compare left right
    | Uid _, Location _ -> -1
    | Location _, Uid _ -> 1
    | Location (u1, l1), Location (u2, l2) ->
      let c = Compilation_unit.compare u1 u2 in
      if c <> 0 then c else Location.compare l1 l2
end

module Check = struct
  module Kind = struct
    type t =
      | Ascription
      | Argument
      | Package
      | Interface

    let int_of_t = function
      | Ascription -> 0
      | Argument -> 1
      | Package -> 2
      | Interface -> 3

    let compare left right = Int.compare (int_of_t left) (int_of_t right)
  end

  type t =
    { implementation : Node.t;
      expectation : Key.t;
      kind : Kind.t;
      site : Location.t
    }

  let compare left right =
    let c = Key.compare left.expectation right.expectation in
    if c <> 0
    then c
    else
      let c = Node.compare left.implementation right.implementation in
      if c <> 0
      then c
      else
        let c = Kind.compare left.kind right.kind in
        if c <> 0 then c else Location.compare left.site right.site
end

module Dependency = struct
  module Reason = struct
    type t =
      | Definition
      | Alias
      | Include
      | With_constraint
      | Destructive_substitution
      | Module_type_of
      | Strengthening
      | Functor_type
      | Instance
      | Argument_member
      | Interface

    let int_of_t = function
      | Definition -> 0
      | Alias -> 1
      | Include -> 2
      | With_constraint -> 3
      | Destructive_substitution -> 4
      | Module_type_of -> 5
      | Strengthening -> 6
      | Functor_type -> 7
      | Instance -> 8
      | Argument_member -> 9
      | Interface -> 10

    let compare left right = Int.compare (int_of_t left) (int_of_t right)
  end

  type t =
    { derived : Key.t;
      source : Key.t;
      reason : Reason.t
    }

  let compare left right =
    let c = Key.compare left.derived right.derived in
    if c <> 0
    then c
    else
      let c = Key.compare left.source right.source in
      if c <> 0 then c else Reason.compare left.reason right.reason
end

module Context_equality = struct
  type t =
    { left : Context.t;
      right : Context.t
    }

  let compare e1 e2 =
    compare_pair Context.compare Context.compare (e1.left, e1.right)
      (e2.left, e2.right)

  let oriented { left; right } =
    let c = Context.compare left right in
    if c < 0
    then Some { left; right }
    else if c > 0
    then Some { left = right; right = left }
    else None
end

module Omission = struct
  module Reason = struct
    type t =
      | Unresolved_module_type
      | Unresolved_module
      | Unsupported_path
      | Missing_parameter_expectation

    let int_of_t = function
      | Unresolved_module_type -> 0
      | Unresolved_module -> 1
      | Unsupported_path -> 2
      | Missing_parameter_expectation -> 3

    let compare left right = Int.compare (int_of_t left) (int_of_t right)
  end

  type t =
    { affected : Key.t option;
      source : Uid.t option;
      reason : Reason.t
    }

  let compare left right =
    let c = Option.compare Key.compare left.affected right.affected in
    if c <> 0
    then c
    else
      let c = Option.compare Uid.compare left.source right.source in
      if c <> 0 then c else Reason.compare left.reason right.reason
end

type t =
  { checks : Check.t list;
    dependencies : Dependency.t list;
    equalities : Context_equality.t list;
    omissions : Omission.t list
  }

type frozen = t

module Check_set = Set.Make (Check)
module Dependency_set = Set.Make (Dependency)
module Context_equality_set = Set.Make (Context_equality)
module Omission_set = Set.Make (Omission)

module Builder = struct
  type nonrec t =
    { mutable checks : Check_set.t;
      mutable dependencies : Dependency_set.t;
      mutable equalities : Context_equality_set.t;
      mutable omissions : Omission_set.t
    }

  let create () =
    { checks = Check_set.empty;
      dependencies = Dependency_set.empty;
      equalities = Context_equality_set.empty;
      omissions = Omission_set.empty
    }

  let add_check t check = t.checks <- Check_set.add check t.checks

  let add_dependency t dependency =
    t.dependencies <- Dependency_set.add dependency t.dependencies

  let add_equality t equality =
    match Context_equality.oriented equality with
    | None -> ()
    | Some equality ->
      t.equalities <- Context_equality_set.add equality t.equalities

  let add_omission t omission =
    t.omissions <- Omission_set.add omission t.omissions

  let freeze t : frozen =
    { checks = Check_set.elements t.checks;
      dependencies = Dependency_set.elements t.dependencies;
      equalities = Context_equality_set.elements t.equalities;
      omissions = Omission_set.elements t.omissions
    }
end

let find_module env path =
  match Env.find_module path env with
  | declaration -> Some declaration
  | exception Not_found -> None

let find_modtype env path =
  match Env.find_modtype path env with
  | declaration -> Some declaration
  | exception Not_found -> None

let normalize_module_path env path =
  match Env.normalize_module_path None env path with
  | path -> path
  | exception Not_found -> path

let find_normalized_module env path =
  find_module env (normalize_module_path env path)

let rec path_contains_apply : Path.t -> bool = function
  | Path.Pident _ -> false
  | Path.Pdot (prefix, _) -> path_contains_apply prefix
  | Path.Papply _ -> true
  | Path.Pextra_ty (prefix, _) -> path_contains_apply prefix

let rec longident_contains_apply : Longident.t -> bool = function
  | Longident.Lident _ -> false
  | Longident.Ldot (prefix, _) -> longident_contains_apply prefix.txt
  | Longident.Lapply _ -> true

let rec head_uid : Context.t -> Uid.t option = function
  | Context.Def uid | Context.Body uid -> Some uid
  | Context.Proj (_, uid) -> Some uid
  | Context.App (f, _) -> head_uid f
  | Context.Site _ -> None

let rec family_context : Context.t -> Context.t option = function
  | Context.Proj (c, uid) ->
    Option.map (fun c -> Context.Proj (c, uid)) (family_context c)
  | Context.App (f, _) -> Option.map (fun uid -> Context.Body uid) (head_uid f)
  | Context.Def _ | Context.Body _ | Context.Site _ -> None

let rec path_of_module_expr (module_expr : module_expr) =
  match module_expr.mod_desc with
  | Tmod_ident (path, _) -> Some path
  | Tmod_apply (functor_, argument, _, _, _) -> (
    match path_of_module_expr functor_, path_of_module_expr argument with
    | Some functor_, Some argument -> Some (Path.Papply (functor_, argument))
    | (Some _ | None), _ -> None)
  | Tmod_structure _ | Tmod_functor _ | Tmod_apply_unit _ | Tmod_constraint _
  | Tmod_unpack _ ->
    None

let rec unwrap_implicit_constraint (module_expr : module_expr) =
  match module_expr.mod_desc with
  | Tmod_constraint (inner, _, (Tmodtype_implicit | Tmodtype_package _), _) ->
    unwrap_implicit_constraint inner
  | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_apply _
  | Tmod_apply_unit _ | Tmod_constraint _ | Tmod_unpack _ ->
    module_expr

let has_remove_aliases_attribute attributes =
  match Attr_helper.get_no_payload_attribute "remove_aliases" attributes with
  | Some _ -> true
  | None -> false

let single_structure_include (module_expr : module_expr) =
  match module_expr.mod_desc with
  | Tmod_structure { str_items = [item]; _ } -> (
    match item.str_desc with
    | Tstr_include { incl_mod; incl_kind = Tincl_structure; _ } -> Some incl_mod
    | Tstr_eval _ | Tstr_value _ | Tstr_primitive _ | Tstr_type _
    | Tstr_typext _ | Tstr_exception _ | Tstr_module _ | Tstr_recmodule _
    | Tstr_modtype _ | Tstr_open _ | Tstr_class _ | Tstr_class_type _
    | Tstr_include _ | Tstr_attribute _ | Tstr_jkind _ ->
      None)
  | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_apply _
  | Tmod_apply_unit _ | Tmod_constraint _ | Tmod_unpack _ ->
    None

let typeof_subject (module_expr : module_expr) =
  if has_remove_aliases_attribute module_expr.mod_attributes
  then module_expr
  else
    match single_structure_include module_expr with
    | Some included -> included
    | None -> module_expr

let declared_module_type_path env path =
  let rec follow visited path =
    if List.exists (Path.same path) visited
    then None
    else
      match find_module env path with
      | Some { Types.md_type = Mty_ident declared; _ } -> Some declared
      | Some { Types.md_type = Mty_strengthen (Mty_ident declared, _, _); _ } ->
        Some declared
      | Some { Types.md_type = Mty_alias target; _ } ->
        follow (path :: visited) target
      | Some
          { Types.md_type = Mty_signature _ | Mty_functor _ | Mty_strengthen _;
            _
          } ->
        None
      | None -> None
  in
  follow [] path

module String_map = Map.Make (String)
module Path_map = Map.Make (Path)

type signature_index =
  { modtype_members : Types.modtype_declaration String_map.t;
    module_members : Types.module_declaration String_map.t
  }

let empty_signature_index =
  { modtype_members = String_map.empty; module_members = String_map.empty }

type signature_member =
  | Modtype_member of string * Types.modtype_declaration
  | Module_member of string * Types.module_declaration
  | Other_member

let classify_signature_member (item : Types.signature_item) =
  match item with
  | Types.Sig_modtype (id, declaration, _) ->
    Modtype_member (Ident.name id, declaration)
  | Types.Sig_module (id, _, declaration, _, _) ->
    Module_member (Ident.name id, declaration)
  | Types.Sig_value _ | Types.Sig_type _ | Types.Sig_typext _
  | Types.Sig_class _ | Types.Sig_class_type _ | Types.Sig_jkind _ ->
    Other_member

let scraped_signature env (module_type : Types.module_type) =
  match Mtype.scrape env module_type with
  | Mty_signature signature -> Some signature
  | Mty_ident _ | Mty_functor _ | Mty_alias _ | Mty_strengthen _ -> None

let scraped_alias_signature env (module_type : Types.module_type) =
  match Mtype.scrape_alias env module_type with
  | Mty_signature signature -> Some signature
  | Mty_ident _ | Mty_functor _ | Mty_alias _ | Mty_strengthen _ -> None

let index_of_signature (signature : Types.signature) =
  List.fold_left
    (fun index item ->
      match classify_signature_member item with
      | Modtype_member (name, declaration) ->
        { index with
          modtype_members =
            String_map.add name declaration index.modtype_members
        }
      | Module_member (name, declaration) ->
        { index with
          module_members = String_map.add name declaration index.module_members
        }
      | Other_member -> index)
    empty_signature_index signature

let signature_index_of_module_type env (module_type : Types.module_type) =
  match scraped_signature env module_type with
  | Some signature -> index_of_signature signature
  | None -> empty_signature_index

let has_argument_members env (parameter_type : Types.module_type) =
  match scraped_signature env parameter_type with
  | Some signature ->
    List.exists
      (fun item ->
        match classify_signature_member item with
        | Modtype_member _ | Module_member _ -> true
        | Other_member -> false)
      signature
  | None -> false

let rec path_root_idents acc : Path.t -> Ident.t list = function
  | Path.Pident id -> id :: acc
  | Path.Pdot (prefix, _) | Path.Pextra_ty (prefix, _) ->
    path_root_idents acc prefix
  | Path.Papply (functor_, argument) ->
    path_root_idents (path_root_idents acc functor_) argument

let functor_argument_roots env (module_type : Types.module_type) =
  let roots = ref Ident.Set.empty in
  let check_path path =
    List.iter
      (fun root ->
        let path = Path.Pident root in
        if Env.is_functor_arg path env && not (Ident.Set.mem root !roots)
        then roots := Ident.Set.add root !roots)
      (path_root_idents [] path)
  in
  Types.with_type_mark (fun mark ->
      let iterators =
        { (Btype.type_iterators mark) with Btype.it_path = check_path }
      in
      iterators.it_module_type iterators module_type);
  Ident.Set.elements !roots

let named_modtype_uids env (module_type : Types.module_type) =
  let uids = ref Uid.Set.empty in
  let check_path path =
    match find_modtype env path with
    | Some declaration -> uids := Uid.Set.add declaration.Types.mtd_uid !uids
    | None -> ()
  in
  Types.with_type_mark (fun mark ->
      let iterators =
        { (Btype.type_iterators mark) with
          Btype.it_module_type =
            (fun it mty ->
              (match mty with
              | Types.Mty_ident path -> check_path path
              | Types.Mty_alias _ | Types.Mty_signature _ | Types.Mty_functor _
              | Types.Mty_strengthen _ ->
                ());
              Btype.(type_iterators mark).it_module_type it mty)
        }
      in
      iterators.it_module_type iterators module_type);
  Uid.Set.elements !uids

let is_relevant_expectation cache (module_type : Typedtree.module_type) =
  let rec relevant (module_type : Typedtree.module_type) =
    match Uid.Map.find_opt module_type.mty_uid !cache with
    | Some relevant -> relevant
    | None ->
      let result = compute module_type in
      cache := Uid.Map.add module_type.mty_uid result !cache;
      result
  and compute (module_type : Typedtree.module_type) =
    match module_type.mty_desc with
    | Tmty_ident _ | Tmty_typeof _ | Tmty_alias _ -> true
    | Tmty_signature signature ->
      List.exists
        (fun item ->
          match item.sig_desc with
          | Tsig_include (include_, _) -> relevant include_.incl_mod
          | Tsig_modtype _ | Tsig_modtypesubst _ -> true
          | Tsig_module declaration -> relevant declaration.md_type
          | Tsig_recmodule declarations ->
            List.exists
              (fun (declaration : module_declaration) ->
                relevant declaration.md_type)
              declarations
          | Tsig_value _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
          | Tsig_exception _ | Tsig_modsubst _ | Tsig_open _ | Tsig_class _
          | Tsig_class_type _ | Tsig_attribute _ | Tsig_jkind _ ->
            false)
        signature.sig_items
    | Tmty_with (base, constraints) ->
      relevant base
      || List.exists
           (fun (_, _, constraint_) ->
             match constraint_ with
             | Twith_modtype _ | Twith_modtypesubst _ | Twith_module _
             | Twith_modsubst _ ->
               true
             | Twith_type _ | Twith_typesubst _ | Twith_jkind _
             | Twith_jkindsubst _ ->
               false)
           constraints
    | Tmty_strengthen _ -> true
    | Tmty_functor (parameter, result, _) -> (
      relevant result
      ||
      match parameter with
      | Unit -> false
      | Named (_, _, parameter_type, _) -> relevant parameter_type)
  in
  relevant module_type

let facts_of_tree compilation_unit artifact iterate =
  let unit_uid = Uid.of_compilation_unit_id compilation_unit in
  let facts = Builder.create () in
  let module_contexts : Context.t Uid.Map.t ref = ref Uid.Map.empty in
  let modtype_contexts : Context.t Uid.Map.t ref = ref Uid.Map.empty in
  let modtype_declaration_contexts : Context.t Uid.Map.t ref =
    ref Uid.Map.empty
  in
  let parameter_expectations : Uid.t Uid.Map.t ref = ref Uid.Map.empty in
  let binding_expectations : Key.t Uid.Map.t ref = ref Uid.Map.empty in
  let site_counter = ref 0 in
  let fresh_site () =
    let occurrence = !site_counter in
    site_counter := occurrence + 1;
    Context.Site (compilation_unit, artifact, occurrence)
  in
  let scoped f =
    let saved_module_contexts = !module_contexts
    and saved_modtype_contexts = !modtype_contexts in
    Fun.protect
      ~finally:(fun () ->
        module_contexts := saved_module_contexts;
        modtype_contexts := saved_modtype_contexts)
      f
  in
  let enclosing = ref [Context.Def unit_uid] in
  let enclosing_context () = List.hd !enclosing in
  let with_enclosing context f =
    enclosing := context :: !enclosing;
    Fun.protect
      ~finally:(fun () -> enclosing := List.tl !enclosing)
      (fun () -> scoped f)
  in
  let add_check implementation expectation kind site =
    Builder.add_check facts { Check.implementation; expectation; kind; site }
  in
  let add_dependency derived source reason =
    if not (Key.equal derived source)
    then Builder.add_dependency facts { Dependency.derived; source; reason }
  in
  let add_equality a b =
    if not (Context.equal a b)
    then Builder.add_equality facts { Context_equality.left = a; right = b }
  in
  let add_omission ?affected ?source reason =
    Builder.add_omission facts { Omission.affected; source; reason }
  in
  let record_module_context uid context =
    module_contexts := Uid.Map.add uid context !module_contexts
  in
  let record_modtype_context uid context =
    modtype_contexts := Uid.Map.add uid context !modtype_contexts
  in
  let module_context uid =
    match Uid.Map.find_opt uid !module_contexts with
    | Some context -> context
    | None -> Context.Def uid
  in
  let record_member_contexts root signature =
    List.iter
      (fun item ->
        match classify_signature_member item with
        | Modtype_member (_, declaration) ->
          record_modtype_context declaration.mtd_uid root
        | Module_member (_, declaration) ->
          record_module_context declaration.md_uid
            (Context.Proj (root, declaration.md_uid))
        | Other_member -> ())
      signature
  in
  let path_application_hook :
      (site:Location.t -> Env.t -> Path.t -> Path.t -> unit) ref =
    ref (fun ~site:_ _ _ _ -> ())
  in
  (* Naming another compilation unit must not read that unit's interface:
     [Env.find_module] loads the .cmi to recover [md_uid], which would make the
     facts of a unit depend on which interfaces happen to sit on the load path
     rather than on its source (see
     [testsuite/tests/reproducibility/cmis_on_file_system.ml]). Name persistent
     units the way [Env.find_shape] does, from the module name alone, so that
     the same source always produces the same facts. *)
  let persistent_unit_uid (path : Path.t) =
    match path with
    | Path.Pident id
      when Ident.is_global id && not (Current_unit.Name.is_ident id) ->
      (Shape.for_persistent_unit (Ident.name id)).Shape.uid
    | Path.Pident _ | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ -> None
  in
  let rec context_of_path ?site env (path : Path.t) : Context.t option =
    match persistent_unit_uid path with
    | Some uid -> Some (module_context uid)
    | None -> (
      match find_module env path with
      | None -> None
      | Some declaration -> (
        let uid = declaration.Types.md_uid in
        match path with
        | Path.Pident _ -> Some (module_context uid)
        | Path.Pdot (prefix, _) ->
          Option.map
            (fun prefix -> Context.Proj (prefix, uid))
            (context_of_path ?site env prefix)
        | Path.Papply (functor_, argument) -> (
          (match site with
          | Some site -> !path_application_hook ~site env functor_ argument
          | None -> ());
          match
            ( context_of_path ?site env functor_,
              context_of_path ?site env argument )
          with
          | Some functor_, Some argument ->
            Some (Context.App (functor_, argument))
          | (Some _ | None), _ -> None)
        | Path.Pextra_ty _ -> None))
  in
  let context_of_path ?site env path =
    let path =
      (* Normalizing would load the interface too, for no gain: a compilation
         unit is never an alias. *)
      match persistent_unit_uid path with
      | Some _ -> path
      | None -> normalize_module_path env path
    in
    context_of_path ?site env path
  in
  let rec report_path_applications ~site env (path : Path.t) =
    match path with
    | Path.Pident _ -> ()
    | Path.Pdot (prefix, _) | Path.Pextra_ty (prefix, _) ->
      report_path_applications ~site env prefix
    | Path.Papply (functor_, argument) ->
      report_path_applications ~site env functor_;
      report_path_applications ~site env argument;
      !path_application_hook ~site env functor_ argument
  in
  let report_longident_applications ~site env (lid : Longident.t) =
    match lid with
    | Longident.Ldot (prefix, _) when longident_contains_apply prefix.txt -> (
      match
        Env.lookup_module_path ~use:false ~loc:site ~load:false prefix.txt env
      with
      | path, _ -> report_path_applications ~site env path
      | exception _ -> ())
    | Longident.Lident _ | Longident.Ldot _ | Longident.Lapply _ -> ()
  in
  let named_signature_owner env (module_type : Types.module_type) =
    let owner_of_modtype_path path =
      match find_modtype env path with
      | Some declaration -> Some (Context.Body declaration.Types.mtd_uid)
      | None -> None
    in
    match (module_type : Types.module_type) with
    | Mty_ident path | Mty_strengthen (Mty_ident path, _, _) ->
      owner_of_modtype_path path
    | Mty_alias target -> (
      match declared_module_type_path env target with
      | Some declared -> owner_of_modtype_path declared
      | None -> None)
    | Mty_signature _ | Mty_functor _ | Mty_strengthen _ -> None
  in
  let functor_result_owner env (functor_type : Types.module_type) =
    match Mtype.scrape_alias env functor_type with
    | Mty_functor (_, result, _) -> named_signature_owner env result
    | Mty_ident _ | Mty_signature _ | Mty_alias _ | Mty_strengthen _ -> None
  in
  let functor_result_family env (functor_ : Types.module_declaration) =
    match functor_result_owner env functor_.md_type with
    | Some owner -> owner
    | None -> Context.Body functor_.md_uid
  in
  let rec family_context_of_path env (path : Path.t) : Context.t option =
    match path with
    | Path.Papply (functor_, _) -> (
      match find_module env functor_ with
      | Some declaration -> functor_result_owner env declaration.Types.md_type
      | None -> None)
    | Path.Pdot (prefix, _) when path_contains_apply prefix -> (
      match find_module env path, family_context_of_path env prefix with
      | Some declaration, Some family -> (
        match named_signature_owner env declaration.Types.md_type with
        | Some owner -> Some owner
        | None -> Some (Context.Proj (family, declaration.Types.md_uid)))
      | (Some _ | None), _ -> None)
    | Path.Pident _ | Path.Pdot _ | Path.Pextra_ty _ -> None
  in
  let named_key ?family context uid =
    let key = Key.Named { context; family_uid = uid } in
    (match
       match family with
       | Some family -> Some family
       | None -> family_context context
     with
    | Some family ->
      add_dependency key
        (Key.Named { context = family; family_uid = uid })
        Dependency.Reason.Instance
    | None -> ());
    key
  in
  let key_of_modtype_path ~site env (path : Path.t) : Key.t option =
    match path with
    | Path.Papply _ | Path.Pextra_ty _ ->
      add_omission Omission.Reason.Unsupported_path;
      None
    | Path.Pdot (prefix, _) -> (
      match find_modtype env path with
      | None -> None
      | Some declaration -> (
        let uid = declaration.Types.mtd_uid in
        let prefix = normalize_module_path env prefix in
        match context_of_path ~site env prefix with
        | Some context ->
          Some
            (named_key ?family:(family_context_of_path env prefix) context uid)
        | None ->
          let key = named_key (Context.Def uid) uid in
          add_omission ~affected:key ~source:uid
            Omission.Reason.Unresolved_module;
          Some key))
    | Path.Pident _ -> (
      match find_modtype env path with
      | None -> None
      | Some declaration -> (
        let uid = declaration.Types.mtd_uid in
        match Uid.Map.find_opt uid !modtype_contexts with
        | Some context -> Some (named_key context uid)
        | None -> Some (named_key (Context.Def uid) uid)))
  in
  let key_of_module_type (module_type : Typedtree.module_type) =
    match module_type.mty_desc with
    | Tmty_ident (path, _) -> (
      match
        key_of_modtype_path ~site:module_type.mty_loc module_type.mty_env path
      with
      | Some key -> key
      | None ->
        let key = Key.Anon { key_uid = module_type.mty_uid } in
        add_omission ~affected:key Omission.Reason.Unresolved_module_type;
        key)
    | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _
    | Tmty_alias _ | Tmty_strengthen _ ->
      Key.Anon { key_uid = module_type.mty_uid }
  in
  let uid_of_module_path env path =
    match find_normalized_module env path with
    | Some declaration -> Some declaration.Types.md_uid
    | None -> None
  in
  let node_of_module_path env ~loc path =
    match uid_of_module_path env path with
    | Some uid -> Node.Uid uid
    | None -> Node.Location (compilation_unit, loc)
  in
  let add_subject_expectation_edges key ~site env reason path =
    let path = normalize_module_path env path in
    match find_module env path with
    | None -> add_omission ~affected:key Omission.Reason.Unresolved_module
    | Some declaration -> (
      let uid = declaration.Types.md_uid in
      add_dependency key (Key.Anon { key_uid = uid }) reason;
      Option.iter
        (fun expectation -> add_dependency key expectation reason)
        (Uid.Map.find_opt uid !binding_expectations);
      match declared_module_type_path env path with
      | Some declared -> (
        match key_of_modtype_path ~site env declared with
        | Some source -> add_dependency key source reason
        | None ->
          add_omission ~affected:key Omission.Reason.Unresolved_module_type)
      | None -> ())
  in
  let node_of_module_expr (module_expr : module_expr) =
    let module_expr = unwrap_implicit_constraint module_expr in
    match module_expr.mod_desc with
    | Tmod_ident (path, _) ->
      node_of_module_path module_expr.mod_env ~loc:module_expr.mod_loc path
    | Tmod_structure _ | Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_constraint _ | Tmod_unpack _ ->
      Node.Location (compilation_unit, module_expr.mod_loc)
  in
  let module Handled = Set.Make (struct
    type t = Key.t * Location.t

    let compare = compare_pair Key.compare Location.compare
  end) in
  let handled_checks = ref Handled.empty in
  let handled expectation site =
    Handled.mem (expectation, site) !handled_checks
  in
  let mark_handled expectation site =
    handled_checks := Handled.add (expectation, site) !handled_checks
  in
  let relevance_cache : bool Uid.Map.t ref = ref Uid.Map.empty in
  let is_relevant_expectation (module_type : Typedtree.module_type) =
    is_relevant_expectation relevance_cache module_type
  in
  let rec instance_members env ~instance ~family (signature : Types.signature) =
    List.iter
      (fun item ->
        match classify_signature_member item with
        | Modtype_member (_, declaration) ->
          add_dependency
            (Key.Named { context = instance; family_uid = declaration.mtd_uid })
            (Key.Named { context = family; family_uid = declaration.mtd_uid })
            Dependency.Reason.Instance
        | Module_member (_, declaration) ->
          let instance = Context.Proj (instance, declaration.md_uid) in
          let family =
            match named_signature_owner env declaration.md_type with
            | Some owner -> owner
            | None -> Context.Proj (family, declaration.md_uid)
          in
          Option.iter
            (instance_members env ~instance ~family)
            (scraped_signature env declaration.md_type)
        | Other_member -> ())
      signature
  in
  let register_application_members ~root (module_expr : module_expr) =
    let module_expr = unwrap_implicit_constraint module_expr in
    let functor_path =
      match module_expr.mod_desc with
      | Tmod_apply (functor_, _, _, _, _) | Tmod_apply_unit (functor_, _) ->
        path_of_module_expr (unwrap_implicit_constraint functor_)
      | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_constraint _
      | Tmod_unpack _ ->
        None
    in
    match functor_path with
    | None -> ()
    | Some functor_path -> (
      match find_normalized_module module_expr.mod_env functor_path with
      | None -> ()
      | Some functor_declaration -> (
        let family =
          functor_result_family module_expr.mod_env functor_declaration
        in
        match scraped_signature module_expr.mod_env module_expr.mod_type with
        | Some signature ->
          instance_members module_expr.mod_env ~instance:root ~family signature;
          record_member_contexts root signature
        | None -> ()))
  in
  let rec signature_component env ~prefix_indexes index (path : Path.t) =
    match path with
    | Path.Pident id -> String_map.find_opt (Ident.name id) index.module_members
    | Path.Pdot (prefix, name) -> (
      match prefix_index env ~prefix_indexes index prefix with
      | Some prefix_index ->
        String_map.find_opt name prefix_index.module_members
      | None -> None)
    | Path.Papply _ | Path.Pextra_ty _ -> None
  and prefix_index env ~prefix_indexes index (prefix : Path.t) =
    match Path_map.find_opt prefix !prefix_indexes with
    | Some prefix_index -> Some prefix_index
    | None ->
      Option.map
        (fun (declaration : Types.module_declaration) ->
          let built =
            signature_index_of_module_type env declaration.Types.md_type
          in
          prefix_indexes := Path_map.add prefix built !prefix_indexes;
          built)
        (signature_component env ~prefix_indexes index prefix)
  in
  let register_argument_members ~derived ~parameter_type env argument_source =
    let rec walk ~source (parameter_type : Types.module_type) =
      match scraped_signature env parameter_type with
      | Some signature ->
        let source_index =
          match source with
          | `Path _ -> empty_signature_index
          | `Type module_type -> signature_index_of_module_type env module_type
        in
        List.iter
          (fun item ->
            match classify_signature_member item with
            | Modtype_member (name, _) -> (
              let source_key =
                match source with
                | `Path path ->
                  key_of_modtype_path ~site:Location.none env
                    (Path.Pdot (path, name))
                | `Type _ -> (
                  match
                    String_map.find_opt name source_index.modtype_members
                  with
                  | Some source_declaration -> (
                    let uid = source_declaration.Types.mtd_uid in
                    match Uid.Map.find_opt uid !modtype_contexts with
                    | Some context -> Some (named_key context uid)
                    | None ->
                      add_omission ~affected:derived ~source:uid
                        Omission.Reason.Unresolved_module;
                      None)
                  | None -> None)
              in
              match source_key with
              | Some source_key ->
                add_dependency derived source_key
                  Dependency.Reason.Argument_member
              | None ->
                add_omission ~affected:derived
                  Omission.Reason.Unresolved_module_type)
            | Module_member (name, declaration) -> (
              let member_source =
                match source with
                | `Path path -> Some (`Path (Path.Pdot (path, name)))
                | `Type _ -> (
                  match
                    String_map.find_opt name source_index.module_members
                  with
                  | Some member -> Some (`Type member.Types.md_type)
                  | None -> None)
              in
              match member_source with
              | Some member_source ->
                walk ~source:member_source declaration.md_type
              | None ->
                add_omission ~affected:derived Omission.Reason.Unresolved_module
              )
            | Other_member -> ())
          signature
      | None -> ()
    in
    walk ~source:argument_source parameter_type
  in
  let emit_argument_check ~site ~anchor env ~parameter_type ~expectation
      ~functor_instance argument_node argument_source =
    let instance_key uid =
      let key = Key.Named { context = anchor (); family_uid = uid } in
      (match expectation with
      | Some parameter_uid ->
        add_dependency key
          (Key.Anon { key_uid = parameter_uid })
          Dependency.Reason.Instance
      | None -> ());
      key
    in
    let instance_scoped parameter_uid =
      let expectation_key = instance_key parameter_uid in
      add_check argument_node expectation_key Check.Kind.Argument site;
      if functor_instance
      then
        List.iter
          (fun source ->
            add_omission ~affected:expectation_key ~source
              Omission.Reason.Missing_parameter_expectation)
          (named_modtype_uids env parameter_type);
      expectation_key
    in
    let derived =
      match (parameter_type : Types.module_type) with
      | Mty_strengthen (Mty_ident path, subject, _) -> (
        let key =
          match find_modtype env path with
          | Some declaration -> Some (instance_key declaration.Types.mtd_uid)
          | None -> Option.map instance_key expectation
        in
        match key with
        | Some key ->
          add_check argument_node key Check.Kind.Argument site;
          (match key_of_modtype_path ~site env path with
          | Some base -> add_dependency key base Dependency.Reason.Strengthening
          | None ->
            add_omission ~affected:key Omission.Reason.Unresolved_module_type);
          add_subject_expectation_edges key ~site env
            Dependency.Reason.Strengthening subject;
          Some key
        | None ->
          add_omission Omission.Reason.Missing_parameter_expectation;
          None)
      | Mty_ident path -> (
        match key_of_modtype_path ~site env path, find_modtype env path with
        | Some named_key, Some declaration ->
          if has_argument_members env parameter_type
          then begin
            let key = instance_key declaration.Types.mtd_uid in
            add_check argument_node key Check.Kind.Argument site;
            add_dependency key named_key Dependency.Reason.Instance;
            Some key
          end
          else begin
            add_check argument_node named_key Check.Kind.Argument site;
            Some named_key
          end
        | (Some _ | None), _ -> (
          match expectation with
          | Some parameter_uid -> Some (instance_scoped parameter_uid)
          | None ->
            add_omission Omission.Reason.Missing_parameter_expectation;
            None))
      | Mty_signature _ | Mty_functor _ | Mty_alias _ | Mty_strengthen _ -> (
        match expectation with
        | Some parameter_uid -> Some (instance_scoped parameter_uid)
        | None ->
          List.iter
            (fun source ->
              add_omission ~source Omission.Reason.Missing_parameter_expectation)
            (named_modtype_uids env parameter_type);
          None)
    in
    match derived with
    | Some derived ->
      register_argument_members ~derived ~parameter_type env argument_source
    | None -> ()
  in
  let () =
    path_application_hook
      := fun ~site env functor_path argument_path ->
           match find_normalized_module env functor_path with
           | None -> ()
           | Some functor_declaration -> (
             match Mtype.scrape_alias env functor_declaration.Types.md_type with
             | Mty_functor (Named (_, parameter_type, expectation, _), _, _) ->
               let argument_node =
                 node_of_module_path env ~loc:site argument_path
               in
               let anchor () =
                 match
                   context_of_path env
                     (Path.Papply (functor_path, argument_path))
                 with
                 | Some context -> context
                 | None -> fresh_site ()
               in
               emit_argument_check ~site ~anchor env ~parameter_type
                 ~expectation
                 ~functor_instance:(path_contains_apply functor_path)
                 argument_node (`Path argument_path)
             | Mty_functor (Unit, _, _)
             | Mty_ident _ | Mty_signature _ | Mty_alias _ | Mty_strengthen _ ->
               ())
  in
  let register_functor_ascription (inner : module_expr)
      (interface_type : Types.module_type) =
    let rec join_parameters env ~body_context ~interface_context
        (body_type : Types.module_type) (interface_type : Types.module_type) =
      match scraped_signature env body_type with
      | Some signature ->
        let interface_index =
          signature_index_of_module_type env interface_type
        in
        List.iter
          (fun item ->
            match classify_signature_member item with
            | Modtype_member (name, declaration) -> (
              let derived =
                Key.Named
                  { context = body_context; family_uid = declaration.mtd_uid }
              in
              match
                String_map.find_opt name interface_index.modtype_members
              with
              | Some interface_declaration ->
                add_dependency derived
                  (Key.Named
                     { context = interface_context;
                       family_uid = interface_declaration.Types.mtd_uid
                     })
                  Dependency.Reason.Interface
              | None ->
                add_omission ~affected:derived ~source:declaration.mtd_uid
                  Omission.Reason.Unresolved_module_type)
            | Module_member (name, declaration) -> (
              match String_map.find_opt name interface_index.module_members with
              | Some interface_member ->
                join_parameters env
                  ~body_context:
                    (Context.Proj (body_context, declaration.md_uid))
                  ~interface_context:
                    (Context.Proj
                       (interface_context, interface_member.Types.md_uid))
                  declaration.md_type interface_member.Types.md_type
              | None -> ())
            | Other_member -> ())
          signature
      | None -> ()
    in
    let rec loop (inner : module_expr) (interface_type : Types.module_type) =
      let inner = unwrap_implicit_constraint inner in
      match inner.mod_desc, Mtype.scrape_alias inner.mod_env interface_type with
      | ( Tmod_functor (Named (_, _, body_parameter, _), body, _),
          Mty_functor
            ( Named (_, interface_parameter_type, interface_parameter, _),
              result,
              _ ) ) ->
        (match interface_parameter with
        | Some interface_uid ->
          join_parameters inner.mod_env
            ~body_context:(Context.Def body_parameter.mty_uid)
            ~interface_context:(Context.Def interface_uid)
            body_parameter.mty_type interface_parameter_type
        | None ->
          List.iter
            (fun uid ->
              add_omission ~source:uid
                Omission.Reason.Missing_parameter_expectation)
            (named_modtype_uids inner.mod_env body_parameter.mty_type));
        loop body result
      | Tmod_functor (Unit, body, _), Mty_functor (Unit, result, _) ->
        loop body result
      | ( ( Tmod_functor _ | Tmod_ident _ | Tmod_structure _ | Tmod_apply _
          | Tmod_apply_unit _ | Tmod_constraint _ | Tmod_unpack _ ),
          _ ) ->
        ()
    in
    loop inner interface_type
  in
  let register_binding_expectation uid (implementation : module_expr) =
    match Uid.Map.find_opt uid !binding_expectations with
    | Some expectation -> Some expectation
    | None -> (
      let register expectation =
        binding_expectations
          := Uid.Map.add uid expectation !binding_expectations;
        add_dependency
          (Key.Anon { key_uid = uid })
          expectation Dependency.Reason.Interface;
        Some expectation
      in
      match implementation.mod_desc with
      | Tmod_constraint (_, _, Tmodtype_explicit (module_type, _), _) ->
        if is_relevant_expectation module_type
        then register (key_of_module_type module_type)
        else None
      | Tmod_constraint (inner, _, Tmodtype_package path, _) -> (
        match key_of_modtype_path ~site:inner.mod_loc inner.mod_env path with
        | Some expectation -> register expectation
        | None -> None)
      | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_apply _
      | Tmod_apply_unit _ | Tmod_constraint _ | Tmod_unpack _ ->
        None)
  in
  let register_ascription_member_pairs uid (inner : module_expr)
      (module_type : Typedtree.module_type) =
    let site = inner.mod_loc in
    let rec resolve_member visited env (member_type : Types.module_type) =
      match member_type with
      | Types.Mty_signature body -> `Signature (visited, None, body)
      | Types.Mty_ident path -> (
        match find_modtype env path with
        | None -> `Unresolved
        | Some declaration -> (
          let owner_uid = declaration.Types.mtd_uid in
          if Uid.Set.mem owner_uid visited
          then `Unresolved
          else
            match declaration.Types.mtd_type with
            | None -> `Abstract
            | Some manifest -> (
              match
                resolve_member (Uid.Set.add owner_uid visited) env manifest
              with
              | `Signature (visited, None, body) ->
                `Signature (visited, Some (Context.Body owner_uid), body)
              | (`Signature (_, Some _, _) | `Abstract | `Unresolved | `Other)
                as resolved ->
                resolved)))
      | Types.Mty_alias _ | Types.Mty_strengthen _ -> (
        match scraped_alias_signature env member_type with
        | Some body -> `Signature (visited, None, body)
        | None -> `Other)
      | Types.Mty_functor _ -> `Other
    in
    let rec pair_members visited env ~body_context ~ascribed_context
        (body_signature : Types.signature) (ascribed : Types.signature) =
      let body_index = index_of_signature body_signature in
      let env = Env.add_signature body_signature env in
      let (_ : Env.t) =
        List.fold_left
          (fun env (item : Types.signature_item) ->
            (match classify_signature_member item with
            | Modtype_member (name, declaration) -> (
              match declaration.mtd_type with
              | None -> ()
              | Some _ -> (
                match String_map.find_opt name body_index.modtype_members with
                | Some body_declaration
                  when not
                         (Uid.equal body_declaration.Types.mtd_uid
                            declaration.mtd_uid) ->
                  add_dependency
                    (Key.Named
                       { context = ascribed_context;
                         family_uid = declaration.mtd_uid
                       })
                    (named_key body_context body_declaration.Types.mtd_uid)
                    Dependency.Reason.Interface
                | Some _ | None -> ()))
            | Module_member (name, declaration) -> (
              match String_map.find_opt name body_index.module_members with
              | Some body_declaration
                when not
                       (Uid.equal body_declaration.Types.md_uid
                          declaration.md_uid) -> (
                if Uid.for_actual_declaration declaration.md_uid
                then
                  add_check
                    (if Uid.for_actual_declaration body_declaration.Types.md_uid
                     then Node.Uid body_declaration.Types.md_uid
                     else Node.Location (compilation_unit, site))
                    (Key.Anon { key_uid = declaration.md_uid })
                    Check.Kind.Ascription site;
                match resolve_member visited env declaration.Types.md_type with
                | `Signature (visited, ascribed_owner, ascribed_members) -> (
                  match
                    resolve_member Uid.Set.empty env
                      body_declaration.Types.md_type
                  with
                  | `Signature (_, body_owner, body_members) ->
                    let ascribed_context =
                      match ascribed_owner with
                      | Some owner -> owner
                      | None ->
                        Context.Proj (ascribed_context, declaration.md_uid)
                    in
                    let body_context =
                      match body_owner with
                      | Some owner -> owner
                      | None ->
                        Context.Proj
                          (body_context, body_declaration.Types.md_uid)
                    in
                    pair_members visited env ~body_context ~ascribed_context
                      body_members ascribed_members
                  | `Abstract | `Unresolved | `Other ->
                    add_omission Omission.Reason.Unresolved_module)
                | `Unresolved ->
                  add_omission Omission.Reason.Unresolved_module_type
                | `Abstract | `Other -> ())
              | Some _ | None -> ())
            | Other_member -> ());
            Env.add_signature [item] env)
          env ascribed
      in
      ()
    in
    let inner = unwrap_implicit_constraint inner in
    match scraped_signature module_type.mty_env module_type.mty_type with
    | Some ascribed -> (
      match scraped_alias_signature inner.mod_env inner.mod_type with
      | Some body_signature ->
        let context = module_context uid in
        let body_context =
          match path_of_module_expr inner with
          | Some path -> (
            match context_of_path inner.mod_env path with
            | Some subject -> subject
            | None -> context)
          | None -> context
        in
        pair_members Uid.Set.empty module_type.mty_env ~body_context
          ~ascribed_context:context body_signature ascribed
      | None -> ())
    | None -> ()
  in
  let add_binding uid (implementation : module_expr) =
    match implementation.mod_desc with
    | Tmod_constraint (inner, _, Tmodtype_explicit (module_type, _), _) -> (
      register_functor_ascription inner module_type.mty_type;
      register_ascription_member_pairs uid inner module_type;
      match register_binding_expectation uid implementation with
      | Some expectation ->
        mark_handled expectation inner.mod_loc;
        add_check (Node.Uid uid) expectation Check.Kind.Ascription inner.mod_loc
      | None -> ())
    | Tmod_constraint (inner, _, Tmodtype_package _, _) -> (
      match register_binding_expectation uid implementation with
      | Some expectation ->
        mark_handled expectation inner.mod_loc;
        add_check (Node.Uid uid) expectation Check.Kind.Package inner.mod_loc
      | None ->
        add_omission
          ~affected:(Key.Anon { key_uid = uid })
          Omission.Reason.Unresolved_module_type)
    | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_apply _
    | Tmod_apply_unit _ | Tmod_constraint _ | Tmod_unpack _ -> (
      let unwrapped = unwrap_implicit_constraint implementation in
      match path_of_module_expr unwrapped with
      | Some path -> (
        match context_of_path unwrapped.mod_env path with
        | Some target -> add_equality (module_context uid) target
        | None -> ())
      | None -> (
        match unwrapped.mod_desc with
        | Tmod_apply _ | Tmod_apply_unit _ ->
          register_application_members ~root:(module_context uid) unwrapped
        | Tmod_ident _ | Tmod_structure _ | Tmod_functor _ | Tmod_constraint _
        | Tmod_unpack _ ->
          ()))
  in
  let register_functor_parameter ~body_env ident
      (parameter : Typedtree.module_type) =
    (match ident with
    | None -> ()
    | Some ident -> (
      match find_module body_env (Path.Pident ident) with
      | Some declaration ->
        record_module_context declaration.Types.md_uid
          (Context.Def parameter.mty_uid);
        parameter_expectations
          := Uid.Map.add declaration.Types.md_uid parameter.mty_uid
               !parameter_expectations
      | None -> ()));
    let named = key_of_module_type parameter in
    if not (Key.equal named (Key.Anon { key_uid = parameter.mty_uid }))
    then
      add_dependency
        (Key.Anon { key_uid = parameter.mty_uid })
        named Dependency.Reason.Alias
  in
  let register_include_functor ~site env (functor_type : Types.module_type) =
    match Mtype.scrape_alias env functor_type with
    | Mty_functor (Named (_, parameter_type, expectation, _), _, _) ->
      emit_argument_check ~site
        ~anchor:(fun () -> fresh_site ())
        env ~parameter_type ~expectation ~functor_instance:false
        (Node.Location (compilation_unit, site))
        (`Type (Types.Mty_signature []))
    | Mty_functor (Unit, _, _)
    | Mty_ident _ | Mty_signature _ | Mty_alias _ | Mty_strengthen _ ->
      ()
  in
  let register_structure_include (include_ : include_declaration) =
    let context = enclosing_context () in
    let unwrapped = unwrap_implicit_constraint include_.incl_mod in
    let register_members ?(equalities = true) root =
      record_member_contexts root include_.incl_type;
      if not (Context.equal root context)
      then
        List.iter
          (fun item ->
            match classify_signature_member item with
            | Modtype_member (_, declaration) ->
              add_dependency
                (Key.Named { context; family_uid = declaration.mtd_uid })
                (Key.Named { context = root; family_uid = declaration.mtd_uid })
                Dependency.Reason.Include
            | Module_member (_, declaration) ->
              if equalities
              then
                add_equality
                  (Context.Proj (context, declaration.md_uid))
                  (Context.Proj (root, declaration.md_uid))
            | Other_member -> ())
          include_.incl_type
    in
    match include_.incl_kind with
    | Tincl_functor _ | Tincl_gen_functor _ -> (
      register_include_functor ~site:include_.incl_loc unwrapped.mod_env
        unwrapped.mod_type;
      let root = fresh_site () in
      register_members ~equalities:false root;
      match path_of_module_expr unwrapped with
      | Some functor_path -> (
        match find_normalized_module unwrapped.mod_env functor_path with
        | Some functor_declaration ->
          instance_members unwrapped.mod_env ~instance:root
            ~family:
              (functor_result_family unwrapped.mod_env functor_declaration)
            include_.incl_type
        | None -> ())
      | None -> ())
    | Tincl_structure -> (
      match path_of_module_expr unwrapped with
      | Some path when not (path_contains_apply path) -> (
        match context_of_path unwrapped.mod_env path with
        | Some root -> register_members root
        | None ->
          List.iter
            (fun item ->
              match classify_signature_member item with
              | Modtype_member (_, declaration) ->
                add_omission
                  ~affected:
                    (Key.Named { context; family_uid = declaration.mtd_uid })
                  ~source:declaration.mtd_uid Omission.Reason.Unresolved_module
              | Module_member _ | Other_member -> ())
            include_.incl_type)
      | Some path -> (
        match context_of_path unwrapped.mod_env path with
        | Some root ->
          register_members root;
          register_application_members ~root unwrapped
        | None -> register_members context)
      | None -> (
        match unwrapped.mod_desc with
        | Tmod_apply _ | Tmod_apply_unit _ ->
          let root = fresh_site () in
          register_members ~equalities:false root;
          register_application_members ~root unwrapped
        | Tmod_structure _ -> ()
        | Tmod_ident _ | Tmod_functor _ | Tmod_constraint _ | Tmod_unpack _ ->
          register_members context))
  in
  let interface_root =
    match artifact with
    | Artifact.Interface -> Some unit_uid
    | Artifact.Implementation -> None
  in
  let when_interface_root f =
    match !enclosing with
    | [_] -> Option.iter f interface_root
    | [] | _ :: _ :: _ -> ()
  in
  let functor_body_context uid (module_expr : module_expr) =
    let rec is_functor (module_expr : module_expr) =
      match module_expr.mod_desc with
      | Tmod_functor _ -> true
      | Tmod_constraint (inner, _, _, _) -> is_functor inner
      | Tmod_ident _ | Tmod_structure _ | Tmod_apply _ | Tmod_apply_unit _
      | Tmod_unpack _ ->
        false
    in
    if is_functor module_expr then Context.Body uid else module_context uid
  in
  let iterator =
    { Tast_iterator.default_iterator with
      structure_item =
        (fun iterator item ->
          (match item.str_desc with
          | Tstr_include include_ -> register_structure_include include_
          | Tstr_recmodule bindings ->
            List.iter
              (fun binding ->
                record_module_context binding.mb_uid
                  (Context.Proj (enclosing_context (), binding.mb_uid)))
              bindings;
            List.iter
              (fun binding ->
                ignore
                  (register_binding_expectation binding.mb_uid binding.mb_expr
                    : Key.t option))
              bindings
          | Tstr_eval _ | Tstr_value _ | Tstr_primitive _ | Tstr_type _
          | Tstr_typext _ | Tstr_exception _ | Tstr_module _ | Tstr_modtype _
          | Tstr_open _ | Tstr_class _ | Tstr_class_type _ | Tstr_attribute _
          | Tstr_jkind _ ->
            ());
          Tast_iterator.default_iterator.structure_item iterator item);
      signature_item =
        (fun iterator item ->
          (match item.sig_desc with
          | Tsig_include (include_, _) -> (
            (match include_.incl_kind with
            | Tincl_functor _ | Tincl_gen_functor _ ->
              register_include_functor ~site:include_.incl_loc
                include_.incl_mod.mty_env include_.incl_mod.mty_type
            | Tincl_structure -> ());
            when_interface_root (fun unit_uid ->
                if is_relevant_expectation include_.incl_mod
                then
                  add_dependency
                    (Key.Anon { key_uid = unit_uid })
                    (key_of_module_type include_.incl_mod)
                    Dependency.Reason.Interface);
            match include_.incl_mod.mty_desc with
            | Tmty_ident (path, _) -> (
              match find_modtype include_.incl_mod.mty_env path with
              | Some included ->
                let context = enclosing_context () in
                let root = Context.Body included.Types.mtd_uid in
                record_member_contexts context include_.incl_type;
                List.iter
                  (fun item ->
                    match classify_signature_member item with
                    | Modtype_member (_, declaration) ->
                      add_dependency
                        (Key.Named { context; family_uid = declaration.mtd_uid })
                        (Key.Named
                           { context = root; family_uid = declaration.mtd_uid })
                        Dependency.Reason.Include
                    | Module_member _ | Other_member -> ())
                  include_.incl_type
              | None -> ())
            | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _
            | Tmty_alias _ | Tmty_strengthen _ ->
              ())
          | Tsig_recmodule declarations ->
            List.iter
              (fun declaration ->
                record_module_context declaration.md_uid
                  (Context.Proj (enclosing_context (), declaration.md_uid)))
              declarations;
            when_interface_root (fun unit_uid ->
                List.iter
                  (fun (declaration : Typedtree.module_declaration) ->
                    if is_relevant_expectation declaration.md_type
                    then
                      add_dependency
                        (Key.Anon { key_uid = unit_uid })
                        (Key.Anon { key_uid = declaration.md_uid })
                        Dependency.Reason.Interface)
                  declarations)
          | Tsig_module declaration ->
            when_interface_root (fun unit_uid ->
                if is_relevant_expectation declaration.md_type
                then
                  add_dependency
                    (Key.Anon { key_uid = unit_uid })
                    (Key.Anon { key_uid = declaration.md_uid })
                    Dependency.Reason.Interface)
          | Tsig_modtype declaration | Tsig_modtypesubst declaration ->
            when_interface_root (fun unit_uid ->
                add_dependency
                  (Key.Anon { key_uid = unit_uid })
                  (Key.Named
                     { context = enclosing_context ();
                       family_uid = declaration.mtd_uid
                     })
                  Dependency.Reason.Interface)
          | Tsig_open open_ ->
            let path, _ = open_.open_expr in
            report_path_applications ~site:open_.open_loc open_.open_env path
          | Tsig_value _ | Tsig_type _ | Tsig_typesubst _ | Tsig_typext _
          | Tsig_exception _ | Tsig_modsubst _ | Tsig_class _
          | Tsig_class_type _ | Tsig_attribute _ | Tsig_jkind _ ->
            ());
          Tast_iterator.default_iterator.signature_item iterator item);
      typ =
        (fun iterator core_type ->
          (match core_type.ctyp_desc with
          | Ttyp_constr (path, _, _)
          | Ttyp_class (path, _, _)
          | Ttyp_open (path, _, _)
          | Ttyp_package { tpt_path = path; _ } ->
            report_path_applications ~site:core_type.ctyp_loc core_type.ctyp_env
              path
          | _ -> ());
          Tast_iterator.default_iterator.typ iterator core_type);
      pat =
        (fun (type k) iterator (pattern : k general_pattern) ->
          (match pattern.pat_desc with
          | Tpat_construct (lid, _, _, _, _) ->
            report_longident_applications ~site:lid.loc pattern.pat_env lid.txt
          | _ -> ());
          Tast_iterator.default_iterator.pat iterator pattern);
      class_expr =
        (fun iterator class_expr ->
          (match class_expr.cl_desc with
          | Tcl_ident (path, lid, _) ->
            report_path_applications ~site:lid.loc class_expr.cl_env path
          | _ -> ());
          Tast_iterator.default_iterator.class_expr iterator class_expr);
      class_type =
        (fun iterator class_type ->
          (match class_type.cltyp_desc with
          | Tcty_constr (path, lid, _) ->
            report_path_applications ~site:lid.loc class_type.cltyp_env path
          | _ -> ());
          Tast_iterator.default_iterator.class_type iterator class_type);
      expr =
        (fun iterator expression ->
          (match expression.exp_desc with
          | Texp_ident { path; _ } ->
            report_path_applications ~site:expression.exp_loc expression.exp_env
              path
          | Texp_new (path, lid, _, _) ->
            report_path_applications ~site:lid.loc expression.exp_env path
          | Texp_construct (lid, _, _, _, _) ->
            report_longident_applications ~site:lid.loc expression.exp_env
              lid.txt
          | Texp_field { lid; _ } | Texp_setfield { lid; _ } ->
            report_longident_applications ~site:lid.loc expression.exp_env
              lid.txt
          | _ -> ());
          match expression.exp_desc with
          | Texp_letmodule (Some _, _, _, uid, module_expr, _) ->
            record_module_context uid (Context.Proj (enclosing_context (), uid));
            add_binding uid module_expr;
            with_enclosing (functor_body_context uid module_expr) (fun () ->
                Tast_iterator.default_iterator.expr iterator expression)
          | _ -> Tast_iterator.default_iterator.expr iterator expression);
      module_expr =
        (fun iterator module_expr ->
          (match module_expr.mod_desc with
          | Tmod_constraint
              (implementation, _, Tmodtype_explicit (module_type, _), _) ->
            if is_relevant_expectation module_type
            then begin
              let expectation = key_of_module_type module_type in
              if not (handled expectation implementation.mod_loc)
              then begin
                register_functor_ascription implementation module_type.mty_type;
                add_check
                  (Node.Location (compilation_unit, module_expr.mod_loc))
                  expectation Check.Kind.Ascription implementation.mod_loc
              end
            end
          | Tmod_constraint (implementation, _, Tmodtype_package path, _) -> (
            match
              key_of_modtype_path ~site:implementation.mod_loc
                implementation.mod_env path
            with
            | Some expectation ->
              if not (handled expectation implementation.mod_loc)
              then
                add_check
                  (Node.Location (compilation_unit, module_expr.mod_loc))
                  expectation Check.Kind.Package implementation.mod_loc
            | None -> add_omission Omission.Reason.Unresolved_module_type)
          | Tmod_functor _ -> ()
          | Tmod_apply (functor_, argument, _, _, _) -> (
            (match path_of_module_expr module_expr with
            | Some path when path_contains_apply path -> (
              match context_of_path module_expr.mod_env path with
              | Some root -> register_application_members ~root module_expr
              | None -> ())
            | Some _ | None -> ());
            match Mtype.scrape_alias functor_.mod_env functor_.mod_type with
            | Mty_functor (Named (_, parameter_type, expectation, _), _, _) ->
              let functor_instance =
                match
                  path_of_module_expr (unwrap_implicit_constraint functor_)
                with
                | Some path -> path_contains_apply path
                | None -> (
                  match (unwrap_implicit_constraint functor_).mod_desc with
                  | Tmod_apply _ | Tmod_apply_unit _ -> true
                  | Tmod_ident _ | Tmod_structure _ | Tmod_functor _
                  | Tmod_constraint _ | Tmod_unpack _ ->
                    false)
              in
              let argument_source =
                match
                  path_of_module_expr (unwrap_implicit_constraint argument)
                with
                | Some path -> `Path path
                | None -> `Type argument.mod_type
              in
              let anchor () =
                let context =
                  match path_of_module_expr module_expr with
                  | Some path -> context_of_path module_expr.mod_env path
                  | None -> None
                in
                match context with
                | Some context -> context
                | None -> fresh_site ()
              in
              emit_argument_check ~site:argument.mod_loc ~anchor
                argument.mod_env ~parameter_type ~expectation ~functor_instance
                (node_of_module_expr argument)
                argument_source
            | Mty_functor (Unit, _, _)
            | Mty_ident _ | Mty_signature _ | Mty_alias _ | Mty_strengthen _ ->
              ())
          | Tmod_ident (path, _) ->
            report_path_applications ~site:module_expr.mod_loc
              module_expr.mod_env path
          | Tmod_constraint (_, _, Tmodtype_implicit, _)
          | Tmod_structure _ | Tmod_apply_unit _ | Tmod_unpack _ ->
            ());
          match module_expr.mod_desc with
          | Tmod_functor (parameter, body, _) ->
            scoped (fun () ->
                (match parameter with
                | Named (ident, _, parameter_type, _) ->
                  register_functor_parameter ~body_env:body.mod_env ident
                    parameter_type;
                  with_enclosing (Context.Def parameter_type.mty_uid) (fun () ->
                      iterator.module_type iterator parameter_type)
                | Unit -> ());
                iterator.module_expr iterator body)
          | Tmod_ident _ | Tmod_structure _ | Tmod_apply _ | Tmod_apply_unit _
          | Tmod_constraint _ | Tmod_unpack _ ->
            Tast_iterator.default_iterator.module_expr iterator module_expr);
      module_binding =
        (fun iterator binding ->
          record_module_context binding.mb_uid
            (Context.Proj (enclosing_context (), binding.mb_uid));
          (match binding.mb_id with
          | None -> ()
          | Some _ -> add_binding binding.mb_uid binding.mb_expr);
          with_enclosing (functor_body_context binding.mb_uid binding.mb_expr)
            (fun () ->
              Tast_iterator.default_iterator.module_binding iterator binding));
      module_declaration =
        (fun iterator declaration ->
          record_module_context declaration.md_uid
            (Context.Proj (enclosing_context (), declaration.md_uid));
          if is_relevant_expectation declaration.md_type
          then
            add_dependency
              (Key.Anon { key_uid = declaration.md_uid })
              (key_of_module_type declaration.md_type)
              Dependency.Reason.Interface;
          with_enclosing
            (Context.Proj (enclosing_context (), declaration.md_uid))
            (fun () ->
              Tast_iterator.default_iterator.module_declaration iterator
                declaration));
      module_type_declaration =
        (fun iterator declaration ->
          let context = enclosing_context () in
          record_modtype_context declaration.mtd_uid context;
          modtype_declaration_contexts
            := Uid.Map.add declaration.mtd_uid context
                 !modtype_declaration_contexts;
          let key = Key.Named { context; family_uid = declaration.mtd_uid } in
          (match declaration.mtd_type with
          | None -> ()
          | Some body -> (
            match body.mty_desc with
            | Tmty_ident (path, _) -> (
              match
                key_of_modtype_path ~site:body.mty_loc body.mty_env path
              with
              | Some source -> add_dependency key source Dependency.Reason.Alias
              | None ->
                add_omission ~affected:key
                  Omission.Reason.Unresolved_module_type)
            | Tmty_signature _ | Tmty_functor _ | Tmty_with _ | Tmty_typeof _
            | Tmty_alias _ | Tmty_strengthen _ ->
              if is_relevant_expectation body
              then
                add_dependency key
                  (Key.Anon { key_uid = body.mty_uid })
                  Dependency.Reason.Definition));
          with_enclosing (Context.Body declaration.mtd_uid) (fun () ->
              Tast_iterator.default_iterator.module_type_declaration iterator
                declaration));
      module_type =
        (fun iterator module_type ->
          let traverse () =
            let env = module_type.mty_env in
            let key = Key.Anon { key_uid = module_type.mty_uid } in
            (match module_type.mty_desc with
            | Tmty_ident (path, _) ->
              ignore
                (key_of_modtype_path ~site:module_type.mty_loc env path
                  : Key.t option)
            | Tmty_signature signature ->
              List.iter
                (fun item ->
                  match item.sig_desc with
                  | Tsig_include (include_, _) ->
                    if is_relevant_expectation include_.incl_mod
                    then
                      add_dependency key
                        (key_of_module_type include_.incl_mod)
                        Dependency.Reason.Include
                  | Tsig_module declaration ->
                    if is_relevant_expectation declaration.md_type
                    then
                      add_dependency key
                        (Key.Anon { key_uid = declaration.md_uid })
                        Dependency.Reason.Interface
                  | Tsig_recmodule declarations ->
                    List.iter
                      (fun (declaration : module_declaration) ->
                        if is_relevant_expectation declaration.md_type
                        then
                          add_dependency key
                            (Key.Anon { key_uid = declaration.md_uid })
                            Dependency.Reason.Interface)
                      declarations
                  | Tsig_modtype declaration | Tsig_modtypesubst declaration ->
                    add_dependency key
                      (Key.Named
                         { context = enclosing_context ();
                           family_uid = declaration.mtd_uid
                         })
                      Dependency.Reason.Interface
                  | Tsig_value _ | Tsig_type _ | Tsig_typesubst _
                  | Tsig_typext _ | Tsig_exception _ | Tsig_modsubst _
                  | Tsig_open _ | Tsig_class _ | Tsig_class_type _
                  | Tsig_attribute _ | Tsig_jkind _ ->
                    ())
                signature.sig_items
            | Tmty_with (base, constraints) ->
              if is_relevant_expectation base
              then
                add_dependency key (key_of_module_type base)
                  Dependency.Reason.With_constraint;
              let base_index =
                lazy (signature_index_of_module_type env base.mty_type)
              in
              let prefix_indexes = ref Path_map.empty in
              List.iter
                (fun (component, (lid : Longident.t Location.loc), constraint_)
                   ->
                  match constraint_ with
                  | Twith_modtype constraint_type ->
                    add_dependency key
                      (key_of_module_type constraint_type)
                      Dependency.Reason.With_constraint
                  | Twith_modtypesubst constraint_type ->
                    add_dependency key
                      (key_of_module_type constraint_type)
                      Dependency.Reason.Destructive_substitution
                  | Twith_module (rhs, _) | Twith_modsubst (rhs, _) -> (
                    add_subject_expectation_edges key ~site:lid.loc env
                      Dependency.Reason.With_constraint rhs;
                    match
                      signature_component env ~prefix_indexes
                        (Lazy.force base_index) component
                    with
                    | Some component_declaration ->
                      let node = node_of_module_path env ~loc:lid.loc rhs in
                      add_check node
                        (Key.Anon
                           { key_uid = component_declaration.Types.md_uid })
                        Check.Kind.Ascription lid.loc
                    | None ->
                      add_omission ~affected:key
                        Omission.Reason.Unresolved_module)
                  | Twith_type _ | Twith_typesubst _ | Twith_jkind _
                  | Twith_jkindsubst _ ->
                    ())
                constraints
            | Tmty_strengthen (base, path, _) ->
              (let node =
                 node_of_module_path env ~loc:module_type.mty_loc path
               in
               add_check node (key_of_module_type base) Check.Kind.Ascription
                 module_type.mty_loc);
              add_subject_expectation_edges key ~site:module_type.mty_loc env
                Dependency.Reason.Strengthening path;
              if is_relevant_expectation base
              then
                add_dependency key (key_of_module_type base)
                  Dependency.Reason.Strengthening
            | Tmty_functor (parameter, result, _) ->
              (match parameter with
              | Named (ident, _, parameter_type, _) ->
                register_functor_parameter ~body_env:result.mty_env ident
                  parameter_type;
                if is_relevant_expectation parameter_type
                then
                  add_dependency key
                    (Key.Anon { key_uid = parameter_type.mty_uid })
                    Dependency.Reason.Functor_type
              | Unit -> ());
              if is_relevant_expectation result
              then
                add_dependency key
                  (key_of_module_type result)
                  Dependency.Reason.Functor_type
            | Tmty_typeof implementation ->
              (let subject =
                 unwrap_implicit_constraint (typeof_subject implementation)
               in
               match path_of_module_expr subject with
               | Some path ->
                 add_subject_expectation_edges key ~site:module_type.mty_loc
                   subject.mod_env Dependency.Reason.Module_type_of path
               | None ->
                 add_omission ~affected:key Omission.Reason.Unresolved_module);
              List.iter
                (fun root ->
                  match find_module env (Path.Pident root) with
                  | Some declaration -> (
                    match
                      Uid.Map.find_opt declaration.Types.md_uid
                        !parameter_expectations
                    with
                    | Some parameter_uid ->
                      add_dependency key
                        (Key.Anon { key_uid = parameter_uid })
                        Dependency.Reason.Module_type_of
                    | None ->
                      add_omission ~affected:key
                        Omission.Reason.Unresolved_module)
                  | None ->
                    add_omission ~affected:key Omission.Reason.Unresolved_module)
                (functor_argument_roots env module_type.mty_type)
            | Tmty_alias (path, lid) ->
              add_subject_expectation_edges key ~site:lid.loc env
                Dependency.Reason.Alias path);
            match module_type.mty_desc with
            | Tmty_functor (parameter, result, _) ->
              (match parameter with
              | Named (_, _, parameter_type, _) ->
                with_enclosing (Context.Def parameter_type.mty_uid) (fun () ->
                    iterator.module_type iterator parameter_type)
              | Unit -> ());
              iterator.module_type iterator result
            | Tmty_ident _ | Tmty_signature _ | Tmty_with _ | Tmty_typeof _
            | Tmty_alias _ | Tmty_strengthen _ ->
              Tast_iterator.default_iterator.module_type iterator module_type
          in
          match module_type.mty_desc with
          | Tmty_functor _ -> scoped traverse
          | Tmty_ident _ | Tmty_signature _ | Tmty_with _ | Tmty_typeof _
          | Tmty_alias _ | Tmty_strengthen _ ->
            traverse ())
    }
  in
  iterate iterator;
  facts, fun uid -> Uid.Map.find_opt uid !modtype_declaration_contexts

let interface_check ~implementation ~expectation =
  { Check.implementation = Node.Uid implementation;
    expectation = Key.Anon { key_uid = expectation };
    kind = Check.Kind.Interface;
    site = Location.none
  }

let interface_pair_omissions reason ~implementation ~interface =
  [ { Omission.affected = None; source = Some implementation; reason };
    { Omission.affected = None; source = Some interface; reason } ]

let of_implementation compilation_unit ~module_pairs ~modtype_pairs
    ~unit_interface_check ~argument_interface structure =
  let facts, modtype_context =
    facts_of_tree compilation_unit Artifact.Implementation (fun iterator ->
        iterator.structure iterator structure)
  in
  let unit_uid = Uid.of_compilation_unit_id compilation_unit in
  List.iter
    (fun (implementation, interface) ->
      Builder.add_check facts
        (interface_check ~implementation ~expectation:interface))
    module_pairs;
  if unit_interface_check
  then
    Builder.add_check facts
      (interface_check ~implementation:unit_uid ~expectation:unit_uid);
  Option.iter
    (fun expectation ->
      Builder.add_check facts
        (interface_check ~implementation:unit_uid ~expectation))
    argument_interface;
  let interface_uid_of_impl =
    let table =
      List.fold_left
        (fun table (implementation, interface) ->
          Uid.Map.add implementation interface table)
        Uid.Map.empty module_pairs
    in
    fun uid -> Uid.Map.find_opt uid table
  in
  let rec translate_context (context : Context.t) : Context.t option =
    match context with
    | Context.Def _ -> Some context
    | Context.Proj (inner, uid) -> (
      match interface_uid_of_impl uid, translate_context inner with
      | Some interface, Some inner -> Some (Context.Proj (inner, interface))
      | (Some _ | None), _ -> None)
    | Context.App _ | Context.Body _ | Context.Site _ -> None
  in
  List.iter
    (fun (implementation, interface) ->
      let unrepresentable reason =
        List.iter
          (Builder.add_omission facts)
          (interface_pair_omissions reason ~implementation ~interface)
      in
      match modtype_context implementation with
      | None -> unrepresentable Omission.Reason.Unresolved_module_type
      | Some context -> (
        match translate_context context with
        | Some interface_context ->
          Builder.add_dependency facts
            { Dependency.derived =
                Key.Named { context; family_uid = implementation };
              source =
                Key.Named
                  { context = interface_context; family_uid = interface };
              reason = Dependency.Reason.Interface
            }
        | None -> unrepresentable Omission.Reason.Unresolved_module))
    modtype_pairs;
  Builder.freeze facts

let of_interface compilation_unit ~argument_interface signature =
  let facts, (_ : Uid.t -> Context.t option) =
    facts_of_tree compilation_unit Artifact.Interface (fun iterator ->
        iterator.signature iterator signature)
  in
  Option.iter
    (fun expectation ->
      Builder.add_check facts
        (interface_check
           ~implementation:(Uid.of_compilation_unit_id compilation_unit)
           ~expectation))
    argument_interface;
  Builder.freeze facts
