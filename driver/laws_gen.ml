(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* Two substitutions give the [Pident]s of the laws a meaning in the
   generated file: in the type context, the items of the result of a
   functor [F] are qualified by its application to the fields of the choice
   module type; in the value context, by a module [I] bound to the
   application of [F] to the parameters of a first-class module [M]. *)

open Types

(* Laws and scopes *)

type law =
  { l_field : string;  (* the field of [each], qualified by its module *)
    l_local : string;  (* the name of the law within its scope *)
    l_display : string;  (* its path, for messages *)
    l_frames : frame list;  (* its scopes, outermost first *)
    l_source : law_description;  (* as written, for the [text] fields *)
    l_types : (Ident.t * type_expr) list;  (* the parameters, in the type
                                              context *)
    l_values : law_description;  (* in the value context: the clauses *)
  }

(* An applicative functor whose laws are generated. *)
and frame =
  { f_local : string;  (* the name of its choice in the enclosing scope *)
    f_display : string;  (* its path, for messages *)
    f_functor : Path.t;  (* the functor, in the enclosing value context *)
    f_params : (Ident.t * module_type) list;  (* in the type context *)
    f_choices : Ident.t;  (* [M], the choices of the scope *)
    f_instance : Ident.t;  (* [I], the application of the functor *)
  }

type item =
  | Law of law
  | Scope of frame * item list

type context =
  { cmi : string;
    env : Env.t;  (* to expand module types *)
    types : Subst.t;
    values : Subst.t;
    qualifier : string list;  (* the module path from the top, for fields *)
    relative : string list;  (* the module path from the scope *)
    display : string list;  (* the module path, for messages *)
    frames : frame list;  (* the enclosing scopes, innermost first *)
    skipped : string list ref;  (* laws that are not generated *)
  }

let error ~cmi fmt =
  Location.raise_errorf ~loc:(Location.in_file cmi) fmt

let fresh used base =
  let rec loop i =
    let name = if i = 0 then base else base ^ "_" ^ Int.to_string i in
    if Misc.Stdlib.String.Set.mem name !used then loop (i + 1)
    else (used := Misc.Stdlib.String.Set.add name !used; name)
  in
  loop 0

let add_items s prefix sg =
  List.fold_left
    (fun s item ->
       let p id = Path.Pdot (prefix, Ident.name id) in
       match item with
       | Sig_value (id, _, _) -> Subst.add_value id (p id) s
       | Sig_type (id, _, _, _) | Sig_typext (id, _, _, _)
       | Sig_class (id, _, _, _) | Sig_class_type (id, _, _, _) ->
           (* Extension constructors and classes are substituted like
              types, see [Env.prefix_idents]. *)
           Subst.add_type id (p id) s
       | Sig_module (id, _, _, _, _) -> Subst.add_module id (p id) s
       | Sig_modtype (id, _, _) -> Subst.add_modtype id (p id) s
       | Sig_jkind (id, _, _) -> Subst.add_jkind id (p id) s
       | Sig_law _ -> s)
    s sg

(* Module types are expanded through the environment, which loads the
   interfaces of other units from the load path. *)
let expand ctx mty = Mtype.scrape ctx.env mty

let cannot_expand ctx p =
  error ~cmi:ctx.cmi
    "@[The module type %a of %s cannot be expanded,@ so its laws cannot \
     be generated.@ Is its interface in the load path?@]"
    Printtyp.Doc.path p (String.concat "." ctx.display)

let qualify path name = String.concat "_" (path @ [name])

(* A functor application names no particular instance of its values. *)
let check_paths ctx display (ld : law_description) =
  List.iter
    (fun ((ns : Spec.namespace), path) ->
       match ns with
       | (Value | Extension) when Path.contains_apply path ->
           error ~cmi:ctx.cmi
             "@[<hov>The@ law@ %a@ refers@ to@ %a@ through@ a@ functor@ \
              application,@ which@ names@ no@ particular@ instance.@]"
             Misc.Style.inline_code display
             Misc.Style.inline_code (Path.name path)
       | Value | Extension | Type -> ())
    (List.concat_map Spec.paths (ld.law_conclusion :: ld.law_assumptions))

(* The laws within a module type, for messages. *)
let rec laws_within ctx display mty =
  match expand ctx mty with
  | Mty_signature sg ->
      List.concat_map
        (function
          | Sig_law (id, _, Exported) ->
              [String.concat "." (display @ [Ident.name id])]
          | Sig_module (id, _, md, _, _) ->
              laws_within ctx (display @ [Ident.name id]) md.md_type
          | _ -> [])
        sg
  | Mty_functor (_, res, _) -> laws_within ctx display res
  | Mty_ident _ | Mty_alias _ | Mty_strengthen _ -> []

let rec walk_signature ctx ~pty ~pvl sg =
  let ctx =
    { ctx with
      env = Env.add_signature sg ctx.env;
      types = add_items ctx.types pty sg;
      values = add_items ctx.values pvl sg }
  in
  List.concat_map (walk_item ctx ~pty ~pvl) sg

and walk_item ctx ~pty ~pvl = function
  | Sig_law (id, ld, Exported) ->
      let name = Ident.name id in
      let display = String.concat "." (ctx.display @ [name]) in
      check_paths ctx display ld;
      [ Law
          { l_field = qualify ctx.qualifier name;
            l_local = qualify ctx.relative name;
            l_display = display;
            l_frames = List.rev ctx.frames;
            l_source = ld;
            l_types =
              List.map (fun (x, ty) -> (x, Subst.type_expr ctx.types ty))
                ld.law_params;
            l_values = Subst.law_description ctx.values ld } ]
  | Sig_module (id, _, md, _, _) ->
      let name = Ident.name id in
      let ctx =
        { ctx with
          qualifier = ctx.qualifier @ [String.uncapitalize_ascii name];
          relative = ctx.relative @ [String.uncapitalize_ascii name];
          display = ctx.display @ [name] }
      in
      let pty = Path.Pdot (pty, name) and pvl = Path.Pdot (pvl, name) in
      begin match expand ctx md.md_type with
      | Mty_signature sg -> walk_signature ctx ~pty ~pvl sg
      | Mty_functor _ as mty -> walk_functor ctx ~pty ~pvl mty
      | Mty_alias _ ->
          (* The laws of the aliased module are the ones of its own
             interface. *)
          []
      | Mty_ident p -> cannot_expand ctx p
      | Mty_strengthen _ -> assert false
      end
  | Sig_law (_, _, Hidden) | Sig_modtype _
  | Sig_value _ | Sig_type _ | Sig_typext _ | Sig_class _
  | Sig_class_type _ | Sig_jkind _ ->
      (* The laws of module types are checked on the modules of these
         types, which carry them. *)
      []

(* An applicative functor, maybe with several parameters, is a scope. The
   laws of generative functors, whose types cannot be named outside of an
   application, are not generated.

   The parameters are fields of the choice module type of the scope. A
   parameter with the name of a parameter of an enclosing functor, which
   is a field of an enclosing choice module type, is renamed. *)
and walk_functor ctx ~pty ~pvl mty =
  let used =
    ref
      (Misc.Stdlib.String.Set.of_list
         (List.concat_map
            (fun frame ->
               List.map (fun (id, _) -> Ident.name id) frame.f_params)
            ctx.frames))
  in
  let rec parameters ctx acc = function
    | Mty_functor (Named (id, arg, _), res, _) ->
        let id =
          match id with Some id -> id | None -> Ident.create_local "Arg"
        in
        let name = fresh used (Ident.name id) in
        let field =
          if String.equal name (Ident.name id) then id
          else Ident.create_local name
        in
        let ctx =
          { ctx with
            env = Env.add_module ~arg:true id Mp_present arg ctx.env;
            types =
              (if field == id then ctx.types
               else Subst.add_module id (Pident field) ctx.types) }
        in
        parameters ctx ((id, field, arg) :: acc) (expand ctx res)
    | Mty_functor (Unit, _, _) -> None
    | res -> Some (ctx, List.rev acc, res)
  in
  match parameters ctx [] mty with
  | None ->
      ctx.skipped := !(ctx.skipped) @ laws_within ctx ctx.display mty;
      []
  | Some (inner, params, res) ->
      match res with
      | Mty_signature sg ->
          let depth = List.length ctx.frames in
          let numbered name =
            if depth = 0 then name else name ^ Int.to_string depth
          in
          let frame =
            { f_local = String.concat "_" ctx.relative;
              f_display = String.concat "." ctx.display;
              f_functor = pvl;
              f_params =
                (* The type of a parameter may refer to the earlier ones,
                   which may have been renamed. *)
                List.map
                  (fun (_, field, arg) ->
                     (field, Subst.modtype Keep inner.types arg))
                  params;
              f_choices = Ident.create_local (numbered "M");
              f_instance = Ident.create_local (numbered "I") }
          in
          let application =
            List.fold_left
              (fun f (_, field, _) -> Path.Papply (f, Pident field))
              pty params
          in
          let values =
            List.fold_left
              (fun s (id, field, _) ->
                 Subst.add_module id
                   (Path.Pdot (Pident frame.f_choices, Ident.name field)) s)
              ctx.values params
          in
          let ctx =
            { inner with
              values; relative = []; frames = frame :: ctx.frames }
          in
          begin match
            walk_signature ctx ~pty:application
              ~pvl:(Path.Pident frame.f_instance)
              sg
          with
          | [] -> []
          | items -> [ Scope (frame, items) ]
          end
      | Mty_ident p -> cannot_expand inner p
      | Mty_functor _ | Mty_alias _ | Mty_strengthen _ -> assert false

let rec laws_of_items items =
  List.concat_map
    (function Law law -> [law] | Scope (_, items) -> laws_of_items items)
    items

let rec frames_of_items items =
  List.concat_map
    (function
      | Law _ -> []
      | Scope (frame, items) -> frame :: frames_of_items items)
    items

(* The constructor of the input type of a law. It does not capture the
   predefined constructors, which clauses refer to without
   qualification. *)
let constructor law = String.capitalize_ascii (law.l_local ^ "_input")

let check_names ~cmi items =
  let duplicates what names =
    let rec check seen = function
      | [] -> ()
      | (name, by) :: rest ->
          begin match List.assoc_opt name seen with
          | Some by' ->
              error ~cmi
                "The %s %s and %s of the generated file have the same \
                 name %s."
                what by' by name
          | None -> check ((name, by) :: seen) rest
          end
    in
    check [] names
  in
  duplicates "laws"
    (List.map (fun law -> law.l_field, law.l_display) (laws_of_items items));
  let rec scope items =
    duplicates "choices"
      (List.map
         (function
           | Law law -> law.l_local, law.l_display
           | Scope (frame, _) -> frame.f_local, frame.f_display)
         items);
    List.iter
      (function
        | Law law ->
            let c = constructor law in
            if c.[0] = '_' then
              error ~cmi
                "The law %s cannot be given a constructor in the generated \
                 file: %s is not a valid constructor name."
                law.l_display c
        | Scope (_, items) -> scope items)
      items
  in
  scope items

let global_roots items =
  let roots = ref Ident.Set.empty in
  let add_roots p =
    List.iter
      (fun id -> if Ident.is_global id then roots := Ident.Set.add id !roots)
      (Path.heads p)
  in
  with_type_mark (fun mark ->
    let base = Btype.type_iterators mark in
    (* The type iterators visit the types of laws; the paths of their
       clauses must be visited too, also in the laws of the module types
       of parameters. *)
    let it_law_description it (ld : law_description) =
      base.it_law_description it ld;
      List.iter
        (fun clause ->
           List.iter (fun (_, p) -> add_roots p) (Spec.paths clause))
        (ld.law_conclusion :: ld.law_assumptions)
    in
    let it = { base with it_path = add_roots; it_law_description } in
    List.iter
      (fun law ->
         List.iter (fun (_, ty) -> it.it_type_expr it ty) law.l_types;
         it.it_law_description it law.l_values)
      (laws_of_items items);
    List.iter
      (fun frame ->
         add_roots frame.f_functor;
         List.iter (fun (_, mty) -> it.it_module_type it mty) frame.f_params)
      (frames_of_items items));
  Ident.Set.elements !roots

type names =
  { aliases : (Ident.t * Ident.t) list;  (* root, alias *)
    choice : string;  (* the parameter of [Instantiate] *)
  }

let choose_names items =
  let frames = frames_of_items items in
  let params =
    List.concat_map
      (fun frame -> List.map (fun (id, _) -> Ident.name id) frame.f_params)
      frames
  in
  let roots = global_roots items in
  let used =
    ref
      (Misc.Stdlib.String.Set.of_list
         (List.map Ident.name roots @ params
          @ List.concat_map
              (fun frame ->
                 [Ident.name frame.f_choices; Ident.name frame.f_instance])
              frames
          @ ["Law"; "Instantiate"; "C"; "CamlinternalLaw"]))
  in
  let aliases =
    List.map
      (fun id ->
         id, Ident.create_persistent (fresh used ("Laws_gen_" ^ Ident.name id)))
      roots
  in
  (* The fields for the parameters of functors in the choice module types
     must not shadow the parameter of [Instantiate]. *)
  let choice =
    fresh (ref (Misc.Stdlib.String.Set.of_list params)) "Choice"
  in
  { aliases; choice }

let alias_items names items =
  let s =
    List.fold_left
      (fun s (id, alias) -> Subst.add_module id (Path.Pident alias) s)
      Subst.identity names.aliases
  in
  let frame f =
    { f with
      f_functor = Subst.module_path s f.f_functor;
      f_params =
        List.map (fun (id, mty) -> (id, Subst.modtype Keep s mty)) f.f_params }
  in
  let rec items_ is = List.map item is
  and item = function
    | Law law ->
        Law
          { law with
            l_frames = List.map frame law.l_frames;
            l_types = List.map (fun (x, ty) -> (x, Subst.type_expr s ty))
                        law.l_types;
            l_values = Subst.law_description s law.l_values }
    | Scope (f, is) -> Scope (frame f, items_ is)
  in
  items_ items

(* The generated files, as parse trees *)

open Ast_helper

let mknoloc = Location.mknoloc

let lid name = mknoloc (Longident.Lident name)

let ldot m name =
  mknoloc (Longident.Ldot (mknoloc (Longident.Lident m), mknoloc name))

let evar name = Exp.ident (lid name)

let pvar name = Pat.var (mknoloc name)

let unit_pat = Pat.construct (lid "()") None

let unit_exp = Exp.construct (lid "()") None

let string s = Exp.constant (Const.string s)

let tconstr name args = Typ.constr (lid name) args

let arrow label a b = Typ.arrow label a b [] []

let package name = Typ.package (Typ.package_type name [])

let type_params a = [(a, (Asttypes.NoVariance, Asttypes.NoInjectivity))]

let rec elist = function
  | [] -> Exp.construct (lid "[]") None
  | e :: es ->
      Exp.construct (lid "::") (Some (Exp.tuple [(None, e); (None, elist es)]))

let no_constraint : Parsetree.function_constraint =
  { mode_annotations = []; ret_mode_annotations = [];
    ret_type_constraint = None }

let labelled_param label pat : Parsetree.function_param =
  { pparam_desc = Pparam_val (label, None, pat); pparam_loc = Location.none }

let param pat = labelled_param Asttypes.Nolabel pat

let fun_ params body =
  Exp.function_ params no_constraint (Pfunction_body body)

(* [fun params : ty -> body] *)
let fun_returning ty params body =
  let constraint_ : Parsetree.function_constraint =
    { no_constraint with ret_type_constraint = Some (Pconstraint ty) }
  in
  Exp.function_ params constraint_ (Pfunction_body body)

(* [(module M : S)] *)
let unpack name mty =
  Pat.constraint_ (Pat.unpack (mknoloc (Some name))) (Some (package mty)) []

let binding name body =
  Str.value Nonrecursive [Vb.mk (pvar name) body]

let val_ name ty = Sig.value (Val.mk (mknoloc name) ty)

(* The compiler has no conversion from [Types] to [Parsetree] for module
   types: those of the parameters of functors are printed and parsed
   back. *)
let module_type mty =
  Format.asprintf "%a@?" Printtyp.modtype mty
  |> Lexing.from_string |> Parse.module_type

(* Type annotations for the generated clauses, so that constructors and
   labels are resolved as in the source. *)
let annotate =
  Untypespec.head_type_annotation ~lident_of_path:Untypespec.lident_of_path

let input_type law = law.l_local ^ "_input"

let module_type_name frame = String.capitalize_ascii frame.f_local ^ "_choices"

let variable laws base =
  fresh
    (ref (Misc.Stdlib.String.Set.of_list
            (List.map (fun law -> law.l_field) laws)))
    base

let alias_bindings names =
  List.map
    (fun (root, alias) ->
       Str.module_
         (Mb.mk (mknoloc (Some (Ident.name alias)))
            (Mod.ident (lid (Ident.name root)))))
    names.aliases

let alias_declarations names =
  List.map
    (fun (root, alias) ->
       Sig.module_
         (Md.mk (mknoloc (Some (Ident.name alias)))
            (Mty.alias (lid (Ident.name root)))))
    names.aliases

(* [type 'a each = { l1 : 'a; ... }], or [unit] without laws *)
let each laws =
  let a = Typ.var "a" None in
  match laws with
  | [] ->
      Type.mk ~params:(type_params a) ~manifest:(tconstr "unit" [])
        (mknoloc "each")
  | laws ->
      let fields =
        List.map (fun law -> Type.field (mknoloc law.l_field) a) laws
      in
      Type.mk ~params:(type_params a) ~kind:(Ptype_record fields)
        (mknoloc "each")

let each_values laws =
  let value name params body = binding name (fun_ params body) in
  match laws with
  | [] ->
      [ binding "names" unit_exp;
        value "map"
          [param unit_pat; labelled_param (Labelled "f") (Pat.any ())]
          unit_exp;
        value "to_list" [param unit_pat] (elist []) ]
  | laws ->
      let field law = lid law.l_field in
      let fields =
        Pat.record (List.map (fun law -> (field law, pvar law.l_field)) laws)
          Closed
      in
      let f = variable laws "f" in
      [ binding "names"
          (Exp.record
             (List.map (fun law -> (field law, string law.l_field)) laws)
             None);
        value "map" [param fields; labelled_param (Labelled "f") (pvar f)]
          (Exp.record
             (List.map
                (fun law ->
                   (field law,
                    Exp.apply (evar f) [(Nolabel, evar law.l_field)]))
                laws)
             None);
        value "to_list" [param fields]
          (elist (List.map (fun law -> evar law.l_field) laws)) ]

let each_declarations =
  let a = Typ.var "a" None and b = Typ.var "b" None in
  let each ty = tconstr "each" [ty] in
  [ val_ "names" (each (tconstr "string" []));
    val_ "map"
      (arrow Nolabel (each a)
         (arrow (Labelled "f") (arrow Nolabel a b) (each b)));
    val_ "to_list" (arrow Nolabel (each a) (tconstr "list" [a]))
  ]

(* [type l_input = L_input : ('a : k1) ... . { x1 : t1; ... } -> l_input],
   quantifying the type variables of the law with their kinds *)
let input_declaration law =
  let vars, params = Out_type.tree_of_law_quantification law.l_types in
  let vars =
    List.map
      (fun (v, kind) ->
         (mknoloc v, Some (Parsetree_of_outcometree.jkind_annotation kind)))
      vars
  in
  let args : Parsetree.constructor_arguments =
    match params with
    | [] -> Pcstr_tuple []
    | params ->
        Pcstr_record
          (List.map
             (fun (x, ty) ->
                Type.field (mknoloc x) (Parsetree_of_outcometree.core_type ty))
             params)
  in
  let input = tconstr (input_type law) [] in
  Type.mk
    ~kind:
      (Ptype_variant
         [Type.constructor ~vars ~args ~res:input (mknoloc (constructor law))])
    (mknoloc (input_type law))

let choice_declaration names item =
  let choice ty = Typ.constr (ldot names.choice "t") [ty] in
  match item with
  | Law law ->
      val_ law.l_local (choice (tconstr (input_type law) []))
  | Scope (frame, _) ->
      val_ frame.f_local
        (choice (package (lid (module_type_name frame))))

let rec scope_module_type names (frame, items) =
  let params =
    List.map
      (fun (id, mty) ->
         Sig.module_
           (Md.mk (mknoloc (Some (Ident.name id))) (module_type mty)))
      frame.f_params
  in
  let inputs =
    List.map
      (function
        | Law law -> Sig.type_ Recursive [input_declaration law]
        | Scope (frame, items) ->
            Sig.modtype (scope_module_type names (frame, items)))
      items
  in
  let choices = List.map (choice_declaration names) items in
  Mtd.mk ~typ:(Mty.signature (Sg.mk (params @ inputs @ choices)))
    (mknoloc (module_type_name frame))

let module_types names items =
  List.filter_map
    (function
      | Law _ -> None
      | Scope (frame, items) -> Some (scope_module_type names (frame, items)))
    items
  @ [ Mtd.mk
        ~typ:(Mty.signature (Sg.mk (List.map (choice_declaration names) items)))
        (mknoloc "Choices") ]

let text clause =
  let clause =
    Untypespec.expression ~lident_of_path:Out_type.lident_of_path
      ~annotate:(fun _ -> None) clause
  in
  Format.asprintf "%a"
    (fun ppf e ->
       Format.pp_set_margin ppf max_int;
       Pprintast.expression ppf e)
    clause

type law_names =
  { choices : string;
    qualifier : string option;
    input : string;
  }

let qualified n name =
  match n.qualifier with
  | None -> lid name
  | Some m -> ldot m name

let law_constructor name arg = Exp.construct (ldot "Law" name) (Some arg)

(* [Law.Assertion { value = input; source = C.l; predicate = (fun (L_input
   { x1; ...; xn } : l_input) -> (clause : bool)); text = "..." }] *)
let clause_check ~check law n (clause, source) =
  let fields =
    match law.l_values.law_params with
    | [] -> None
    | params ->
        let field (x, _) = (lid (Ident.name x), pvar (Ident.name x)) in
        Some ([], Pat.record (List.map field params) Closed)
  in
  let input =
    Pat.constraint_
      (Pat.construct (qualified n (constructor law)) fields)
      (Some (Typ.constr (qualified n (input_type law)) []))
      []
  in
  let clause =
    Untypespec.expression ~lident_of_path:Untypespec.lident_of_path ~annotate
      clause
  in
  let predicate =
    fun_ [param input] (Exp.constraint_ clause (Some (tconstr "bool" [])) [])
  in
  law_constructor check
    (Exp.record
       [ (lid "value", evar n.input);
         (lid "source", Exp.ident (ldot n.choices law.l_local));
         (lid "predicate", predicate);
         (lid "text", string (text source)) ]
       None)

(* [Law.Bind (Law.Assumption {...}, fun () -> ... Law.Assertion {...})] *)
let clauses law n =
  let rec bind = function
    | [] ->
        clause_check ~check:"Assertion" law n
          (law.l_values.law_conclusion, law.l_source.law_conclusion)
    | assumption :: rest ->
        law_constructor "Bind"
          (Exp.tuple
             [ (None, clause_check ~check:"Assumption" law n assumption);
               (None, fun_ [param unit_pat] (bind rest)) ])
  in
  bind (List.combine law.l_values.law_assumptions law.l_source.law_assumptions)

(* [let l (module C : Choices) : unit Law.t = Law.Bind (Law.Choose C.f, fun
   (module M : F_choices) -> let module I = F (M.X) in ... Law.Bind
   (Law.Choose M.l, fun input -> clauses))] *)
let law_binding law =
  let input =
    fresh
      (ref (Misc.Stdlib.String.Set.of_list
              (List.map (fun (x, _) -> Ident.name x)
                 law.l_values.law_params)))
      "input"
  in
  let bind choices name k =
    law_constructor "Bind"
      (Exp.tuple
         [ (None, law_constructor "Choose" (Exp.ident (ldot choices name)));
           (None, k) ])
  in
  let rec scopes n = function
    | [] ->
        bind n.choices law.l_local (fun_ [param (pvar input)] (clauses law n))
    | frame :: frames ->
        let m = Ident.name frame.f_choices in
        let instance =
          List.fold_left
            (fun f (id, _) -> Mod.apply f (Mod.ident (ldot m (Ident.name id))))
            (Mod.ident (mknoloc (Untypespec.lident_of_path frame.f_functor)))
            frame.f_params
        in
        bind n.choices frame.f_local
          (fun_ [param (unpack m (qualified n (module_type_name frame)))]
             (Exp.letmodule
                (mknoloc (Some (Ident.name frame.f_instance)))
                instance
                (scopes { n with choices = m; qualifier = Some m } frames)))
  in
  binding law.l_field
    (fun_returning (Typ.constr (ldot "Law" "t") [tconstr "unit" []])
       [param (unpack "C" (lid "Choices"))]
       (scopes { choices = "C"; qualifier = None; input } law.l_frames))

let laws_type = tconstr "each" [Typ.constr (ldot "Law" "t") [tconstr "unit" []]]

let laws_binding laws =
  match laws with
  | [] ->
      binding "laws"
        (fun_returning laws_type
           [param (Pat.constraint_ (Pat.any ())
              (Some (package (lid "Choices"))) [])]
           unit_exp)
  | laws ->
      let choices = variable laws "choices" in
      binding "laws"
        (fun_ [param (pvar choices)]
           (Exp.record
              (List.map
                 (fun law ->
                    (lid law.l_field,
                     Exp.apply (evar law.l_field)
                       [(Nolabel, evar choices)]))
                 laws)
              None))

let top_laws items =
  List.filter_map (function Law law -> Some law | Scope _ -> None) items

let choice_parameter names : Parsetree.functor_parameter =
  Named
    (mknoloc (Some names.choice), Mty.ident (ldot "CamlinternalLaw" "Choice"),
     [])

let implementation names items =
  let laws = laws_of_items items in
  let law_module =
    Str.module_
      (Mb.mk (mknoloc (Some "Law"))
         (Mod.apply
            (Mod.ident (ldot "CamlinternalLaw" "Make"))
            (Mod.ident (lid names.choice))))
  in
  let instantiate =
    Mod.functor_ (choice_parameter names)
      (Mod.structure
         ((law_module :: List.map Str.modtype (module_types names items))
          @ List.map law_binding laws
          @ [laws_binding laws]))
  in
  (* Unused parameters of laws in clauses *)
  let warning =
    Str.attribute
      (Attr.mk (mknoloc "ocaml.warning") (PStr [Str.eval (string "-27")]))
  in
  (warning :: alias_bindings names)
  @ (Str.type_ Recursive [each laws] :: each_values laws)
  @ List.map
      (fun law -> Str.type_ Recursive [input_declaration law])
      (top_laws items)
  @ [Str.module_ (Mb.mk (mknoloc (Some "Instantiate")) instantiate)]

let interface names items =
  let laws = laws_of_items items in
  let law_module =
    (* [module Law : sig type 'a t = 'a CamlinternalLaw.Make(Choice).t end] *)
    let a = Typ.var "a" None in
    let make =
      Longident.Lapply
        (ldot "CamlinternalLaw" "Make", lid names.choice)
    in
    let t =
      Type.mk ~params:(type_params a)
        ~manifest:
          (Typ.constr (mknoloc (Longident.Ldot (mknoloc make, mknoloc "t")))
             [a])
        (mknoloc "t")
    in
    Sig.module_
      (Md.mk (mknoloc (Some "Law"))
         (Mty.signature (Sg.mk [Sig.type_ Recursive [t]])))
  in
  let laws_value =
    val_ "laws" (arrow Nolabel (package (lid "Choices")) laws_type)
  in
  let instantiate =
    Mty.functor_ (choice_parameter names)
      (Mty.signature
         (Sg.mk
            ((law_module :: List.map Sig.modtype (module_types names items))
             @ [laws_value])))
  in
  Sg.mk
    (alias_declarations names
     @ (Sig.type_ Recursive [each laws] :: each_declarations)
     @ List.map
         (fun law -> Sig.type_ Recursive [input_declaration law])
         (top_laws items)
     @ [Sig.module_ (Md.mk (mknoloc (Some "Instantiate")) instantiate)])

let generate file ~cmi ~output =
  (* Pprintast escapes keywords through the keyword table of the lexer. *)
  let keyword_edition =
    Clflags.(Option.map parse_keyword_edition !keyword_edition)
  in
  Lexer.init ?keyword_edition ();
  (* The interface is read through the environment, which gives its items
     fresh identifiers and loads the interfaces it refers to from the load
     path, to expand their module types. *)
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  let artifact =
    Unit_info.Artifact.from_filename
      ~for_pack_prefix:Compilation_unit.Prefix.empty cmi
  in
  let modname = Unit_info.Artifact.modname artifact in
  let sg, _ =
    Env.read_signature
      (Compilation_unit.to_global_name_without_prefix modname) artifact
  in
  let unit =
    Ident.create_persistent
      (Compilation_unit.Name.to_string (Compilation_unit.name modname))
  in
  Subst.enable_value_substitution ();
  let skipped = ref [] in
  let ctx =
    { cmi; env; types = Subst.identity; values = Subst.identity;
      qualifier = []; relative = []; display = []; frames = []; skipped }
  in
  let items = walk_signature ctx ~pty:(Pident unit) ~pvl:(Pident unit) sg in
  check_names ~cmi items;
  let names = choose_names items in
  let items = alias_items names items in
  if not (List.is_empty !skipped) then
    Format.eprintf
      "File %S:@.@[<hov 2>Warning: the laws of generative functors are not \
       generated:@ %a.@]@."
      cmi
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         Format.pp_print_string)
      !skipped;
  (* The file is rendered in memory first, so that an error leaves no
     partial output behind. *)
  let contents =
    match (file : Clflags.laws_file) with
    | Laws_implementation ->
        Format.asprintf "%a@." Pprintast.structure (implementation names items)
    | Laws_interface ->
        Format.asprintf "%a@." Pprintast.signature (interface names items)
  in
  match output with
  | None -> print_string contents
  | Some file ->
      Out_channel.with_open_bin file (fun oc ->
        Out_channel.output_string oc contents)
