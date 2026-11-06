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

(* The laws file of a unit [U] has the following shape, where [l1], ...,
   [ln] are the laws of [U], qualified by their modules, and [f_l] is the law
   [l] of an applicative functor [F] of [U]:
   {[
     [@@@ocaml.warning "-27-40"]
     module Laws_gen_U = U
     type 'a each = { l1 : 'a; ...; ln : 'a }
     let names = { l1 = "l1"; ...; ln = "ln" }
     let locations = { l1 = { CamlinternalLaw.file = "u.ml"; ... }; ... }
     let map { l1; ...; ln } ~f = { l1 = f l1; ...; ln = f ln }
     let to_list { l1; ...; ln } = [ l1; ...; ln ]
     type l1_input = L1_input : ... -> l1_input
     ...
     module Instantiate (Choice : CamlinternalLaw.Choice) = struct
       module Law = CamlinternalLaw.Make (Choice)
       let l1 (choice : l1_input Choice.t) : unit Law.t = ...
       ...
       module type F_l_choices = sig ... end
       let f_l (choice : (module F_l_choices) Choice.t) : unit Law.t = ...
       ...
     end
   ]}
   Each law is a function of its own choice, so a backend chooses the inputs
   of the laws one law at a time. The unit, and every other unit that the
   laws refer to, is referred to through an alias. *)

open Types
module String = Misc.Stdlib.String

(* Syntax *)

(* Locationless syntax for the generated file *)
module P : sig
  open Parsetree

  module Lid : sig
    val ident : string -> Longident.t
    val dot : Longident.t -> string -> Longident.t
  end

  module Exp : sig
    val ident : Longident.t -> expression
    val var : string -> expression
    val int : int -> expression
    val string : string -> expression
    val unit : expression
    val tuple : expression list -> expression
    val list : expression list -> expression
    val construct : Longident.t -> expression option -> expression
    val record : (Longident.t * expression) list -> expression

    val apply :
      expression -> (Asttypes.arg_label * expression) list -> expression

    val fun_ :
      ?return:core_type ->
      ?newtypes:(string * jkind_annotation option) list ->
      (Asttypes.arg_label * pattern) list ->
      expression ->
      expression
    (** [fun_ ~return ~newtypes params body] is
        [fun (type (a1 : k1) ...) params : return -> body]. *)

    val constraint_ : expression -> core_type -> expression
    val letmodule : string -> module_expr -> expression -> expression
  end

  module Pat : sig
    val var : string -> pattern
    val any : pattern
    val unit : pattern
    val construct : Longident.t -> pattern option -> pattern
    val record : (Longident.t * pattern) list -> pattern
    val constraint_ : pattern -> core_type -> pattern

    val unpack : string -> Longident.t -> pattern
    (** [unpack m s] is [((module m) : (module s))]. *)
  end

  module Typ : sig
    val var : string -> core_type
    val any : core_type
    val constr : Longident.t -> core_type list -> core_type
    val package : Longident.t -> core_type
  end

  module Jkind : sig
    val default : jkind_annotation
    val abbreviation : Longident.t -> jkind_annotation
    val mod_ : jkind_annotation -> mode list -> jkind_annotation
    val product : jkind_annotation list -> jkind_annotation
  end

  (** The parameters of a type declaration are its type variables with their
      kinds, as in [type ('a : k) t]. *)
  module Type : sig
    val abbrev :
      ?params:(string * jkind_annotation option) list ->
      string ->
      core_type ->
      type_declaration

    val record :
      ?params:(string * jkind_annotation option) list ->
      string ->
      (string * core_type) list ->
      type_declaration

    val variant :
      ?params:(string * jkind_annotation option) list ->
      string ->
      constructor_declaration list ->
      type_declaration

    val constructor :
      ?vars:(string * jkind_annotation option) list ->
      ?args:(string * core_type) list ->
      ?res:core_type ->
      string ->
      constructor_declaration
    (** [constructor ~vars ~args ~res c] is
        [c : ('a1 : k1) ... . { x1 : t1; ... } -> res], with no argument when
        [args] is empty. *)
  end

  module Mod : sig
    val ident : Longident.t -> module_expr
    val apply : module_expr -> module_expr -> module_expr
    val structure : structure_item list -> module_expr
    val functor_ : string -> module_type -> module_expr -> module_expr
  end

  module Mty : sig
    val ident : Longident.t -> module_type
    val signature : signature_item list -> module_type
  end

  module Sig : sig
    val value : string -> core_type -> signature_item
    val type_ : type_declaration -> signature_item
    val module_ : string -> module_type -> signature_item
    val modtype : string -> module_type -> signature_item
  end

  module Str : sig
    val value : string -> expression -> structure_item
    val type_ : type_declaration -> structure_item
    val module_ : string -> module_expr -> structure_item
    val modtype : string -> module_type -> structure_item

    val warning : string -> structure_item
    (** [warning spec] is [[@@@ocaml.warning spec]]. *)

    val text : string -> structure_item
    (** [text s] is the floating comment [(** s *)]. *)
  end
end = struct
  open Parsetree
  module H = Ast_helper

  let mknoloc = Location.mknoloc

  module Lid = struct
    let ident name : Longident.t = Lident name
    let dot m name : Longident.t = Ldot (mknoloc m, mknoloc name)
  end

  let type_params params =
    List.map
      (fun (a, k) ->
        (H.Typ.var a k, (Asttypes.NoVariance, Asttypes.NoInjectivity)))
      params

  let fields decls = List.map (fun (x, ty) -> H.Type.field (mknoloc x) ty) decls

  module Exp = struct
    let ident l = H.Exp.ident (mknoloc l)
    let var x = ident (Lid.ident x)
    let int n = H.Exp.constant (H.Const.int n)
    let string s = H.Exp.constant (H.Const.string s)
    let construct c arg = H.Exp.construct (mknoloc c) arg
    let unit = construct (Lid.ident "()") None
    let tuple es = H.Exp.tuple (List.map (fun e -> (None, e)) es)

    let rec list = function
      | [] -> construct (Lid.ident "[]") None
      | e :: es -> construct (Lid.ident "::") (Some (tuple [ e; list es ]))

    let record fields =
      H.Exp.record (List.map (fun (f, e) -> (mknoloc f, e)) fields) None

    let apply f args = H.Exp.apply f args

    let fun_ ?return ?(newtypes = []) params body =
      let param pparam_desc = { pparam_desc; pparam_loc = Location.none } in
      let params =
        List.map (fun (a, k) -> param (Pparam_newtype (mknoloc a, k))) newtypes
        @ List.map
            (fun (label, pat) -> param (Pparam_val (label, None, pat)))
            params
      in
      let constraint_ =
        {
          mode_annotations = [];
          ret_mode_annotations = [];
          ret_type_constraint = Option.map (fun ty -> Pconstraint ty) return;
        }
      in
      H.Exp.function_ params constraint_ (Pfunction_body body)

    let constraint_ e ty = H.Exp.constraint_ e (Some ty) []
    let letmodule m me body = H.Exp.letmodule (mknoloc (Some m)) me body
  end

  module Pat = struct
    let var x = H.Pat.var (mknoloc x)
    let any = H.Pat.any ()

    let construct c arg =
      H.Pat.construct (mknoloc c) (Option.map (fun p -> ([], p)) arg)

    let unit = construct (Lid.ident "()") None

    let record fields =
      H.Pat.record (List.map (fun (f, p) -> (mknoloc f, p)) fields) Closed

    let constraint_ p ty = H.Pat.constraint_ p (Some ty) []

    let unpack m s =
      constraint_
        (H.Pat.unpack (mknoloc (Some m)))
        (H.Typ.package (H.Typ.package_type (mknoloc s) []))
  end

  module Typ = struct
    let var x = H.Typ.var x None
    let any = H.Typ.any None
    let constr c args = H.Typ.constr (mknoloc c) args
    let package s = H.Typ.package (H.Typ.package_type (mknoloc s) [])
  end

  module Jkind = struct
    let mk pjka_desc = { pjka_desc; pjka_loc = Location.none }
    let default = mk Pjk_default
    let abbreviation l = mk (Pjk_abbreviation (mknoloc l))
    let mod_ k modes = mk (Pjk_mod (k, List.map mknoloc modes))
    let product ks = mk (Pjk_product ks)
  end

  module Type = struct
    let abbrev ?(params = []) name manifest =
      H.Type.mk ~params:(type_params params) ~manifest (mknoloc name)

    let record ?(params = []) name decls =
      H.Type.mk ~params:(type_params params)
        ~kind:(Ptype_record (fields decls))
        (mknoloc name)

    let variant ?(params = []) name constructors =
      H.Type.mk ~params:(type_params params) ~kind:(Ptype_variant constructors)
        (mknoloc name)

    let constructor ?(vars = []) ?(args = []) ?res name =
      let vars = List.map (fun (v, k) -> (mknoloc v, k)) vars in
      let args =
        match args with
        | [] -> Pcstr_tuple []
        | decls -> Pcstr_record (fields decls)
      in
      H.Type.constructor ~vars ~args ?res (mknoloc name)
  end

  module Mod = struct
    let ident l = H.Mod.ident (mknoloc l)
    let apply f arg = H.Mod.apply f arg
    let structure items = H.Mod.structure items

    let functor_ x mty body =
      H.Mod.functor_ (Named (mknoloc (Some x), mty, [])) body
  end

  module Mty = struct
    let ident l = H.Mty.ident (mknoloc l)
    let signature items = H.Mty.signature (H.Sg.mk items)
  end

  module Sig = struct
    let value x ty = H.Sig.value (H.Val.mk (mknoloc x) ty)
    let type_ decl = H.Sig.type_ Recursive [ decl ]
    let module_ m mty = H.Sig.module_ (H.Md.mk (mknoloc (Some m)) mty)
    let modtype s mty = H.Sig.modtype (H.Mtd.mk ~typ:mty (mknoloc s))
  end

  module Str = struct
    let value x e = H.Str.value Nonrecursive [ H.Vb.mk (Pat.var x) e ]
    let type_ decl = H.Str.type_ Recursive [ decl ]
    let module_ m me = H.Str.module_ (H.Mb.mk (mknoloc (Some m)) me)
    let modtype s mty = H.Str.modtype (H.Mtd.mk ~typ:mty (mknoloc s))

    let warning spec =
      H.Str.attribute
        (H.Attr.mk (mknoloc "ocaml.warning")
           (PStr [ H.Str.eval (Exp.string spec) ]))

    let text s =
      H.Str.attribute
        (H.Attr.mk (mknoloc "ocaml.text") (PStr [ H.Str.eval (Exp.string s) ]))
  end
end

(* Laws and scopes *)

(* The paths that the idents of a context stand for *)
type paths = Path.t Ident.Map.t

let rec reroot (paths : paths) (p : Path.t) : Path.t =
  match p with
  | Pident id -> Option.value (Ident.Map.find_opt id paths) ~default:p
  | Pdot (q, s) -> Pdot (reroot paths q, s)
  | Papply (f, arg) -> Papply (reroot paths f, reroot paths arg)
  | Pextra_ty (q, extra) -> Pextra_ty (reroot paths q, extra)

(* A clause that a backend tests, at its location *)
type test_clause = test_clause_desc * Location.t

and test_clause_desc =
  | Test_bool of Typedtree.expression
  | Test_equal of Typedtree.expression * Typedtree.expression
      (** [e1 =:= e2], tested with [Stdlib.( = )] *)
  | Test_and of test_clause * test_clause

type law = {
  (* The field of [each], qualified by the modules of the law. *)
  l_field : string;
  (* The name of the law in its scope. *)
  l_local : string;
  (* The path of the law, for messages. *)
  l_display : string;
  l_loc : Location.t;
  (* The scopes of the law, outermost first. *)
  l_frames : frame list;
  l_newtypes : Typedtree.law_newtype list;
  l_params : Typedtree.law_param list;
  l_assumptions : test_clause list;
  l_conclusion : test_clause;
  (* The paths of the idents of the law, in types *)
  l_paths : paths;
}

(* An applicative functor whose laws are generated: a scope. Its parameters
   are the fields of the choice module type [M] of the scope, and its result
   is its application [F (X1) ... (Xn)] in types and the module [I] in
   values. *)
and frame = {
  (* The name of its choice in the enclosing scope. *)
  f_local : string;
  f_functor : Path.t;
  f_params : (Ident.t * Typedtree.module_type) list;
  (* The paths of the idents of the parameters, in types *)
  f_paths : paths;
  f_application : Path.t;
  f_choices : Ident.t;
  f_instance : Ident.t;
}

type item = Law of law | Scope of frame * item list

let rec laws_of_items items =
  List.concat_map
    (function Law law -> [ law ] | Scope (_, items) -> laws_of_items items)
    items

let rec frames_of_items items =
  List.concat_map
    (function
      | Law _ -> [] | Scope (frame, items) -> frame :: frames_of_items items)
    items

let error ~loc fmt = Location.raise_errorf ~loc fmt

let fresh used base =
  let rec loop i =
    let name = if i = 0 then base else base ^ "_" ^ Int.to_string i in
    if String.Set.mem name !used then loop (i + 1)
    else (
      used := String.Set.add name !used;
      name)
  in
  loop 0

(* The laws of a unit *)

(* The laws of a unit are the ones written in it. The laws of a module
   that the unit refers to, applies or unpacks are generated where they
   are written, and the ones of a functor are tested under the backend's
   choice of its parameters rather than at each application. *)

(* The walk of a unit. [path] below is the path of the current module. *)
type context = {
  (* The unit bound to its interface, to check that the laws refer to
     exported items only. *)
  exported : Env.t;
  paths : paths;
  (* The module path from the top, for the fields of [each]. *)
  qualifier : string list;
  (* The module path from the scope. *)
  relative : string list;
  (* The module path, for messages. *)
  display : string list;
  (* The enclosing scopes, innermost first. *)
  frames : frame list;
}

let add_items paths prefix sg =
  List.fold_left
    (fun paths item ->
      let id = signature_item_id item in
      Ident.Map.add id (Path.Pdot (prefix, Ident.name id)) paths)
    paths sg

let qualify path name = String.concat "_" (path @ [ name ])

let enter ctx name =
  {
    ctx with
    qualifier = ctx.qualifier @ [ String.uncapitalize_ascii name ];
    relative = ctx.relative @ [ String.uncapitalize_ascii name ];
    display = ctx.display @ [ name ];
  }

(* CR sspies: The laws and the types of their parameters come from the
   implementation, while the generated file is compiled against the
   interface. A law whose types are more general than the interface, for
   example over the ['a : value_or_null] of an inferred [let f x = x]
   exported as [val f : 'a -> 'a], gives a laws file that does not
   compile. *)

(* A law may refer to exported items only. The items are looked up through
   the unit, in an environment that binds the unit to its interface. *)
(* CR sspies: The check belongs in typing, next to the other errors about
   laws, so that Merlin and [-stop-after typing] report it. It needs the
   paths of the law rooted at the unit, which only this walk computes. *)
let check_exported ctx ~loc display (ld : law_declaration) =
  let check what find p =
    let p = reroot ctx.paths p in
    match find p ctx.exported with
    | _ -> ()
    | exception Not_found ->
        error ~loc
          "@[The law %a refers to the %s %a,@ which the interface of the unit \
           does not export.@]"
          Misc.Style.inline_code display what
          (Misc.Style.as_inline_code Printtyp.Doc.path)
          p
  in
  List.iter
    (fun ((ns : Spec.namespace), p) ->
      match ns with
      | Value -> check "value" Env.find_value p
      | Type -> check "type" Env.find_type p
      | Extension -> check "extension constructor" Env.find_extension_alias p)
    (List.concat_map Spec.clause_paths
       (ld.law_conclusion :: ld.law_assumptions));
  with_type_mark (fun mark ->
      let it =
        {
          (Btype.type_iterators mark) with
          it_path = check "type" Env.find_type;
        }
      in
      List.iter (fun (_, ty, _) -> it.it_type_expr it ty) ld.law_params)

(* The laws that a backend cannot test: the reason, and the path of the law,
   in source order *)
let untested : (string * string) list ref = ref []

exception Untested of string

(* Whether values of type [ty] may contain functions, which [Stdlib.( = )]
   cannot compare. The declarations of types are inspected once. *)
let may_contain_functions env ty =
  let seen_paths = ref [] and seen = Hashtbl.create 16 in
  let rec go ty =
    let ty = Ctype.expand_head env ty in
    if Hashtbl.mem seen (get_id ty) then false
    else (
      Hashtbl.add seen (get_id ty) ();
      match get_desc ty with
      | Tarrow _ | Tobject _ | Tpackage _ -> true
      | Tconstr (p, args, _) ->
          List.exists go args
          || (not (List.exists (Path.same p) !seen_paths))
             &&
             (seen_paths := p :: !seen_paths;
              match Env.find_type p env with
              | decl ->
                  let found = ref false in
                  Btype.iter_type_expr_kind
                    (fun ty -> if go ty then found := true)
                    decl.type_kind;
                  !found
              | exception Not_found -> false)
      | _ ->
          let found = ref false in
          Btype.iter_type_expr (fun ty -> if go ty then found := true) ignore ty;
          !found)
  in
  go ty

let rec test_clause (c : Typedtree.law_clause) : test_clause =
  match c.lc_desc with
  | Tlc_bool e -> (Test_bool (Translspec.typedtree e), c.lc_loc)
  | Tlc_equal (e1, e2) ->
      let e1 = Translspec.typedtree e1 and e2 = Translspec.typedtree e2 in
      if may_contain_functions e1.exp_env e1.exp_type then
        raise
          (Untested "it compares values that may contain functions with =:=");
      (Test_equal (e1, e2), c.lc_loc)
  | Tlc_and (c1, c2) -> (Test_and (test_clause c1, test_clause c2), c.lc_loc)
  | Tlc_wp _ -> raise (Untested "it contains let%wp")

let law ctx (ld : Typedtree.law_declaration) =
  let name = Ident.name ld.law_id in
  let display = String.concat "." (ctx.display @ [ name ]) in
  check_exported ctx ~loc:ld.law_loc display ld.law_law;
  match
    (List.map test_clause ld.law_assumptions, test_clause ld.law_conclusion)
  with
  | exception Untested reason ->
      untested := (display, reason) :: !untested;
      []
  | l_assumptions, l_conclusion ->
      [
        Law
          {
            l_field = qualify ctx.qualifier name;
            l_local = qualify ctx.relative name;
            l_display = display;
            l_loc = ld.law_loc;
            l_frames = List.rev ctx.frames;
            l_newtypes = ld.law_newtypes;
            l_params = ld.law_params;
            l_assumptions;
            l_conclusion;
            l_paths = ctx.paths;
          };
      ]

let rec walk_structure ctx ~path (str : Typedtree.structure) =
  let ctx = { ctx with paths = add_items ctx.paths path str.str_type } in
  List.concat_map (walk_structure_item ctx ~path) str.str_items

and walk_structure_item ctx ~path (item : Typedtree.structure_item) =
  match item.str_desc with
  | Tstr_law ld -> law ctx ld
  | Tstr_module mb -> walk_module_binding ctx ~path mb
  | Tstr_recmodule mbs -> List.concat_map (walk_module_binding ctx ~path) mbs
  | Tstr_include { incl_kind = Tincl_structure; incl_mod; _ } ->
      walk_module_expr ctx ~path incl_mod
  | Tstr_include { incl_kind = Tincl_functor _ | Tincl_gen_functor _; _ }
  | Tstr_eval _ | Tstr_value _ | Tstr_primitive _ | Tstr_type _ | Tstr_typext _
  | Tstr_exception _ | Tstr_modtype _ | Tstr_open _ | Tstr_class _
  | Tstr_class_type _ | Tstr_attribute _ | Tstr_jkind _ ->
      []

and walk_module_binding ctx ~path (mb : Typedtree.module_binding) =
  match mb.mb_id with
  | None -> []
  | Some id ->
      let name = Ident.name id in
      walk_module_expr (enter ctx name)
        ~path:(Path.Pdot (path, name))
        mb.mb_expr

(* A constraint keeps the laws of the structure, also the ones that its
   signature hides. *)
and walk_module_expr ctx ~path (me : Typedtree.module_expr) =
  match me.mod_desc with
  | Tmod_structure str -> walk_structure ctx ~path str
  | Tmod_constraint (me, _, _, _) -> walk_module_expr ctx ~path me
  | Tmod_functor _ -> walk_functor ctx ~path me
  | Tmod_ident _ | Tmod_apply _ | Tmod_apply_unit _ | Tmod_unpack _ -> []

(* An applicative functor, maybe with several parameters, is a scope.
   Generative functors have no laws, see [Typemod]. *)
and walk_functor ctx ~path (me : Typedtree.module_expr) =
  let rec parameters acc (me : Typedtree.module_expr) =
    match me.mod_desc with
    | Tmod_functor (Named (id, _, mty, _), body, _) ->
        parameters ((id, mty) :: acc) body
    | Tmod_functor (Unit, _, _) -> None
    | Tmod_ident _ | Tmod_structure _ | Tmod_apply _ | Tmod_apply_unit _
    | Tmod_constraint _ | Tmod_unpack _ ->
        Some (List.rev acc, me)
  in
  match parameters [] me with
  | None -> []
  | Some (params, body) -> (
      let ctx, frame = scope ctx ~path params in
      match walk_module_expr ctx ~path:frame.f_application body with
      | [] -> []
      | items -> [ Scope (frame, items) ])

(* A parameter with the name of a parameter of an enclosing functor, which
   is a field of an enclosing choice module type, is renamed. *)
and scope ctx ~path params =
  let used =
    ref
      (String.Set.of_list
         (List.concat_map
            (fun frame ->
              List.map (fun (id, _) -> Ident.name id) frame.f_params)
            ctx.frames))
  in
  let ctx, params =
    List.fold_left_map
      (fun ctx (id, (mty : Typedtree.module_type)) ->
        let id =
          match id with Some id -> id | None -> Ident.create_local "Arg"
        in
        let name = fresh used (Ident.name id) in
        let field =
          if String.equal name (Ident.name id) then id
          else Ident.create_local name
        in
        let exported =
          Env.add_module ~arg:true field Mp_present mty.mty_type ctx.exported
        in
        let paths = Ident.Map.add id (Path.Pident field) ctx.paths in
        ({ ctx with exported; paths }, (field, mty)))
      ctx params
  in
  let depth = List.length ctx.frames in
  let numbered name = if depth = 0 then name else name ^ Int.to_string depth in
  let frame =
    {
      f_local = String.concat "_" ctx.relative;
      f_functor = path;
      f_params = params;
      f_paths = ctx.paths;
      f_application =
        List.fold_left
          (fun f (field, _) -> Path.Papply (f, Pident field))
          path params;
      f_choices = Ident.create_local (numbered "M");
      f_instance = Ident.create_local (numbered "I");
    }
  in
  ({ ctx with relative = []; frames = frame :: ctx.frames }, frame)

(* Names *)

let input_type law = law.l_local ^ "_input"

(* The constructor of the input type of a law. It does not capture the
   predefined constructors, which clauses refer to without
   qualification. *)
let constructor law = String.capitalize_ascii (input_type law)
let module_type_name frame = String.capitalize_ascii frame.f_local ^ "_choices"

(* The choice module type of a law in a scope *)
let law_module_type_name law = String.capitalize_ascii law.l_field ^ "_choices"

let check_names laws =
  let rec duplicates seen = function
    | [] -> ()
    | law :: rest ->
        begin match List.assoc_opt law.l_field seen with
        | Some by ->
            error ~loc:law.l_loc
              "The laws %s and %s of the generated laws file have the same \
               name %s."
              by law.l_display law.l_field
        | None -> duplicates ((law.l_field, law.l_display) :: seen) rest
        end
  in
  duplicates [] laws;
  List.iter
    (fun law ->
      let c = constructor law in
      if c.[0] = '_' then
        error ~loc:law.l_loc
          "The law %s cannot be given a constructor in the generated laws \
           file: %s is not a valid constructor name."
          law.l_display c)
    laws

(* Paths *)

(* The paths of the laws are rooted at the unit: the items of the unit are
   qualified by the unit. *)

(* In a scope, the items of the result of the functor are qualified by its
   application to the fields of the choice module type of the scope. In the
   clauses, which are values, the application is the instance [I] of the
   scope and the fields are the ones of the first-class module [M] of its
   choices. *)
let rec in_scope frames (p : Path.t) : Path.t =
  match List.find_opt (fun f -> Path.same f.f_application p) frames with
  | Some frame -> Pident frame.f_instance
  | None -> (
      match p with
      | Pident id -> (
          let is_param f =
            List.exists (fun (field, _) -> Ident.same field id) f.f_params
          in
          match List.find_opt is_param frames with
          | Some frame -> Pdot (Pident frame.f_choices, Ident.name id)
          | None -> p)
      | Pdot (q, s) -> Pdot (in_scope frames q, s)
      | Papply (f, arg) -> Papply (in_scope frames f, in_scope frames arg)
      | Pextra_ty (q, extra) -> Pextra_ty (in_scope frames q, extra))

(* [t#] is the constructor ["t#"], see [Parser.unboxed_type]. *)
let lident : Path.t -> Longident.t = function
  | Pextra_ty (Pident id, Punboxed_ty) -> Lident (Ident.name id ^ "#")
  | Pextra_ty (Pdot (q, name), Punboxed_ty) ->
      Untypeast.lident_of_path (Pdot (q, name ^ "#"))
  | p -> Untypeast.lident_of_path p

(* The syntax of typed trees, with the paths written by [print] *)

(* Constructors and labels are written unqualified, and the constructors,
   records and records of field accesses are annotated with their type,
   [(C : _ t)], so that the generated clause types with the same
   constructors and labels. The types of inline records have no syntax, but
   their constructor determines them. Extension constructors are written by
   their paths. *)
let annotation print ty =
  match get_desc ty with
  | Tconstr (Pextra_ty (_, (Pcstr_ty _ | Pext_ty)), _, _) -> None
  | Tconstr (p, args, _) ->
      Some (P.Typ.constr (print p) (List.map (fun _ -> P.Typ.any) args))
  | _ -> Misc.fatal_error "Laws_gen.annotation: not a type constructor"

let with_type print ty pe =
  Option.fold ~none:pe ~some:(P.Exp.constraint_ pe) (annotation print ty)

let with_type_pattern print ty pp =
  Option.fold ~none:pp ~some:(P.Pat.constraint_ pp) (annotation print ty)

let annotate print (e : Typedtree.expression) (pe : Parsetree.expression) =
  match (e.exp_desc, pe.pexp_desc) with
  | ( Texp_construct (_, { cstr_tag = Ordinary _ | Null; cstr_res; _ }, _, _, _),
      _ ) ->
      with_type print cstr_res pe
  | Texp_record { fields; _ }, _ when Array.length fields > 0 ->
      let lbl, _, _ = fields.(0) in
      with_type print lbl.lbl_res pe
  | Texp_record_unboxed_product { fields; _ }, _ when Array.length fields > 0 ->
      let lbl, _, _ = fields.(0) in
      with_type print lbl.lbl_res pe
  | Texp_field { label; _ }, Pexp_field (record, lid) ->
      {
        pe with
        pexp_desc = Pexp_field (with_type print label.lbl_res record, lid);
      }
  | Texp_unboxed_field { label; _ }, Pexp_unboxed_field (record, lid) ->
      {
        pe with
        pexp_desc =
          Pexp_unboxed_field (with_type print label.lbl_res record, lid);
      }
  | _ -> pe

let annotate_pattern (type k) print (p : k Typedtree.general_pattern)
    (pp : Parsetree.pattern) =
  match p.pat_desc with
  | Tpat_construct (_, { cstr_tag = Ordinary _ | Null; cstr_res; _ }, _, _, _)
    ->
      with_type_pattern print cstr_res pp
  | Tpat_record ((_, lbl, _) :: _, _, _) ->
      with_type_pattern print lbl.lbl_res pp
  | Tpat_record_unboxed_product ((_, lbl, _) :: _, _, _) ->
      with_type_pattern print lbl.lbl_res pp
  | _ -> pp

let constructor_lid print (cstr : Data_types.constructor_description) :
    Longident.t =
  match cstr.cstr_tag with
  | Extension path -> print path
  | Ordinary _ | Null -> Lident cstr.cstr_name

(* Local opens are dropped: paths are written in full, and constructors and
   labels are selected by their annotations, so nothing refers to the open. *)
let mapper print =
  {
    Untypeast.default_mapper with
    expr =
      (fun sub e ->
        match e.exp_desc with
        | Texp_open (_, body) ->
            sub.expr sub { body with exp_extra = e.exp_extra @ body.exp_extra }
        | _ -> annotate print e (Untypeast.default_mapper.expr sub e));
    pat =
      (fun (type k) sub (p : k Typedtree.general_pattern) ->
        annotate_pattern print p (Untypeast.default_mapper.pat sub p));
    ident = (fun _ path lid -> { lid with txt = print path });
    constructor =
      (fun _ cstr lid -> { lid with txt = constructor_lid print cstr });
    label = (fun _ lbl lid -> { lid with txt = Lident lbl.lbl_name });
  }

(* The types [(type a)] of [newtypes] are written as type variables. With
   [wildcards], the type variables that are not universal are written [_], and
   aliases are dropped. *)
let core_type ?(newtypes = []) ?(wildcards = false) print cty =
  let m = mapper print in
  let typ (sub : Untypeast.mapper) (cty : Typedtree.core_type) =
    match (cty.ctyp_desc, get_desc cty.ctyp_type) with
    | Ttyp_constr (Pident id, _, []), _ -> (
        match List.find_opt (fun (id', _) -> Ident.same id id') newtypes with
        | Some (_, name) -> P.Typ.var name
        | None -> m.typ sub cty)
    | Ttyp_var _, Tvar _ when wildcards -> P.Typ.any
    | Ttyp_alias (cty, _, _), _ when wildcards -> sub.typ sub cty
    | _ -> m.typ sub cty
  in
  let m = { m with typ } in
  m.typ m cty

let module_type print mty =
  let m = mapper print in
  m.module_type m mty

let expression print e =
  let m = mapper print in
  m.expr m e

let law_print print law p = print (reroot law.l_paths p)

let clause_print print law p =
  print (in_scope law.l_frames (reroot law.l_paths p))

(* Kinds *)

(* Kinds are converted through their outcome trees, which is how the
   compiler prints them. Not every outcome tree is a kind that can be
   written in a source file; [Unsupported] names the construct that cannot
   be. *)
module Jkind_of_outcometree = struct
  open Outcometree

  exception Unsupported of string

  let unsupported what = raise (Unsupported what)

  let rec jkind : out_jkind -> Parsetree.jkind_annotation = function
    | Ojkind_const const -> jkind_const const
    | Ojkind_product ks -> P.Jkind.product (List.map jkind ks)
    | Ojkind_var _ -> unsupported "a kind variable"
    | Ojkind_addressable _ -> unsupported "an addressable kind"
    | Ojkind_box _ -> unsupported "a boxed kind"

  and jkind_const : out_jkind_const -> Parsetree.jkind_annotation = function
    | Ojkind_const_default -> P.Jkind.default
    | Ojkind_const_abbreviation (name, []) ->
        P.Jkind.abbreviation (P.Lid.ident name)
    | Ojkind_const_abbreviation (_, _ :: _) -> unsupported "a kind with axes"
    | Ojkind_const_mod (Some base, ms) ->
        P.Jkind.mod_ (jkind_const base)
          (List.map (fun m -> Parsetree.Mode m) ms)
    | Ojkind_const_mod (None, _) -> unsupported "a mod kind without a base"
    | Ojkind_const_with _ -> unsupported "a kind with a with-bound"
    | Ojkind_const_kind_of _ -> unsupported "the kind of a type"
    | Ojkind_const_product consts ->
        P.Jkind.product (List.map jkind_const consts)
end

let printable_jkind ~loc name ty =
  match Out_type.tree_of_type_jkind ty with
  | None -> None
  | Some tree -> (
      try Some (Jkind_of_outcometree.jkind tree)
      with Jkind_of_outcometree.Unsupported construct ->
        error ~loc
          "@[The kind of %a@ cannot be written in the laws file:@ it contains \
           %s.@]"
          Misc.Style.inline_code ("'" ^ name) construct)

(* The generated file *)

let lid = P.Lid.ident
let ldot m name = P.Lid.dot (lid m) name
let tconstr name args = P.Typ.constr (lid name) args
let param pat = (Asttypes.Nolabel, pat)
let labelled_param label pat = (Asttypes.Labelled label, pat)

let variable laws base =
  fresh (ref (String.Set.of_list (List.map (fun law -> law.l_field) laws))) base

type names = {
  aliases : (Ident.t * Ident.t) list; (* root, alias *)
  choice : string; (* the parameter of [Instantiate] *)
  print : Path.t -> Longident.t;
}

(** Where a law or a clause is written, for the messages of the backend:
    {[
    {
      CamlinternalLaw.file = "u.ml";
      start_line = 3;
      start_col = 0;
      end_line = 3;
      end_col = 40;
    }
    ]} *)
let location (loc : Location.t) =
  let column (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol in
  P.Exp.record
    [
      (ldot "CamlinternalLaw" "file", P.Exp.string loc.loc_start.pos_fname);
      (lid "start_line", P.Exp.int loc.loc_start.pos_lnum);
      (lid "start_col", P.Exp.int (column loc.loc_start));
      (lid "end_line", P.Exp.int loc.loc_end.pos_lnum);
      (lid "end_col", P.Exp.int (column loc.loc_end));
    ]

(** The record of the laws, with one field per law:
    {[
      type 'a each = { l1 : 'a; ...; ln : 'a }
    ]}
    and [type 'a each = unit] without laws. *)
let each laws =
  match laws with
  | [] -> P.Type.abbrev ~params:[ ("a", None) ] "each" (tconstr "unit" [])
  | laws ->
      P.Type.record
        ~params:[ ("a", None) ]
        "each"
        (List.map (fun law -> (law.l_field, P.Typ.var "a")) laws)

(** The values of [each]:
    {[
      let names = { l1 = "l1"; ...; ln = "ln" }
      let locations = { l1 = { CamlinternalLaw.file = ...; ... }; ... }
      let map { l1; ...; ln } ~f = { l1 = f l1; ...; ln = f ln }
      let to_list { l1; ...; ln } = [ l1; ...; ln ]
    ]} *)
let each_values laws =
  let value name params body = P.Str.value name (P.Exp.fun_ params body) in
  match laws with
  | [] ->
      [
        P.Str.value "names" P.Exp.unit;
        P.Str.value "locations" P.Exp.unit;
        value "map"
          [ param P.Pat.unit; labelled_param "f" P.Pat.any ]
          P.Exp.unit;
        value "to_list" [ param P.Pat.unit ] (P.Exp.list []);
      ]
  | laws ->
      let field law = lid law.l_field in
      let fields =
        P.Pat.record
          (List.map (fun law -> (field law, P.Pat.var law.l_field)) laws)
      in
      let record value =
        P.Exp.record (List.map (fun law -> (field law, value law)) laws)
      in
      let f = variable laws "f" in
      [
        P.Str.value "names" (record (fun law -> P.Exp.string law.l_field));
        P.Str.value "locations" (record (fun law -> location law.l_loc));
        value "map"
          [ param fields; labelled_param "f" (P.Pat.var f) ]
          (record (fun law ->
               P.Exp.apply (P.Exp.var f) [ (Nolabel, P.Exp.var law.l_field) ]));
        value "to_list"
          [ param fields ]
          (P.Exp.list (List.map (fun law -> P.Exp.var law.l_field) laws));
      ]

let param_type_variables law =
  Typemod.law_type_variables
    (List.map
       (fun (p : Typedtree.law_param) -> p.lp_type.ctyp_type)
       law.l_params)

let newtype_jkind law (t : Typedtree.law_newtype) =
  printable_jkind ~loc:law.l_loc t.ln_name.txt t.ln_type

(** The input type of a law [l (type a ...) (x1 : t1) ... (xn : tn)], which
    quantifies the types of the law and the type variables of the annotations
    with their kinds:
    {[
      type l_input =
        | L_input : ('a : k) ... . { x1 : t1; ...; xn : tn } -> l_input
    ]} *)
let input_declaration print law =
  let tyvars = param_type_variables law in
  let used = ref (String.Set.of_list (List.map fst tyvars)) in
  let newtypes =
    List.map
      (fun (t : Typedtree.law_newtype) -> (t, fresh used t.ln_name.txt))
      law.l_newtypes
  in
  let vars =
    List.map (fun (t, name) -> (name, newtype_jkind law t)) newtypes
    @ List.map
        (fun (name, ty) -> (name, printable_jkind ~loc:law.l_loc name ty))
        tyvars
  in
  let newtypes =
    List.map
      (fun ((t : Typedtree.law_newtype), name) -> (t.ln_id, name))
      newtypes
  in
  let args =
    List.map
      (fun (p : Typedtree.law_param) ->
        (Ident.name p.lp_id, core_type ~newtypes (law_print print law) p.lp_type))
      law.l_params
  in
  P.Type.variant (input_type law)
    [
      P.Type.constructor ~vars ~args
        ~res:(tconstr (input_type law) [])
        (constructor law);
    ]

(* Choices *)

let choice_type names ty = P.Typ.constr (ldot names.choice "t") [ ty ]

(** The parameters of a scope, as written:
    {[
      module X1 : S1
      ...
    ]} *)
let frame_parameters print frame =
  List.map
    (fun (id, mty) ->
      P.Sig.module_ (Ident.name id)
        (module_type (fun p -> print (reroot frame.f_paths p)) mty))
    frame.f_params

(** The choice module type of a law [l] in a scope [F], with the parameters of
    [F] and the input type of [l] and its choice; or, if [l] is in a scope [G]
    in [F], with the parameters of [F] and the choice module type of [G] for [l]
    and its choice:
    {[
      module type F_l_choices = sig
        module X1 : S1
        ...
        module type G_choices = sig
          module Y1 : T1
          ...
          type l_input = ...
          val l : l_input Choice.t
        end
        val g : (module G_choices) Choice.t
      end
    ]}
    A law at the top has none: its choice is the one of its input. *)
let law_module_type names law =
  let rec scope frame frames =
    let inner =
      match frames with
      | [] ->
          [
            P.Sig.type_ (input_declaration names.print law);
            P.Sig.value law.l_local
              (choice_type names (tconstr (input_type law) []));
          ]
      | next :: frames ->
          let name = module_type_name next in
          [
            P.Sig.modtype name (scope next frames);
            P.Sig.value next.f_local
              (choice_type names (P.Typ.package (lid name)));
          ]
    in
    P.Mty.signature (frame_parameters names.print frame @ inner)
  in
  match law.l_frames with
  | [] -> None
  | frame :: frames -> Some (law_module_type_name law, scope frame frames)

(* Law bindings *)

(* The names in the binding of a law, in a scope: the choices [M] of the
   scope, which qualify the input type of the law, the choice of the input,
   and the input. *)
type law_names = {
  qualifier : string option;
  source : Longident.t;
  input : string;
}

let qualified n name =
  match n.qualifier with None -> lid name | Some m -> ldot m name

let law_constructor name arg = P.Exp.construct (ldot "Law" name) (Some arg)

(* The boolean expression that tests a clause. [=:=] is tested with
   [Stdlib.( = )], and [&&] of clauses with [Stdlib.( && )]. *)

(** A clause of a law [l] in a scope with choices [M], an [Assumption] or an
    [Assertion] (at the top, the [source] is the [choice] of the law):
    {[
      Law.Assertion
        { value = input; source = M.l;
          predicate = (fun (M.L_input { x1; ...; xn } : M.l_input) ->
            (clause : bool));
          location = { CamlinternalLaw.file = ...; ... } }
    ]}
    The types [(type a)] of a law are bound for the clause, which may refer to
    them, by a function of the parameters, whose other type variables are the
    existential types of the input:
    {[
      (fun (type (a : k)) (x1 : t1) ... (xn : tn) -> (clause : bool)) x1 ... xn
    ]} *)
let stdlib_path name =
  Path.Pdot (Pident (Ident.create_persistent "Stdlib"), name)

let rec clause_expression print ((clause, _) : test_clause) =
  let operator name a b =
    P.Exp.apply
      (P.Exp.ident (print (stdlib_path name)))
      [ (Nolabel, a); (Nolabel, b) ]
  in
  match clause with
  | Test_bool e -> expression print e
  | Test_equal (e1, e2) ->
      operator "=" (expression print e1) (expression print e2)
  | Test_and (c1, c2) ->
      operator "&&" (clause_expression print c1) (clause_expression print c2)

let clause_check ~check print law n ((_, clause_loc) as clause : test_clause) =
  let fields =
    match law.l_params with
    | [] -> None
    | params ->
        let field (p : Typedtree.law_param) =
          let x = Ident.name p.lp_id in
          (lid x, P.Pat.var x)
        in
        Some (P.Pat.record (List.map field params))
  in
  let pattern =
    P.Pat.constraint_
      (P.Pat.construct (qualified n (constructor law)) fields)
      (P.Typ.constr (qualified n (input_type law)) [])
  in
  let body =
    P.Exp.constraint_
      (clause_expression (clause_print print law) clause)
      (tconstr "bool" [])
  in
  let newtypes =
    List.map
      (fun (t : Typedtree.law_newtype) -> (t.ln_name.txt, newtype_jkind law t))
      law.l_newtypes
  in
  let typed (p : Typedtree.law_param) =
    param
      (P.Pat.constraint_
         (P.Pat.var (Ident.name p.lp_id))
         (core_type ~wildcards:true (clause_print print law) p.lp_type))
  in
  let argument (p : Typedtree.law_param) =
    (Asttypes.Nolabel, P.Exp.var (Ident.name p.lp_id))
  in
  let body =
    match (newtypes, law.l_params) with
    | [], _ -> body
    | newtypes, [] -> P.Exp.fun_ ~newtypes [] body
    | newtypes, params ->
        P.Exp.apply
          (P.Exp.fun_ ~newtypes (List.map typed params) body)
          (List.map argument params)
  in
  let predicate = P.Exp.fun_ [ param pattern ] body in
  law_constructor check
    (P.Exp.record
       [
         (lid "value", P.Exp.var n.input);
         (lid "source", P.Exp.ident n.source);
         (lid "predicate", predicate);
         (lid "location", location clause_loc);
       ])

(** The assumptions of a law, then its conclusion:
    {[
      Law.Bind (Law.Assumption { ... }, fun () -> ...
        Law.Assertion { ... })
    ]} *)
let clauses print law n =
  let rec bind = function
    | [] -> clause_check ~check:"Assertion" print law n law.l_conclusion
    | assumption :: rest ->
        law_constructor "Bind"
          (P.Exp.tuple
             [
               clause_check ~check:"Assumption" print law n assumption;
               P.Exp.fun_ [ param P.Pat.unit ] (bind rest);
             ])
  in
  bind law.l_assumptions

(** The binding of a law [l], a function of its choice. At the top, it chooses
    the input of the law:
    {[
    let l (choice : l_input Choice.t) : unit Law.t =
      Law.Bind (Law.Choose choice, fun input -> clauses)
    ]}
    In a scope, it chooses the scopes of the law, applies their functors, and
    then chooses the input of the law:
    {[
      let f_l (choice : (module F_l_choices) Choice.t) : unit Law.t =
        Law.Bind (Law.Choose choice, fun ((module M) : (module F_l_choices)) ->
          let module I = Laws_gen_U.F (M.X1) ... (M.Xn) in
          ...
          Law.Bind (Law.Choose M.l, fun input -> clauses))
    ]} *)
let law_binding names law =
  let used =
    ref
      (String.Set.of_list
         (List.map
            (fun (p : Typedtree.law_param) -> Ident.name p.lp_id)
            law.l_params))
  in
  let input = fresh used "input" in
  let choice = fresh used "choice" in
  let bind choice k =
    law_constructor "Bind"
      (P.Exp.tuple [ law_constructor "Choose" (P.Exp.ident choice); k ])
  in
  let local = function [] -> law.l_local | frame :: _ -> frame.f_local in
  let rec scopes ~choice ~qualifier = function
    | [] ->
        bind choice
          (P.Exp.fun_
             [ param (P.Pat.var input) ]
             (clauses names.print law { qualifier; source = choice; input }))
    | frame :: frames ->
        let m = Ident.name frame.f_choices in
        let instance =
          List.fold_left
            (fun f (id, _) ->
              P.Mod.apply f (P.Mod.ident (ldot m (Ident.name id))))
            (P.Mod.ident (names.print (in_scope law.l_frames frame.f_functor)))
            frame.f_params
        in
        let module_type =
          match qualifier with
          | None -> lid (law_module_type_name law)
          | Some q -> ldot q (module_type_name frame)
        in
        bind choice
          (P.Exp.fun_
             [ param (P.Pat.unpack m module_type) ]
             (P.Exp.letmodule
                (Ident.name frame.f_instance)
                instance
                (scopes
                   ~choice:(ldot m (local frames))
                   ~qualifier:(Some m) frames)))
  in
  let chosen =
    match law.l_frames with
    | [] -> tconstr (input_type law) []
    | _ :: _ -> P.Typ.package (lid (law_module_type_name law))
  in
  let parameter =
    P.Pat.constraint_ (P.Pat.var choice) (choice_type names chosen)
  in
  P.Str.value law.l_field
    (P.Exp.fun_
       ~return:(P.Typ.constr (ldot "Law" "t") [ tconstr "unit" [] ])
       [ param parameter ]
       (scopes ~choice:(lid choice) ~qualifier:None law.l_frames))

(** A law, after its choice module type if it is in a scope *)
let law_items names law =
  (match law_module_type names law with
    | None -> []
    | Some (name, mty) -> [ P.Str.modtype name mty ])
  @ [ law_binding names law ]

(* Aliases *)

(* The global modules and the unit that the generated file refers to. The
   file is built once with a [print] that records them. *)
let global_roots ~unit items =
  let roots = ref Ident.Set.empty in
  let print p =
    List.iter
      (fun id ->
        if Ident.is_global id || Ident.same id unit then
          roots := Ident.Set.add id !roots)
      (Path.heads p);
    lident p
  in
  List.iter
    (fun law ->
      ignore (input_declaration print law);
      List.iter
        (fun clause ->
          ignore (clause_expression (clause_print print law) clause))
        (law.l_conclusion :: law.l_assumptions);
      List.iter
        (fun frame -> ignore (print (in_scope law.l_frames frame.f_functor)))
        law.l_frames)
    (laws_of_items items);
  List.iter
    (fun frame -> ignore (frame_parameters print frame))
    (frames_of_items items);
  Ident.Set.elements !roots

(* The aliases of the roots are fresh among the roots and the parameters
   of functors, which are fields of the choice module types. *)
let choose_names ~unit items =
  let params =
    List.concat_map
      (fun frame -> List.map (fun (id, _) -> Ident.name id) frame.f_params)
      (frames_of_items items)
  in
  let roots = global_roots ~unit items in
  let used = ref (String.Set.of_list (List.map Ident.name roots @ params)) in
  let aliases =
    List.map
      (fun id ->
        let alias = fresh used ("Laws_gen_" ^ Ident.name id) in
        (id, Ident.create_persistent alias))
      roots
  in
  let alias_paths =
    List.fold_left
      (fun paths (id, alias) -> Ident.Map.add id (Path.Pident alias) paths)
      Ident.Map.empty aliases
  in
  let print p = lident (reroot alias_paths p) in
  (* The fields for the parameters of functors in the choice module types
     must not shadow the parameter of [Instantiate]. *)
  let choice = fresh (ref (String.Set.of_list params)) "Choice" in
  { aliases; choice; print }

(** {[
      module Laws_gen_U = U
      ...
    ]} *)
let alias_bindings names =
  List.map
    (fun (root, alias) ->
      P.Str.module_ (Ident.name alias) (P.Mod.ident (lid (Ident.name root))))
    names.aliases

(* The file *)

let top_laws items =
  List.filter_map (function Law law -> Some law | Scope _ -> None) items

(* The laws file, of the shape above *)
let implementation names items =
  let laws = laws_of_items items in
  let law_module =
    P.Str.module_ "Law"
      (P.Mod.apply
         (P.Mod.ident (ldot "CamlinternalLaw" "Make"))
         (P.Mod.ident (lid names.choice)))
  in
  let instantiate =
    P.Mod.functor_ names.choice
      (P.Mty.ident (ldot "CamlinternalLaw" "Choice"))
      (P.Mod.structure (law_module :: List.concat_map (law_items names) laws))
  in
  (* Unused parameters of laws in clauses, and constructors and labels that
     their type annotation selects (see [annotation]) *)
  (P.Str.warning "-27-40" :: alias_bindings names)
  @ List.map
      (fun (law, reason) ->
        P.Str.text (Printf.sprintf " The law %s is not tested: %s. " law reason))
      (List.rev !untested)
  @ (P.Str.type_ (each laws) :: each_values laws)
  @ List.map
      (fun law -> P.Str.type_ (input_declaration names.print law))
      (top_laws items)
  @ [ P.Str.module_ "Instantiate" instantiate ]

let generate ~unit (impl : Typedtree.implementation) =
  let str = impl.structure in
  (* The unit is a local ident, bound to its interface in the environment
     of [check_exported]; the generated file refers to it through an alias
     with its name. *)
  let unit =
    Ident.create_local
      (Compilation_unit.Name.to_string (Compilation_unit.name unit))
  in
  let exported =
    Env.add_module unit Mp_present (Mty_signature impl.signature)
      str.str_final_env
  in
  let ctx =
    {
      exported;
      paths = Ident.Map.empty;
      qualifier = [];
      relative = [];
      display = [];
      frames = [];
    }
  in
  untested := [];
  let items = walk_structure ctx ~path:(Path.Pident unit) str in
  check_names (laws_of_items items);
  let names = choose_names ~unit items in
  Laws_file.of_syntax (implementation names items)
