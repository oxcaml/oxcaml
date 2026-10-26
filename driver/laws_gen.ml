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
   [ln] are the laws of [U], qualified by their modules:
   {[
     [@@@ocaml.warning "-27"]
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
       module type Choices = sig
         val l1 : l1_input Choice.t
         ...
       end
       let l1 ((module C) : (module Choices)) : unit Law.t = ...
       ...
       let laws choices = { l1 = l1 choices; ...; ln = ln choices }
     end
   ]}
   The unit, and every other unit that the laws refer to, is referred to
   through an alias. *)

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
      (Asttypes.arg_label * pattern) list ->
      expression ->
      expression
    (** [fun_ ~return params body] is [fun params : return -> body]. *)

    val constraint_ : expression -> core_type -> expression
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
  end

  module Str : sig
    val value : string -> expression -> structure_item
    val type_ : type_declaration -> structure_item
    val module_ : string -> module_expr -> structure_item
    val modtype : string -> module_type -> structure_item

    val warning : string -> structure_item
    (** [warning spec] is [[@@@ocaml.warning spec]]. *)
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

    let fun_ ?return params body =
      let params =
        List.map
          (fun (label, pat) ->
            {
              pparam_desc = Pparam_val (label, None, pat);
              pparam_loc = Location.none;
            })
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
  end
end

(* Laws *)

(* The paths that the idents of a context stand for *)
type paths = Path.t Ident.Map.t

let rec reroot (paths : paths) (p : Path.t) : Path.t =
  match p with
  | Pident id -> Option.value (Ident.Map.find_opt id paths) ~default:p
  | Pdot (q, s) -> Pdot (reroot paths q, s)
  | Papply (f, arg) -> Papply (reroot paths f, reroot paths arg)
  | Pextra_ty (q, extra) -> Pextra_ty (reroot paths q, extra)

type law = {
  (* The field of [each], qualified by the modules of the law. *)
  l_field : string;
  (* The path of the law, for messages. *)
  l_display : string;
  l_loc : Location.t;
  l_params : Typedtree.law_param list;
  l_assumptions : Typedtree.expression list;
  l_conclusion : Typedtree.expression;
  (* The paths of the idents of the law, in types *)
  l_paths : paths;
}

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
   are written. *)

(* The walk of a unit. [path] below is the path of the current module. *)
type context = {
  (* The unit bound to its interface, to check that the laws refer to
     exported items only. *)
  exported : Env.t;
  paths : paths;
  (* The module path from the top, for the fields of [each]. *)
  qualifier : string list;
  (* The module path, for messages. *)
  display : string list;
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
    (fun ((Value : Spec.namespace), p) -> check "value" Env.find_value p)
    (List.concat_map Spec.paths (ld.law_conclusion :: ld.law_assumptions));
  with_type_mark (fun mark ->
      let it =
        {
          (Btype.type_iterators mark) with
          it_path = check "type" Env.find_type;
        }
      in
      List.iter (fun (_, ty) -> it.it_type_expr it ty) ld.law_params)

let law ctx (ld : Typedtree.law_declaration) =
  let name = Ident.name ld.law_id in
  let display = String.concat "." (ctx.display @ [ name ]) in
  check_exported ctx ~loc:ld.law_loc display ld.law_law;
  {
    l_field = qualify ctx.qualifier name;
    l_display = display;
    l_loc = ld.law_loc;
    l_params = ld.law_params;
    l_assumptions = List.map Translspec.typedtree ld.law_assumptions;
    l_conclusion = Translspec.typedtree ld.law_conclusion;
    l_paths = ctx.paths;
  }

(* The first law of a module expression *)
let first_law (me : Typedtree.module_expr) =
  let exception Found of Location.t in
  let structure_item it (item : Typedtree.structure_item) =
    match item.str_desc with
    | Tstr_law ld -> raise (Found ld.law_loc)
    | _ -> Tast_iterator.default_iterator.structure_item it item
  in
  let it = { Tast_iterator.default_iterator with structure_item } in
  match it.module_expr it me with () -> None | exception Found loc -> Some loc

let rec walk_structure ctx ~path (str : Typedtree.structure) =
  let ctx = { ctx with paths = add_items ctx.paths path str.str_type } in
  List.concat_map (walk_structure_item ctx ~path) str.str_items

and walk_structure_item ctx ~path (item : Typedtree.structure_item) =
  match item.str_desc with
  | Tstr_law ld -> [ law ctx ld ]
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
  | Tmod_functor _ -> (
      match first_law me with
      | None -> []
      | Some loc ->
          error ~loc "Laws in functors are not supported in the laws file yet.")
  | Tmod_ident _ | Tmod_apply _ | Tmod_apply_unit _ | Tmod_unpack _ -> []

(* Names *)

let input_type law = law.l_field ^ "_input"

(* The constructor of the input type of a law. It does not capture the
   predefined constructors, which clauses refer to without
   qualification. *)
let constructor law = String.capitalize_ascii (input_type law)

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

(* [t#] is the constructor ["t#"], see [Parser.unboxed_type]. *)
let lident : Path.t -> Longident.t = function
  | Pextra_ty (Pident id, Punboxed_ty) -> Lident (Ident.name id ^ "#")
  | Pextra_ty (Pdot (q, name), Punboxed_ty) ->
      Untypeast.lident_of_path (Pdot (q, name ^ "#"))
  | p -> Untypeast.lident_of_path p

(* The syntax of typed trees, with the paths written by [print] *)

let mapper print =
  {
    Untypeast.default_mapper with
    ident = (fun _ path lid -> { lid with txt = print path });
  }

let core_type print cty =
  let m = mapper print in
  m.typ m cty

let expression print e =
  let m = mapper print in
  m.expr m e

let law_print print law p = print (reroot law.l_paths p)

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

(** The input type of a law [l (x1 : t1) ... (xn : tn)], which quantifies the
    type variables of the annotations with their kinds:
    {[
      type l_input =
        | L_input : ('a : k) ... . { x1 : t1; ...; xn : tn } -> l_input
    ]} *)
let input_declaration print law =
  let vars =
    List.map
      (fun (name, ty) -> (name, printable_jkind ~loc:law.l_loc name ty))
      (Typemod.law_type_variables
         (List.map
            (fun (p : Typedtree.law_param) -> p.lp_type.ctyp_type)
            law.l_params))
  in
  let args =
    List.map
      (fun (p : Typedtree.law_param) ->
        (Ident.name p.lp_id, core_type (law_print print law) p.lp_type))
      law.l_params
  in
  P.Type.variant (input_type law)
    [
      P.Type.constructor ~vars ~args
        ~res:(tconstr (input_type law) [])
        (constructor law);
    ]

(* Choices *)

(** The choices of the laws:
    {[
      module type Choices = sig
        val l1 : l1_input Choice.t
        ...
      end
    ]} *)
let choices laws =
  P.Mty.signature
    (List.map
       (fun law ->
         P.Sig.value law.l_field
           (P.Typ.constr (ldot "Choice" "t") [ tconstr (input_type law) [] ]))
       laws)

(* Law bindings *)

let law_constructor name arg = P.Exp.construct (ldot "Law" name) (Some arg)

(** A clause of a law [l], an [Assumption] or an [Assertion]:
    {[
      Law.Assertion
        { value = input; source = C.l;
          predicate = (fun (L_input { x1; ...; xn } : l_input) ->
            (clause : bool));
          location = { CamlinternalLaw.file = ...; ... } }
    ]} *)
let clause_check ~check print law ~input (clause : Typedtree.expression) =
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
      (P.Pat.construct (lid (constructor law)) fields)
      (tconstr (input_type law) [])
  in
  let body = expression (law_print print law) clause in
  let predicate =
    P.Exp.fun_ [ param pattern ] (P.Exp.constraint_ body (tconstr "bool" []))
  in
  law_constructor check
    (P.Exp.record
       [
         (lid "value", P.Exp.var input);
         (lid "source", P.Exp.ident (ldot "C" law.l_field));
         (lid "predicate", predicate);
         (lid "location", location clause.exp_loc);
       ])

(** The assumptions of a law, then its conclusion:
    {[
      Law.Bind (Law.Assumption { ... }, fun () -> ...
        Law.Assertion { ... })
    ]} *)
let clauses print law ~input =
  let rec bind = function
    | [] -> clause_check ~check:"Assertion" print law ~input law.l_conclusion
    | assumption :: rest ->
        law_constructor "Bind"
          (P.Exp.tuple
             [
               clause_check ~check:"Assumption" print law ~input assumption;
               P.Exp.fun_ [ param P.Pat.unit ] (bind rest);
             ])
  in
  bind law.l_assumptions

(** The binding of a law [l], which chooses its input:
    {[
    let l ((module C) : (module Choices)) : unit Law.t =
      Law.Bind (Law.Choose C.l, fun input -> clauses)
    ]} *)
let law_binding print law =
  let input =
    fresh
      (ref
         (String.Set.of_list
            (List.map
               (fun (p : Typedtree.law_param) -> Ident.name p.lp_id)
               law.l_params)))
      "input"
  in
  let bind choices name k =
    law_constructor "Bind"
      (P.Exp.tuple
         [ law_constructor "Choose" (P.Exp.ident (ldot choices name)); k ])
  in
  P.Str.value law.l_field
    (P.Exp.fun_
       ~return:(P.Typ.constr (ldot "Law" "t") [ tconstr "unit" [] ])
       [ param (P.Pat.unpack "C" (lid "Choices")) ]
       (bind "C" law.l_field
          (P.Exp.fun_ [ param (P.Pat.var input) ] (clauses print law ~input))))

let laws_type =
  tconstr "each" [ P.Typ.constr (ldot "Law" "t") [ tconstr "unit" [] ] ]

(** The laws for the choices of a backend:
    {[
      let laws choices = { l1 = l1 choices; ...; ln = ln choices }
    ]}
    and [let laws (_ : (module Choices)) : unit Law.t each = ()] without laws.
*)
let laws_binding laws =
  match laws with
  | [] ->
      let choices = P.Typ.package (lid "Choices") in
      P.Str.value "laws"
        (P.Exp.fun_ ~return:laws_type
           [ param (P.Pat.constraint_ P.Pat.any choices) ]
           P.Exp.unit)
  | laws ->
      let choices = variable laws "choices" in
      P.Str.value "laws"
        (P.Exp.fun_
           [ param (P.Pat.var choices) ]
           (P.Exp.record
              (List.map
                 (fun law ->
                   ( lid law.l_field,
                     P.Exp.apply (P.Exp.var law.l_field)
                       [ (Nolabel, P.Exp.var choices) ] ))
                 laws)))

(* Aliases *)

(* The global modules and the unit that the generated file refers to. The
   file is built once with a [print] that records them. *)
let global_roots ~unit laws =
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
        (fun clause -> ignore (expression (law_print print law) clause))
        (law.l_conclusion :: law.l_assumptions))
    laws;
  Ident.Set.elements !roots

(* The aliases of the roots are fresh among the roots. *)
let choose_names ~unit laws =
  let roots = global_roots ~unit laws in
  let used = ref (String.Set.of_list (List.map Ident.name roots)) in
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
  { aliases; print }

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

(* The laws file, of the shape above *)
let implementation names laws =
  let law_module =
    P.Str.module_ "Law"
      (P.Mod.apply
         (P.Mod.ident (ldot "CamlinternalLaw" "Make"))
         (P.Mod.ident (lid "Choice")))
  in
  let instantiate =
    P.Mod.functor_ "Choice"
      (P.Mty.ident (ldot "CamlinternalLaw" "Choice"))
      (P.Mod.structure
         ((law_module :: [ P.Str.modtype "Choices" (choices laws) ])
         @ List.map (law_binding names.print) laws
         @ [ laws_binding laws ]))
  in
  (* Unused parameters of laws in clauses *)
  (P.Str.warning "-27" :: alias_bindings names)
  @ (P.Str.type_ (each laws) :: each_values laws)
  @ List.map (fun law -> P.Str.type_ (input_declaration names.print law)) laws
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
    { exported; paths = Ident.Map.empty; qualifier = []; display = [] }
  in
  let laws = walk_structure ctx ~path:(Path.Pident unit) str in
  check_names laws;
  let names = choose_names ~unit laws in
  Laws_file.of_syntax (implementation names laws)
