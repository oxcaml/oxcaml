(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Jane Street Group LLC                              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types
open Typedtree
module String = Misc.Stdlib.String

exception Unsupported of Location.t * string

let unsupported ~loc fmt =
  Format.kasprintf (fun msg -> raise (Unsupported (loc, msg))) fmt

(* Rocq syntax *)

(* Expressions of λ_dyn, written with the notations of [iris.heap_lang] and
   the smart constructors of [oxiris.lang.encoding]. HeapLang variables are
   strings: the names of OxCaml variables, and names starting with [$] for
   the variables of the translation. *)

type binder = Anon | Named of string

type expr =
  | Var of string
  | Const of string * bool
      (** A Rocq constant or variable, and whether it is a value (rather than an
          expression) *)
  | Int of int
  | Bool of bool
  | Unit
  | Stuck  (** An expression without a value: a raised exception *)
  | Pair of expr * expr
  | Tag of int * expr
  | Nil
  | Cons of expr * expr
  | None_
  | Some_ of expr
  | Fst of expr
  | Snd of expr
  | App of expr * expr list
  | Lam of binder list * expr
  | Rec of string * binder list * expr
  | Let of binder * expr * expr
  | If of expr * expr * expr
  | And_also of expr * expr
  | Or_else of expr * expr
  | Unop of string * expr
  | Binop of string * expr * expr
  | Match_list of expr * expr * binder * binder * expr
  | Match_tag of expr * binder * (int * expr) list * expr
  | While of expr * expr
  | For of binder * Asttypes.direction_flag * expr * expr * expr
      (** [For (i, dir, lo, hi, body)] *)

let rec is_value = function
  | Const (_, is_value) -> is_value
  | Int _ | Bool _ | Unit | Nil | None_ | Lam _ | Rec _ -> true
  | Pair (a, b) | Cons (a, b) -> is_value a && is_value b
  | Tag (_, a) | Some_ a -> is_value a
  | Var _ | Stuck | Fst _ | Snd _ | App _ | Let _ | If _ | And_also _
  | Or_else _ | Unop _ | Binop _ | Match_list _ | Match_tag _ | While _ | For _
    ->
      false

(* Semantic types: [Sty_var "A"] is a type variable and
   [Sty_app ("ty_list", [a])] is [ty_list a]. *)
type sty = Sty_var of string | Sty_app of string * sty list

(* A clause of a law: [spec_true e] or [e1 =:= e2] *)
(* The clauses of a law *)
type clause =
  | Spec_true of expr
  | Spec_eq of expr * expr
  | Spec_and of clause * clause
  | Spec_wp of {
      var : string option;  (** [None] for [()] and [_] *)
      ty : string;  (** The Phase 3 type of the result, printed *)
      mode : string;  (** The Phase 3 mode of the result, printed *)
      e : expr;
      body : clause;
    }

(* The logic of a law: total correctness (Phase 1), weakest preconditions
   (Phase 2) or separation logic (Phase 3), see TARGET.md *)
type phase = Phase1 | Phase2 | Phase3

module Print = struct
  open Format

  let string ppf s = fprintf ppf "\"%s\"" s

  let binder ppf = function
    | Anon -> pp_print_string ppf "<>"
    | Named x -> string ppf x

  (* A binder as the argument of a Rocq function such as [MatchList] *)
  let binder_arg ppf = function
    | Anon -> pp_print_string ppf "BAnon"
    | Named x -> string ppf x

  let binders ppf bs =
    pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf " ") binder ppf bs

  let int ppf n = if n < 0 then fprintf ppf "#(%d)" n else fprintf ppf "#%d" n

  let is_atom = function
    | Var _ | Const _ | Int _ | Bool _ | Unit | Stuck | Pair _ | Nil | None_ ->
        true
    | Tag _ | Cons _ | Some_ _ | Fst _ | Snd _ | App _ | Lam _ | Rec _ | Let _
    | If _ | And_also _ | Or_else _ | Unop _ | Binop _ | Match_list _
    | Match_tag _ | While _ | For _ ->
        false

  (* The operands of operators: atoms and applications *)
  let is_operand = function
    | Tag _ | Cons _ | Some_ _ | Fst _ | Snd _ | App _ | And_also _ | Or_else _
    | Match_list _ | Match_tag _ | While _ | For _ ->
        true
    | e -> is_atom e

  let parens_if b pp ppf x = if b then fprintf ppf "(@[%a@])" pp x else pp ppf x

  let rec expr ppf e =
    match e with
    | Var x -> string ppf x
    | Const (c, _) -> pp_print_string ppf c
    | Int n -> int ppf n
    | Bool b -> fprintf ppf "#%b" b
    | Unit -> pp_print_string ppf "#()"
    | Stuck -> pp_print_string ppf "Stuck"
    | Pair (a, b) -> fprintf ppf "@[<hv 1>(%a,@ %a)@]" component a component b
    | Nil -> pp_print_string ppf "Nil"
    | None_ -> pp_print_string ppf "None'"
    | Tag (i, a) -> call ppf "Tag" [ (fun ppf -> pp_print_int ppf i); arg a ]
    | Cons (a, b) -> call ppf "Cons" [ arg a; arg b ]
    | Some_ a -> call ppf "Some'" [ arg a ]
    | Fst a -> call ppf "Fst" [ arg a ]
    | Snd a -> call ppf "Snd" [ arg a ]
    | And_also (a, b) -> call ppf "AndAlso" [ arg a; arg b ]
    | Or_else (a, b) -> call ppf "OrElse" [ arg a; arg b ]
    | App (f, args) ->
        fprintf ppf "@[<hov 2>%a%a@]"
          (parens_if (not (is_atom f)) expr)
          f
          (fun ppf -> List.iter (fun a -> fprintf ppf "@ %t" (arg a)))
          args
    | Lam (bs, body) -> fprintf ppf "@[<hv 2>λ: %a,@ %a@]" binders bs expr body
    | Rec (f, bs, body) ->
        fprintf ppf "@[<hv 2>rec: %a %a :=@ %a@]" string f binders bs expr body
    | Let (Anon, a, b) -> fprintf ppf "@[<hv>%a ;;@ %a@]" component a expr b
    | Let (Named x, a, b) ->
        fprintf ppf "@[<hv>@[<hv 2>let: %a :=@ %a@ in@]@ %a@]" string x
          component a expr b
    | If (c, a, b) ->
        fprintf ppf "@[<hv>if: %a@ then@;<1 2>%a@ else@;<1 2>%a@]" component c
          component a else_branch b
    | Unop (op, a) ->
        fprintf ppf "%s %a" op (parens_if (not (is_atom a)) expr) a
    | Binop (op, a, b) ->
        fprintf ppf "@[<hov 2>%a %s@ %a@]" operand a op operand b
    | Match_list (e, nil, y, ys, cons) ->
        call ppf "MatchList"
          [
            arg e;
            arg nil;
            (fun ppf -> binder_arg ppf y);
            (fun ppf -> binder_arg ppf ys);
            arg cons;
          ]
    | Match_tag (e, x, arms, default) ->
        let arm ppf (i, e) =
          fprintf ppf "@[<hov 2>@@pair Z expr %d@ %t@]" i (arg e)
        in
        call ppf "MatchTag"
          [
            arg e;
            (fun ppf -> binder_arg ppf x);
            (fun ppf ->
              fprintf ppf "@[<hv 1>[%a]@]"
                (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";@ ") arm)
                arms);
            arg default;
          ]
    | While (c, body) -> call ppf "While" [ arg c; arg body ]
    | For (i, dir, lo, hi, body) ->
        call ppf
          (match dir with Upto -> "For" | Downto -> "ForDown")
          [ (fun ppf -> binder_arg ppf i); arg lo; arg hi; arg body ]

  and arg e ppf = parens_if (not (is_atom e)) expr ppf e
  and operand ppf e = parens_if (not (is_operand e)) expr ppf e

  and component ppf e =
    match e with
    | Lam _ | Rec _ | Let _ | If _ -> parens_if true expr ppf e
    | _ -> expr ppf e

  and else_branch ppf e =
    match e with If _ -> expr ppf e | _ -> component ppf e

  and call ppf name args =
    fprintf ppf "@[<hov 2>%s%a@]" name
      (fun ppf -> List.iter (fun a -> fprintf ppf "@ %t" a))
      args

  (* [e], a value, in [val_scope] *)
  let rec value ppf e =
    match e with
    | Const (c, _) -> pp_print_string ppf c
    | Int n -> int ppf n
    | Bool b -> fprintf ppf "#%b" b
    | Unit -> pp_print_string ppf "#()"
    | Pair (a, b) -> fprintf ppf "@[<hv 1>(%a,@ %a)@]" value a value b
    | Nil -> pp_print_string ppf "NILV"
    | None_ -> pp_print_string ppf "NONEV'"
    | Tag (i, a) ->
        call ppf "TAGV" [ (fun ppf -> pp_print_int ppf i); value_arg a ]
    | Cons (a, b) -> call ppf "CONSV" [ value_arg a; value_arg b ]
    | Some_ a -> call ppf "SOMEV'" [ value_arg a ]
    | Lam _ | Rec _ -> expr ppf e
    | Var _ | Stuck | Fst _ | Snd _ | App _ | Let _ | If _ | And_also _
    | Or_else _ | Unop _ | Binop _ | Match_list _ | Match_tag _ | While _
    | For _ ->
        Misc.fatal_error "Laws_rocq.Print.value: not a value"

  and value_arg e ppf =
    match e with
    | Tag _ | Cons _ | Some_ _ | Lam _ | Rec _ -> fprintf ppf "(%a)" value e
    | _ -> value ppf e

  let rec sty ppf = function
    | Sty_var a -> pp_print_string ppf a
    | Sty_app (c, []) -> pp_print_string ppf c
    | Sty_app (c, args) ->
        fprintf ppf "@[<hov 2>%s%a@]" c
          (fun ppf -> List.iter (fun a -> fprintf ppf "@ %a" sty_arg a))
          args

  and sty_arg ppf = function
    | Sty_app (_, _ :: _) as t -> fprintf ppf "(%a)" sty t
    | t -> sty ppf t

  (* [ty v], the proposition that [v] has the semantic type [ty] *)
  let typing ppf (ty, v) = fprintf ppf "@[<hov 2>%a@ %s@]" sty ty v

  let rec conjuncts = function
    | Spec_and (c1, c2) -> conjuncts c1 @ conjuncts c2
    | c -> [ c ]

  let conjunction pp ppf c =
    match conjuncts c with
    | [ c ] -> pp ppf c
    | cs ->
        fprintf ppf "@[<hov>%a@]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> fprintf ppf " ∧@ ")
             (fun ppf c -> fprintf ppf "(%a)" pp c))
          cs

  let e ppf x = fprintf ppf "(%a)%%E" expr x

  (* Phase 1: Coq propositions *)
  let rec clause1 ppf = function
    | Spec_true x -> fprintf ppf "@[<hov 2>spec_true@ %a@]" e x
    | Spec_eq (a, b) -> fprintf ppf "@[<hov 2>%a =:=@ %a@]" e a e b
    | Spec_and _ as c -> conjunction clause1 ppf c
    | Spec_wp _ -> Misc.fatal_error "Laws_rocq.Print.clause1"

  (* Phase 2: siProp propositions *)
  let rec clause2 ppf = function
    | Spec_true x -> fprintf ppf "@[<hov 2>law_true@ %a@]" e x
    | Spec_eq (a, b) -> fprintf ppf "@[<hov 2>law_eq@ %a@ %a@]" e a e b
    | Spec_and _ as c -> conjunction clause2 ppf c
    | Spec_wp { var; e = x; body; _ } ->
        fprintf ppf "@[<v>@[<hov 2>let%%wp %s :=@ %a in@]@,%a@]"
          (Option.value var ~default:"_")
          e x clause2 body

  (* Phase 3: the body of [slaw_valid]. A premise and a conclusion are
     written differently. *)
  let rec premise3 ppf = function
    | Spec_true x -> fprintf ppf "@[<hov 2>spec_true3@ %a@]" e x
    | Spec_eq (a, b) -> fprintf ppf "@[<hov 2>(%a =:=@ %a)@]" e a e b
    | Spec_and _ as c -> conjunction premise3 ppf c
    | Spec_wp _ as c -> fprintf ppf "(%a)" clause3 c

  and clause3 ppf = function
    | Spec_true x -> fprintf ppf "@[<hov 2>⟦ %a ⟧@]" e x
    | Spec_eq (a, b) -> fprintf ppf "@[<hov 2>%a =:=@ %a@]" e a e b
    | Spec_and _ as c -> conjunction clause3 ppf c
    | Spec_wp { var; ty; mode; e = x; body } ->
        fprintf ppf "@[<v>@[<hov 2>let%%wp %s : %s @@ %s :=@ %a in@]@,%a@]"
          (Option.value var ~default:"_")
          ty mode e x clause3 body
end

(* The translation *)

type value_info =
  | Translated of { rocq : string; is_value : bool; stateful : bool }
      (** [stateful]: the definition allocates or uses mutable state, or refers
          to a definition that does *)
  | Untranslated of { name : string; reason : string }

type type_info = Sem of string | No_sem of { name : string; reason : string }

(* The items of a module of the unit, by name *)
type module_info = {
  m_values : (string, value_info) Hashtbl.t;
  m_types : (string, type_info) Hashtbl.t;
  m_modules : (string, module_info) Hashtbl.t;
}

let new_module () =
  {
    m_values = Hashtbl.create 16;
    m_types = Hashtbl.create 16;
    m_modules = Hashtbl.create 16;
  }

type state = {
  mutable values : value_info Ident.Map.t;
  mutable types : type_info Ident.Map.t;
  mutable modules : module_info Ident.Map.t;
  (* The Rocq names defined by the file *)
  mutable used : String.Set.t;
  (* The other units whose files the file imports *)
  mutable requires : String.Set.t;
  mutable uses_stuck : bool;
  (* Whether the definition being translated is stateful *)
  mutable stateful : bool;
  (* Whether the file has a stateful definition, which makes the Phase 2
     laws Phase 3 laws *)
  mutable uses_state : bool;
  (* The largest phase of the laws, which selects the prelude *)
  mutable max_phase : phase;
  mutable counter : int;
  (* The unit-qualified names of the definitions, and their names *)
  mutable aliases : (string * string) list;
  (* The generated items, last first *)
  mutable items : (Format.formatter -> unit) list;
}

let emit st item = st.items <- item :: st.items

(* Names that the generated file must not define or bind: Rocq keywords and
   the names of the prelude *)
let reserved =
  String.Set.of_list
    [
      "as";
      "at";
      "cofix";
      "else";
      "end";
      "exists";
      "exists2";
      "fix";
      "for";
      "forall";
      "fun";
      "if";
      "IF";
      "in";
      "is";
      "let";
      "match";
      "mod";
      "Prop";
      "return";
      "Set";
      "SProp";
      "struct";
      "then";
      "Type";
      "using";
      "where";
      "with";
      "val";
      "expr";
      "binder";
      "pair";
      "sem_ty";
      "spec_eq";
      "spec_true";
      "ty_int";
      "ty_bool";
      "ty_unit";
      "ty_list";
      "ty_option";
      "ty_prod";
      "ty_fun";
      "list_to_val";
      "match_arms";
      "Stuck";
      "While";
      "For";
      "ForDown";
      "Ref_make";
      "Ref_get";
      "Ref_set";
      "Ref_incr";
      "Ref_decr";
      "Array_length";
      "Array_get";
      "Array_set";
      "Array_unsafe_get";
      "Array_unsafe_set";
      "Array_make";
      "Array_to_list";
    ]

let fresh_name ~avoid base =
  let rec loop i =
    let name = if i = 0 then base else base ^ "_" ^ Int.to_string i in
    if String.Set.mem name reserved || String.Set.mem name avoid then
      loop (i + 1)
    else name
  in
  loop 0

(* A new name defined by the file *)
let define st base =
  let name = fresh_name ~avoid:st.used base in
  st.used <- String.Set.add name st.used;
  name

(* A new HeapLang variable of the translation *)
let fresh st base =
  st.counter <- st.counter + 1;
  "$" ^ base ^ Int.to_string st.counter

let stuck st =
  st.uses_stuck <- true;
  Stuck

(* Paths *)

(* The name of the item [U.M.x] of another unit [U] is [U_M_x], with the
   standard library's [Stdlib.M.x] and [Stdlib__M.x] as [M_x]. *)
let external_name st comps =
  (match comps with
  | unit :: _ :: _ when not (String.starts_with ~prefix:"Stdlib" unit) ->
      st.requires <- String.Set.add unit st.requires
  | _ -> ());
  let comps =
    match comps with
    | "Stdlib" :: (_ :: _ :: _ as rest) -> rest
    | unit :: rest when String.starts_with ~prefix:"Stdlib__" unit ->
        String.sub unit 8 (String.length unit - 8) :: rest
    | comps -> comps
  in
  String.concat "_" comps

type module_ref = Local of module_info | External of string list

let rec resolve_module st ~loc (p : Path.t) =
  match p with
  | Pident id -> (
      match Ident.Map.find_opt id st.modules with
      | Some m -> Local m
      | None ->
          if Ident.is_global id then External [ Ident.name id ]
          else unsupported ~loc "the module %s is not translated" (Path.name p))
  | Pdot (q, s) -> (
      match resolve_module st ~loc q with
      | Local m -> (
          match Hashtbl.find_opt m.m_modules s with
          | Some m -> Local m
          | None ->
              unsupported ~loc "the module %s is not translated" (Path.name p))
      | External comps -> External (comps @ [ s ]))
  | Papply _ | Pextra_ty _ ->
      unsupported ~loc "the module path %s is not supported" (Path.name p)

(* The environment of an expression: the local variables, which stand for
   HeapLang variables or Rocq variables (the parameters of a law), and the
   names of the HeapLang variables in scope. *)
type env = { locals : expr Ident.Map.t; names : String.Set.t }

let empty_env = { locals = Ident.Map.empty; names = String.Set.empty }

let bind env id x =
  {
    locals = Ident.Map.add id (Var x) env.locals;
    names = String.Set.add x env.names;
  }

let value_of_info st ~loc = function
  | Translated { rocq; is_value; stateful } ->
      if stateful then st.stateful <- true;
      Const (rocq, is_value)
  | Untranslated { name; reason } ->
      unsupported ~loc "it refers to %s, which is not translated (%s)" name
        reason

let value_ref st ~loc env (p : Path.t) =
  match p with
  | Pident id -> (
      match Ident.Map.find_opt id env.locals with
      | Some e -> e
      | None -> (
          match Ident.Map.find_opt id st.values with
          | Some info -> value_of_info st ~loc info
          | None -> unsupported ~loc "%s is not translated" (Ident.name id)))
  | Pdot (q, s) -> (
      match resolve_module st ~loc q with
      | Local m -> (
          match Hashtbl.find_opt m.m_values s with
          | Some info -> value_of_info st ~loc info
          | None -> unsupported ~loc "%s is not translated" (Path.name p))
      | External comps ->
          let name = external_name st (comps @ [ s ]) in
          if String.starts_with ~prefix:"Array_" name then st.stateful <- true;
          Const (name, true))
  | Papply _ | Pextra_ty _ ->
      unsupported ~loc "the path %s is not supported" (Path.name p)

let type_of_info ~loc = function
  | Sem name -> name
  | No_sem { name; reason } ->
      unsupported ~loc "the type %s is not translated (%s)" name reason

let type_ref st ~loc (p : Path.t) =
  match p with
  | Pident id -> (
      match Ident.Map.find_opt id st.types with
      | Some info -> Some (type_of_info ~loc info)
      | None -> None)
  | Pdot (q, s) -> (
      match resolve_module st ~loc q with
      | Local m -> Option.map (type_of_info ~loc) (Hashtbl.find_opt m.m_types s)
      | External comps -> Some ("ty_" ^ external_name st (comps @ [ s ])))
  | Papply _ | Pextra_ty _ -> None

(* Types *)

let is_type tenv ty paths =
  match get_desc (Ctype.expand_head tenv ty) with
  | Tconstr (p, _, _) -> List.exists (Path.same p) paths
  | _ -> false

let rec prod = function
  | [] -> Sty_app ("ty_unit", [])
  | [ t ] -> t
  | t :: ts -> Sty_app ("ty_prod", [ t; prod ts ])

(* The semantic type of [ty]. [vars] are the names of the type variables.
   [group] are the types being defined, with their names and parameters,
   which may occur only as a whole argument of a constructor ([direct]). *)
let rec sem_type st ~loc ~tenv ~vars ?(group = []) ?(direct = false) ty =
  let sem ty = sem_type st ~loc ~tenv ~vars ~group ty in
  match get_desc ty with
  | Tvar _ | Tunivar _ -> (
      match List.assoc_opt (get_id ty) vars with
      | Some a -> Sty_var a
      | None -> unsupported ~loc "an unnamed type variable")
  | Tpoly (ty, []) -> sem_type st ~loc ~tenv ~vars ~group ~direct ty
  | Ttuple comps -> prod (List.map (fun (_, ty) -> sem ty) comps)
  | Tarrow (((Optional _ | Position _), _, _), _, _, _) ->
      unsupported ~loc "the type of a function with optional arguments"
  | Tarrow (((Nolabel | Labelled _), _, _), a, b, _) ->
      Sty_app ("ty_fun", [ sem a; sem b ])
  | Tconstr (p, args, _) -> (
      let group_member =
        match p with
        | Pident id ->
            List.find_opt (fun (id', _, _) -> Ident.same id id') group
        | _ -> None
      in
      match group_member with
      | Some (_, name, params) ->
          let same_params =
            List.length args = List.length params
            && List.for_all2 (fun a p -> get_id a = get_id p) args params
          in
          if direct && same_params then
            Sty_app
              (name, List.map (fun p -> sem_type st ~loc ~tenv ~vars p) params)
          else
            unsupported ~loc
              "the type %s occurs in its own definition other than as an \
               argument of a constructor or a field, applied to its parameters"
              (Path.name p)
      | None -> (
          let predef name = function
            | [] -> Sty_app (name, [])
            | _ -> Misc.fatal_error "Laws_rocq.sem_type"
          in
          if Path.same p Predef.path_int then predef "ty_int" args
          else if Path.same p Predef.path_bool then predef "ty_bool" args
          else if Path.same p Predef.path_unit then predef "ty_unit" args
          else if Path.same p Predef.path_list then
            Sty_app ("ty_list", List.map sem args)
          else if Path.same p Predef.path_option then
            Sty_app ("ty_option", List.map sem args)
          else
            match Ctype.try_expand_once_opt tenv ty with
            | ty when match p with Pident _ -> false | _ -> true ->
                (* An abbreviation of another unit or of a module: its
                   expansion *)
                sem_type st ~loc ~tenv ~vars ~group ~direct ty
            | _ | (exception Ctype.Cannot_expand) -> (
                match type_ref st ~loc p with
                | Some name -> Sty_app (name, List.map sem args)
                | None ->
                    unsupported ~loc "the type %s is not supported"
                      (Path.name p))))
  | Tvariant _ -> unsupported ~loc "polymorphic variant types"
  | Tobject _ | Tfield _ | Tnil -> unsupported ~loc "object types"
  | Tpackage _ -> unsupported ~loc "first-class module types"
  | Tpoly _ -> unsupported ~loc "polymorphic types"
  | Tlink _ | Tsubst _ -> Misc.fatal_error "Laws_rocq.sem_type"
  | _ -> unsupported ~loc "this type"

(* The names of type variables: ['a] is [A]. *)
let type_var_names ~avoid tys =
  let acc = ref [] in
  let rec collect ty =
    match get_desc ty with
    | Tvar { name; _ } | Tunivar { name; _ } ->
        if not (List.mem_assoc (get_id ty) !acc) then
          acc := (get_id ty, name) :: !acc
    | _ -> Btype.iter_type_expr collect ignore ty
  in
  List.iter collect tys;
  let ids = List.rev !acc in
  let _, vars =
    List.fold_left_map
      (fun avoid (id, name) ->
        let base =
          match name with Some n -> String.capitalize_ascii n | None -> "T"
        in
        let a = fresh_name ~avoid base in
        (String.Set.add a avoid, (id, a)))
      avoid ids
  in
  vars

(* Expressions *)

let constant ~loc : Typedtree.constant -> expr = function
  | Const_int n -> Int n
  | Const_char c -> Int (Char.code c)
  | _ -> unsupported ~loc "constants other than integers and characters"

let rec tuple = function
  | [] -> Unit
  | [ e ] -> e
  | e :: es -> Pair (e, tuple es)

(* Field [pos] of a tuple or record with [n] components *)
let rec project ~pos ~n e =
  if n <= 1 then e
  else if pos = 0 then Fst e
  else project ~pos:(pos - 1) ~n:(n - 1) (Snd e)

let payload = tuple

let check_label ~loc (lbl : Data_types.label_description) =
  if Types.is_mutable lbl.lbl_mut then
    unsupported ~loc "mutable record fields (other than those of refs)"

(* Whether [ty] is ['a ref] *)
let is_ref tenv ty =
  match get_desc (Ctype.expand_head tenv ty) with
  | Tconstr (p, [ _ ], _) -> String.equal (Path.name p) "Stdlib.ref"
  | _ -> false

(* The tag of a constructor of a variant other than [bool] and [unit] *)
type constructor_kind =
  | Ctor_bool of bool
  | Ctor_unit
  | Ctor_list
  | Ctor_option
  | Ctor_tag of int

let constructor_kind ~loc (cd : Data_types.constructor_description) =
  match cd.cstr_tag with
  | Extension _ -> unsupported ~loc "exceptions and extension constructors"
  | Null -> unsupported ~loc "the constructor Null"
  | Ordinary { src_index; _ } ->
      let p = Data_types.cstr_res_type_path cd in
      if Option.is_some cd.cstr_inlined then unsupported ~loc "inline records"
      else if Path.same p Predef.path_bool then
        Ctor_bool (String.equal cd.cstr_name "true")
      else if Path.same p Predef.path_unit then Ctor_unit
      else if Path.same p Predef.path_list then Ctor_list
      else if Path.same p Predef.path_option then Ctor_option
      else Ctor_tag src_index

let constructor_index (cd : Data_types.constructor_description) =
  match cd.cstr_tag with
  | Ordinary { src_index; _ } -> src_index
  | Extension _ | Null -> Misc.fatal_error "Laws_rocq.constructor_index"

let construct ~loc cd args =
  match (constructor_kind ~loc cd, args) with
  | Ctor_bool b, _ -> Bool b
  | Ctor_unit, _ -> Unit
  | Ctor_list, [] -> Nil
  | Ctor_list, [ a; b ] -> Cons (a, b)
  | Ctor_option, [] -> None_
  | Ctor_option, [ a ] -> Some_ a
  | Ctor_tag i, args -> Tag (i, payload args)
  | (Ctor_list | Ctor_option), _ -> Misc.fatal_error "Laws_rocq.construct"

(* Patterns *)

(* A pattern matched against a value [v] (a variable or a projection of
   one) is a test, [None] when it always succeeds, and the bindings of its
   variables, to evaluate when the test succeeds. *)
type bindings = (Ident.t * string * expr) list

let conj (results : (expr option * bindings) list) =
  let tests = List.filter_map fst results in
  let test =
    match List.rev tests with
    | [] -> None
    | t :: ts -> Some (List.fold_left (fun acc t -> And_also (t, acc)) t ts)
  in
  (test, List.concat_map snd results)

let rec pattern_test (p : pattern) v : expr option * bindings =
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_any -> (None, [])
  | Tpat_var { id; name; _ } -> (None, [ (id, name.txt, v) ])
  | Tpat_alias { pattern; id; name; _ } ->
      let test, bs = pattern_test pattern v in
      (test, (id, name.txt, v) :: bs)
  | Tpat_constant c -> (Some (Binop ("=", v, constant ~loc c)), [])
  | Tpat_tuple ps -> components (List.map (fun (_, p, _) -> p) ps) v
  | Tpat_construct (_, cd, _, args, _) -> (
      let args = List.map snd args in
      match constructor_kind ~loc cd with
      | Ctor_bool true -> (Some v, [])
      | Ctor_bool false -> (Some (Unop ("~", v)), [])
      | Ctor_unit -> (None, [])
      | Ctor_list | Ctor_option | Ctor_tag _ ->
          let tag =
            if cd.cstr_consts + cd.cstr_nonconsts <= 1 then None
            else Some (Binop ("=", Fst v, Int (constructor_index cd)))
          in
          let payload =
            match args with
            | [] -> (None, [])
            | [ a ] -> pattern_test a (Snd v)
            | args -> components args (Snd v)
          in
          conj [ (tag, []); payload ])
  | Tpat_record (fields, _, _) ->
      conj
        (List.map
           (fun (_, (lbl : Data_types.label_description), p) ->
             check_label ~loc lbl;
             pattern_test p
               (project ~pos:lbl.lbl_pos ~n:(Array.length lbl.lbl_all) v))
           fields)
  | Tpat_or (a, b, _) -> (
      match (pattern_test a v, pattern_test b v) with
      | (Some a, []), (Some b, []) -> (Some (Or_else (a, b)), [])
      | (None, []), (_, []) | (_, []), (None, []) -> (None, [])
      | _ -> unsupported ~loc "or-patterns that bind variables")
  | Tpat_variant _ -> unsupported ~loc "polymorphic variants"
  | Tpat_array _ -> unsupported ~loc "arrays"
  | Tpat_lazy _ -> unsupported ~loc "lazy patterns"
  | _ -> unsupported ~loc "this pattern"

and components ps v =
  let n = List.length ps in
  conj (List.mapi (fun pos p -> pattern_test p (project ~pos ~n v)) ps)

let irrefutable p =
  match pattern_test p Unit with
  | None, _ -> true
  | Some _, _ -> false
  | exception Unsupported _ -> false

let bind_all env (bs : bindings) =
  List.fold_left (fun env (id, x, _) -> bind env id x) env bs

let lets (bs : bindings) body =
  List.fold_right (fun (_, x, v) body -> Let (Named x, v, body)) bs body

(* Whether [v] can be matched without binding it first: a constant, or a
   variable that the patterns do not bind *)
let stable v pats =
  match v with
  | Const _ -> true
  | Var x ->
      not
        (List.exists
           (fun p ->
             List.exists
               (fun id -> String.equal (Ident.name id) x)
               (pat_bound_idents p))
           pats)
  | _ -> false

(* Primitives *)

let division st op a b =
  match b with
  | Int n when n <> 0 -> Binop (op, a, b)
  | _ ->
      let d = fresh st "d" and n = fresh st "n" in
      Let
        ( Named d,
          b,
          Let
            ( Named n,
              a,
              If (Binop ("=", Var d, Int 0), stuck st, Binop (op, Var n, Var d))
            ) )

(* The Rocq function that models the primitive [name] on mutable state (see
   [theories/generated/TARGET.md] in oxiris). [arg_ty] is the type of its
   first argument. Out-of-bounds accesses are stuck there, since exceptions
   are not modelled. *)
let state_primitive ~tenv ~arg_ty name =
  match name with
  | "%makemutable" -> Some "Ref_make"
  | "%field0" when is_ref tenv arg_ty -> Some "Ref_get"
  | "%setfield0" when is_ref tenv arg_ty -> Some "Ref_set"
  | "%incr" -> Some "Ref_incr"
  | "%decr" -> Some "Ref_decr"
  | "%array_safe_get" -> Some "Array_get"
  | "%array_safe_set" -> Some "Array_set"
  | "%array_unsafe_get" -> Some "Array_unsafe_get"
  | "%array_unsafe_set" -> Some "Array_unsafe_set"
  | "%array_length" -> Some "Array_length"
  | "caml_array_make" | "caml_make_vect" -> Some "Array_make"
  | _ -> None

(* The translation of the primitive [name] applied to all its arguments.
   [arg_ty] is the type of its first argument. *)
let primitive_op st ~loc ~tenv ~arg_ty name : expr list -> expr =
  let arity () = Misc.fatal_error "Laws_rocq.primitive_op: arity" in
  let un f = function [ a ] -> f a | _ -> arity () in
  let bin f = function [ a; b ] -> f a b | _ -> arity () in
  let comparison types =
    let name =
      match name with
      | "%equal" -> "="
      | "%notequal" -> "<>"
      | "%eq" -> "=="
      | "%noteq" -> "!="
      | "%lessthan" -> "<"
      | "%lessequal" -> "<="
      | "%greaterthan" -> ">"
      | "%greaterequal" -> ">="
      | name -> name
    in
    if not (is_type tenv arg_ty types) then
      unsupported ~loc
        "the comparison %s at a type other than %s (write e1 =:= e2 for the \
         logical equality of two values)"
        name
        (if List.length types > 2 then "int, char, bool and unit"
         else "int and char")
  in
  let equality () =
    comparison Predef.[ path_int; path_char; path_bool; path_unit ]
  in
  let ordering () = comparison Predef.[ path_int; path_char ] in
  match name with
  | "%sequand" -> bin (fun a b -> And_also (a, b))
  | "%sequor" -> bin (fun a b -> Or_else (a, b))
  | "%boolnot" -> un (fun a -> App (Const ("Stdlib_not", true), [ a ]))
  | "%identity" -> un Fun.id
  | ("%field0" | "%field1" | "%field0_immut" | "%field1_immut") as field -> (
      match get_desc (Ctype.expand_head tenv arg_ty) with
      | Ttuple comps ->
          let pos =
            if String.starts_with ~prefix:"%field0" field then 0 else 1
          in
          un (project ~pos ~n:(List.length comps))
      | _ ->
          unsupported ~loc "the primitive %s on a value other than a tuple" name
      )
  | "%ignore" -> un (fun a -> Let (Anon, a, Unit))
  | "%addint" -> bin (fun a b -> Binop ("+", a, b))
  | "%subint" -> bin (fun a b -> Binop ("-", a, b))
  | "%mulint" -> bin (fun a b -> Binop ("*", a, b))
  | "%divint" -> bin (division st "`quot`")
  | "%modint" -> bin (division st "`rem`")
  | "%negint" -> un (fun a -> Unop ("-", a))
  | "%succint" -> un (fun a -> Binop ("+", a, Int 1))
  | "%predint" -> un (fun a -> Binop ("-", a, Int 1))
  | "%equal" | "%eq" ->
      equality ();
      bin (fun a b -> Binop ("=", a, b))
  | "%notequal" | "%noteq" ->
      equality ();
      bin (fun a b -> Unop ("~", Binop ("=", a, b)))
  | "%lessthan" ->
      ordering ();
      bin (fun a b -> Binop ("<", a, b))
  | "%lessequal" ->
      ordering ();
      bin (fun a b -> Binop ("≤", a, b))
  | "%greaterthan" ->
      ordering ();
      bin (fun a b -> Binop ("<", b, a))
  | "%greaterequal" ->
      ordering ();
      bin (fun a b -> Binop ("≤", b, a))
  | "%setfield0" ->
      unsupported ~loc "mutable record fields (other than those of refs)"
  | "%raise" | "%reraise" | "%raise_notrace" ->
      unsupported ~loc "raising exceptions (exceptions are not modelled)"
  | _ -> unsupported ~loc "the primitive %s" name

let describe (e : expression) =
  match e.exp_desc with
  | Texp_array _ | Texp_idx _ -> "arrays"
  | Texp_list_comprehension _ | Texp_array_comprehension _ -> "comprehensions"
  | Texp_atomic_loc _ -> "atomic fields"
  | Texp_try _ -> "exception handlers"
  | Texp_lazy _ -> "lazy values"
  | Texp_variant _ -> "polymorphic variants"
  | Texp_letmodule _ | Texp_pack _ -> "modules in expressions"
  | Texp_letexception _ -> "local exceptions"
  | Texp_send _ | Texp_new _ | Texp_instvar _ | Texp_setinstvar _
  | Texp_override _ | Texp_object _ ->
      "objects"
  | Texp_letop _ -> "binding operators"
  | Texp_unboxed_tuple _ | Texp_unboxed_unit | Texp_unboxed_bool _
  | Texp_record_unboxed_product _ | Texp_unboxed_field _ ->
      "unboxed types"
  | _ -> "this expression"

let rec expr st env (e : expression) =
  let loc = e.exp_loc in
  match e.exp_desc with
  | Texp_ident
      { desc = { val_kind = Val_prim { prim_name = "%boolnot"; _ }; _ }; _ } ->
      Const ("Stdlib_not", true)
  | Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ } ->
      primitive st env ~loc ~fty:e.exp_type ~tenv:e.exp_env prim []
  | Texp_ident { path; _ } -> value_ref st ~loc env path
  | Texp_constant c -> constant ~loc c
  | Texp_let (Nonrecursive, vbs, body) -> let_ st env ~loc vbs body
  | Texp_let
      ( Recursive,
        [ ({ vb_pat = { pat_desc = Tpat_var { id; name; _ }; _ }; _ } as vb) ],
        body ) ->
      let env = bind env id name.txt in
      Let
        ( Named name.txt,
          rec_function st env ~loc name.txt vb.vb_expr,
          expr st env body )
  | Texp_let (Recursive, _, _) ->
      unsupported ~loc "mutually recursive definitions"
  | Texp_function { params; body; _ } -> lam (function_ st env ~loc params body)
  | Texp_apply (f, args, _, _, _, _) -> apply st env ~loc f args
  | Texp_match (scrut, _, cases, [], _) ->
      let cases =
        List.map
          (fun (c : computation case) ->
            match split_pattern c.c_lhs with
            | Some p, None -> (p, c.c_guard, c.c_rhs)
            | _, Some _ | None, None ->
                unsupported ~loc:c.c_lhs.pat_loc "exception patterns")
          cases
      in
      match_ st env ~loc (expr st env scrut) cases
  | Texp_match (_, _, _, _ :: _, _) -> unsupported ~loc "effect handlers"
  | Texp_tuple (es, _) -> tuple (List.map (fun (_, e, _) -> expr st env e) es)
  | Texp_construct (_, cd, _, args, _) ->
      construct ~loc cd (List.map (fun (_, e) -> expr st env e) args)
  | Texp_record
      {
        fields = [| (_, _, Overridden (_, x)) |];
        extended_expression = None;
        _;
      }
    when is_ref e.exp_env e.exp_type ->
      st.stateful <- true;
      App (Const ("Ref_make", true), [ expr st env x ])
  | Texp_record { fields; extended_expression; _ } ->
      record st env ~loc fields extended_expression
  | Texp_field { record; _ } when is_ref record.exp_env record.exp_type ->
      st.stateful <- true;
      App (Const ("Ref_get", true), [ expr st env record ])
  | Texp_setfield { record; newval; _ }
    when is_ref record.exp_env record.exp_type ->
      st.stateful <- true;
      App (Const ("Ref_set", true), [ expr st env record; expr st env newval ])
  | Texp_setfield _ ->
      unsupported ~loc "mutable record fields (other than those of refs)"
  | Texp_field { record; label; _ } ->
      check_label ~loc label;
      project ~pos:label.lbl_pos
        ~n:(Array.length label.lbl_all)
        (expr st env record)
  | Texp_ifthenelse (c, a, b) ->
      If
        ( expr st env c,
          expr st env a,
          match b with Some b -> expr st env b | None -> Unit )
  | Texp_sequence (a, _, b) -> Let (Anon, expr st env a, expr st env b)
  | Texp_while { wh_cond; wh_body; _ } ->
      While (expr st env wh_cond, expr st env wh_body)
  | Texp_for { for_id; for_pat; for_from; for_to; for_dir; for_body; _ } ->
      let x =
        match for_pat.ppat_desc with
        | Ppat_var { txt; _ } -> txt
        | _ -> fresh st "i"
      in
      For
        ( Named x,
          for_dir,
          expr st env for_from,
          expr st env for_to,
          expr st (bind env for_id x) for_body )
  | Texp_letmutable _ | Texp_mutvar _ | Texp_setmutvar _ ->
      unsupported ~loc "mutable variables (let mutable)"
  | Texp_array _ -> unsupported ~loc "array literals"
  | Texp_assert (c, _) -> (
      match expr st env c with
      | Bool false -> stuck st
      | c -> If (c, Unit, stuck st))
  | Texp_open (_, e) -> expr st env e
  | Texp_unreachable -> stuck st
  | _ -> unsupported ~loc "%s" (describe e)

and lam = function [], body -> body | bs, body -> Lam (bs, body)

and rec_function st env ~loc f (e : expression) =
  match e.exp_desc with
  | Texp_function { params; body; _ } -> (
      match function_ st env ~loc params body with
      | [], _ -> Misc.fatal_error "Laws_rocq.rec_function"
      | bs, body -> Rec (f, bs, body))
  | _ -> unsupported ~loc "recursive definitions of values other than functions"

(* The binders and the body of a function. A parameter that is not a
   variable is bound to a variable and matched before the next parameter. *)
and function_ st env ~loc params body =
  match params with
  | [] -> (
      match body with
      | Tfunction_body e -> ([], expr st env e)
      | Tfunction_cases { fc_cases; _ } ->
          let pats = List.map (fun c -> c.c_lhs) fc_cases in
          let x =
            if
              String.Set.mem "param" env.names
              || not (stable (Var "param") pats)
            then fresh st "param"
            else "param"
          in
          let env = { env with names = String.Set.add x env.names } in
          let cases =
            List.map (fun c -> (c.c_lhs, c.c_guard, c.c_rhs)) fc_cases
          in
          ([ Named x ], match_ st env ~loc (Var x) cases))
  | p :: ps -> (
      (match p.fp_arg_label with
      | Optional _ | Position _ -> unsupported ~loc "optional parameters"
      | Nolabel | Labelled _ -> ());
      let simple binder env =
        let bs, body = function_ st env ~loc ps body in
        (binder :: bs, body)
      in
      match p.fp_kind with
      | Tparam_pat { pat_desc = Tpat_var { id; name; _ }; _ } ->
          simple (Named name.txt) (bind env id name.txt)
      | Tparam_pat { pat_desc = Tpat_any; _ } -> simple Anon env
      | Tparam_pat pat ->
          let x = fresh st "p" in
          let env = { env with names = String.Set.add x env.names } in
          ( [ Named x ],
            bind_pattern st env ~loc pat (Var x) (fun env ->
                lam (function_ st env ~loc ps body)) )
      | Tparam_optional_default _ -> unsupported ~loc "optional parameters")

(* [bind_pattern st env p v k] matches [v] against the pattern [p] and
   continues with [k] in the environment of its variables. *)
and bind_pattern st env ~loc:_ (p : pattern) v k =
  match p.pat_desc with
  | Tpat_var { id; name; _ } -> Let (Named name.txt, v, k (bind env id name.txt))
  | Tpat_any -> Let (Anon, v, k env)
  | _ ->
      let with_value k =
        if stable v [ p ] then k v
        else
          let x = fresh st "v" in
          Let (Named x, v, k (Var x))
      in
      with_value (fun v ->
          let test, bs = pattern_test p v in
          let body = lets bs (k (bind_all env bs)) in
          match test with None -> body | Some test -> If (test, body, stuck st))

and let_ st env ~loc vbs body =
  match vbs with
  | [ vb ] ->
      bind_pattern st env ~loc vb.vb_pat (expr st env vb.vb_expr) (fun env ->
          expr st env body)
  | vbs ->
      (* The bound expressions are evaluated in the outer environment. *)
      let tmps = List.map (fun vb -> (fresh st "l", vb)) vbs in
      let rec go env' = function
        | [] -> expr st env' body
        | (x, vb) :: rest ->
            bind_pattern st env' ~loc vb.vb_pat (Var x) (fun env' ->
                go env' rest)
      in
      List.fold_right
        (fun (x, vb) body -> Let (Named x, expr st env vb.vb_expr, body))
        tmps (go env tmps)

and apply st env ~loc f args =
  let args =
    List.map
      (fun (label, arg) ->
        match ((label : arg_label), arg) with
        | (Optional _ | Position _), _ -> unsupported ~loc "optional arguments"
        | _, Omitted _ ->
            unsupported ~loc
              "partial applications that omit a labelled argument"
        | (Nolabel | Labelled _), Arg (e, _) -> e)
      args
  in
  match f.exp_desc with
  | Texp_ident { desc = { val_kind = Val_prim prim; _ }; _ } ->
      primitive st env ~loc ~fty:f.exp_type ~tenv:f.exp_env prim args
  | _ -> App (expr st env f, List.map (expr st env) args)

(* A primitive applied to [args], eta-expanded when partially applied *)
and primitive st env ~loc ~fty ~tenv (prim : Primitive.description) args =
  let arg_ty, tenv =
    match args with
    | (a : expression) :: _ -> (a.exp_type, a.exp_env)
    | [] -> (
        match get_desc (Ctype.expand_head tenv fty) with
        | Tarrow (_, a, _, _) -> (a, tenv)
        | _ -> (fty, tenv))
  in
  match state_primitive ~tenv ~arg_ty prim.prim_name with
  | Some f -> (
      st.stateful <- true;
      let f = Const (f, true) in
      match args with [] -> f | args -> App (f, List.map (expr st env) args))
  | None ->
      let op = primitive_op st ~loc ~tenv ~arg_ty prim.prim_name in
      let n = List.length args and arity = prim.prim_arity in
      if n > arity then
        unsupported ~loc "the primitive %s applied to too many arguments"
          prim.prim_name
      else
        let args = List.map (expr st env) args in
        if n = arity then op args
        else
          let given = List.map (fun a -> (fresh st "a", a)) args in
          let missing = List.init (arity - n) (fun _ -> fresh st "x") in
          List.fold_right
            (fun (x, a) body -> Let (Named x, a, body))
            given
            (Lam
               ( List.map (fun x -> Named x) missing,
                 op
                   (List.map (fun (x, _) -> Var x) given
                   @ List.map (fun x -> Var x) missing) ))

and record st env ~loc fields extended =
  let fields = Array.to_list fields in
  List.iter
    (fun ((lbl : Data_types.label_description), _, _) -> check_label ~loc lbl)
    fields;
  let fields =
    List.sort
      (fun ((a : Data_types.label_description), _, _)
           ((b : Data_types.label_description), _, _) ->
        Int.compare a.lbl_pos b.lbl_pos)
      fields
  in
  let n = List.length fields in
  let build base =
    tuple
      (List.map
         (fun ((lbl : Data_types.label_description), _, def) ->
           match (def, base) with
           | Overridden (_, e), _ -> expr st env e
           | Kept _, Some base -> project ~pos:lbl.lbl_pos ~n base
           | Kept _, None -> Misc.fatal_error "Laws_rocq.record")
         fields)
  in
  match extended with
  | None -> build None
  | Some (base, _, _, _) -> (
      match expr st env base with
      | (Var _ | Const _) as base -> build (Some base)
      | base ->
          let x = fresh st "r" in
          Let (Named x, base, build (Some (Var x))))

and match_ st env ~loc scrut cases =
  match structured st env ~loc scrut cases with
  | Some e -> e
  | None -> tests st env scrut cases

(* A match on a variant whose cases are distinct constructors with
   irrefutable arguments, and maybe a final catch-all case: [if:],
   [MatchList] or [MatchTag] *)
and structured st env ~loc scrut cases =
  let catch_all (p : pattern) =
    match p.pat_desc with Tpat_any | Tpat_var _ -> true | _ -> false
  in
  let rec split acc = function
    | [] -> Some (List.rev acc, None)
    | [ (p, None, rhs) ] when catch_all p -> Some (List.rev acc, Some (p, rhs))
    | (p, None, rhs) :: rest -> (
        match p.pat_desc with
        | Tpat_construct (_, cd, _, args, _)
          when List.for_all (fun (_, a) -> irrefutable a) args
               && (match cd.cstr_tag with Ordinary _ -> true | _ -> false)
               && Option.is_none cd.cstr_inlined ->
            split ((cd, List.map snd args, rhs) :: acc) rest
        | _ -> None)
    | (_, Some _, _) :: _ -> None
  in
  match split [] cases with
  | None | Some ([], _) -> None
  | Some (((cd0, _, _) :: _ as arms), default) -> (
      let indices = List.map (fun (cd, _, _) -> constructor_index cd) arms in
      let distinct =
        List.length (List.sort_uniq Int.compare indices) = List.length indices
      in
      let all_covered =
        List.length arms = cd0.cstr_consts + cd0.cstr_nonconsts
      in
      let arm index =
        List.find_opt (fun (cd, _, _) -> constructor_index cd = index) arms
      in
      (* The default case, with the scrutinee bound to [v] *)
      let default_body v =
        match default with
        | None -> stuck st
        | Some ({ pat_desc = Tpat_var { id; name; _ }; _ }, rhs) ->
            Let (Named name.txt, v, expr st (bind env id name.txt) rhs)
        | Some (_, rhs) -> expr st env rhs
      in
      let with_scrut k =
        match default with
        | Some ({ pat_desc = Tpat_var _; _ }, _) when not (stable scrut []) ->
            let x = fresh st "m" in
            Let (Named x, scrut, k (Var x))
        | _ -> k scrut
      in
      let body_of v args rhs =
        let test, bs =
          match args with
          | [] -> (None, [])
          | [ a ] -> pattern_test a v
          | args -> components args v
        in
        (* The arguments are irrefutable: [test] is [None]. *)
        ignore test;
        lets bs (expr st (bind_all env bs) rhs)
      in
      if not distinct then None
      else
        match constructor_kind ~loc cd0 with
        | Ctor_unit -> None
        | Ctor_bool _ ->
            if
              Option.is_some default
              && (not (stable scrut []))
              &&
              match default with
              | Some ({ pat_desc = Tpat_var _; _ }, _) -> true
              | _ -> false
            then None
            else
              let branch index =
                match arm index with
                | Some (_, _, rhs) -> expr st env rhs
                | None -> default_body scrut
              in
              (* [false] is constructor 0, [true] constructor 1 *)
              Some (If (scrut, branch 1, branch 0))
        | Ctor_list when all_covered -> (
            match (arm 0, arm 1) with
            | Some (_, [], nil), Some (_, [ y; ys ], cons) ->
                let binder (p : pattern) env =
                  match p.pat_desc with
                  | Tpat_var { id; name; _ } ->
                      (Named name.txt, bind env id name.txt, [])
                  | Tpat_any -> (Anon, env, [])
                  | _ ->
                      let x = fresh st "y" in
                      let _, bs = pattern_test p (Var x) in
                      (Named x, bind_all env bs, bs)
                in
                let by, env', bs_y = binder y env in
                let bys, env', bs_ys = binder ys env' in
                Some
                  (Match_list
                     ( scrut,
                       expr st env nil,
                       by,
                       bys,
                       lets (bs_y @ bs_ys) (expr st env' cons) ))
            | _ -> None)
        | Ctor_list | Ctor_option | Ctor_tag _ ->
            let x = "$payload" in
            let uses_payload =
              List.exists
                (fun (_, args, _) ->
                  List.exists
                    (fun (a : pattern) ->
                      match a.pat_desc with Tpat_any -> false | _ -> true)
                    args)
                arms
            in
            Some
              (with_scrut (fun v ->
                   Match_tag
                     ( v,
                       (if uses_payload then Named x else Anon),
                       List.map
                         (fun (cd, args, rhs) ->
                           (constructor_index cd, body_of (Var x) args rhs))
                         arms,
                       if all_covered then stuck st else default_body v ))))

(* Any other match: a sequence of tests *)
and tests st env scrut cases =
  let with_scrut k =
    if stable scrut (List.map (fun (p, _, _) -> p) cases) then k scrut
    else
      let x = fresh st "m" in
      Let (Named x, scrut, k (Var x))
  in
  with_scrut (fun v ->
      let rec chain = function
        | [] -> stuck st
        | (p, guard, rhs) :: rest -> (
            let test, bs = pattern_test p v in
            let env' = bind_all env bs in
            match guard with
            | None -> (
                let body = lets bs (expr st env' rhs) in
                match test with
                | None -> body
                | Some test -> If (test, body, chain rest))
            | Some guard ->
                let k = fresh st "k" in
                let next = App (Var k, [ Unit ]) in
                let body =
                  lets bs (If (expr st env' guard, expr st env' rhs, next))
                in
                let body =
                  match test with
                  | None -> body
                  | Some test -> If (test, body, next)
                in
                Let (Named k, Lam ([ Anon ], chain rest), body))
      in
      chain cases)

(* Phase 3 types and modes (see TARGET.md) *)

(* [(Mode L C P)] from the modes as written; the other axes are ignored. *)
let mode3 modes =
  let axis values default =
    List.fold_left
      (fun acc m ->
        match List.assoc_opt m values with Some v -> v | None -> acc)
      default modes
  in
  Printf.sprintf "(Mode %s %s %s)"
    (axis [ ("local", "Local"); ("global", "Global") ] "Global")
    (axis
       [
         ("uncontended", "Uncontended");
         ("shared", "Shared");
         ("contended", "Contended");
       ]
       "Uncontended")
    (axis
       [ ("portable", "Portable"); ("nonportable", "Nonportable") ]
       "Nonportable")

let default_mode3 = mode3 []

let is_stdlib_ref (p : Path.t) =
  match Path.name p with "Stdlib.ref" | "Stdlib__.ref" -> true | _ -> false

(* Whether values of [ty] are immutable data without functions: Phase 3
   wraps their Phase 1 semantic type in [TPure]. *)
let pure_type ~tenv ty =
  (* The declarations of types are inspected once. *)
  let seen = ref [] in
  let rec pure ty =
    match get_desc (Ctype.expand_head tenv ty) with
    | Tvar _ | Tunivar _ -> true
    | Tarrow _ -> false
    | Ttuple comps -> List.for_all (fun (_, ty) -> pure ty) comps
    | Tpoly (ty, []) -> pure ty
    | Tconstr (p, args, _) ->
        (not (Path.same p Predef.path_array))
        && List.for_all pure args
        && (List.exists (Path.same p) !seen
           ||
           (seen := p :: !seen;
            match Env.find_type p tenv with
            | { type_kind = Type_record (lds, _, _); _ } ->
                List.for_all
                  (fun (ld : Types.label_declaration) ->
                    (not (Types.is_mutable ld.ld_mutable)) && pure ld.ld_type)
                  lds
            | { type_kind = Type_variant (cds, _, _); _ } ->
                List.for_all
                  (fun (cd : Types.constructor_declaration) ->
                    List.for_all pure (Types.tys_of_constr_args cd.cd_args))
                  cds
            | _ -> true
            | exception Not_found -> true))
    | _ -> false
  in
  pure ty

(* Whether [ty] is mutable: an array or a ref, maybe inside other types *)
let rec mutable_type ~tenv ty =
  match get_desc (Ctype.expand_head tenv ty) with
  | Tconstr (p, args, _) ->
      Path.same p Predef.path_array
      || is_stdlib_ref p
      || List.exists (mutable_type ~tenv) args
  | Ttuple comps -> List.exists (fun (_, ty) -> mutable_type ~tenv ty) comps
  | _ -> false

let parens_if_app s = if String.contains s ' ' then "(" ^ s ^ ")" else s

(* The Phase 3 type [ty] of [ty], printed. [total] makes a function type a
   total stateless function. *)
let rec type3 st ~loc ~tenv ~vars ?(total = false) ty =
  let sem1 ty =
    Format.asprintf "%a" Print.sty (sem_type st ~loc ~tenv ~vars ty)
  in
  let pure_arg what ty =
    if pure_type ~tenv ty then parens_if_app (sem1 ty)
    else
      unsupported ~loc "%s of data that is mutable or contains functions" what
  in
  let expanded = Ctype.expand_head tenv ty in
  match get_desc expanded with
  | Tpoly (ty, []) -> type3 st ~loc ~tenv ~vars ~total ty
  | Tconstr (p, [], _) when Path.same p Predef.path_int -> "TInt"
  | Tconstr (p, [], _) when Path.same p Predef.path_bool -> "TBool"
  | Tconstr (p, [], _) when Path.same p Predef.path_unit -> "TUnit"
  | Tconstr (p, [ a ], _) when Path.same p Predef.path_array ->
      "(TArray " ^ pure_arg "arrays" a ^ ")"
  | Tconstr (p, [ a ], _) when is_stdlib_ref p ->
      "(TRef " ^ pure_arg "references" a ^ ")"
  | Tconstr (p, [ a ], _)
    when Path.same p Predef.path_list && not (pure_type ~tenv a) ->
      "(TList " ^ type3 st ~loc ~tenv ~vars a ^ ")"
  | Tarrow (((Nolabel | Labelled _), _, _), _, _, _) when total ->
      "(TPure " ^ parens_if_app (sem1 ty) ^ ")"
  | Tarrow (((Nolabel | Labelled _), _, _), a, b, _) ->
      Printf.sprintf "(TArrow %s %s %s %s)"
        (type3 st ~loc ~tenv ~vars a)
        default_mode3
        (type3 st ~loc ~tenv ~vars b)
        default_mode3
  | _ when pure_type ~tenv ty -> "(TPure " ^ parens_if_app (sem1 ty) ^ ")"
  | _ ->
      unsupported ~loc "this type in Phase 3 (mutable data inside other data)"

(* Clauses *)

let rec clause st env ~avoid ~phase ~vars (c : law_clause) =
  let loc = c.lc_loc in
  let clause = clause st ~phase ~vars in
  match c.lc_desc with
  | Tlc_bool e -> Spec_true (expr st env (Translspec.typedtree e))
  | Tlc_equal (e1, e2) ->
      Spec_eq
        ( expr st env (Translspec.typedtree e1),
          expr st env (Translspec.typedtree e2) )
  | Tlc_and (c1, c2) -> Spec_and (clause env ~avoid c1, clause env ~avoid c2)
  | Tlc_wp { binding; modes; body } ->
      let e = Translspec.typedtree binding.svb_expr in
      let pat = Translspec.typedtree_value_pattern binding.svb_pat in
      let ty, mode =
        match phase with
        | Phase3 ->
            ( type3 st ~loc ~tenv:e.exp_env ~vars e.exp_type,
              mode3
                (List.map
                   (fun { Location.txt = Parsetree.Mode m; _ } -> m)
                   modes) )
        | Phase1 | Phase2 -> ("", "")
      in
      let e' = expr st env e in
      let var, env, avoid =
        match pat.pat_desc with
        | Tpat_var { id; name; _ } ->
            let x = fresh_name ~avoid name.txt in
            ( Some x,
              {
                env with
                locals = Ident.Map.add id (Const (x, true)) env.locals;
              },
              String.Set.add x avoid )
        | Tpat_any -> (None, env, avoid)
        | Tpat_construct (_, cd, _, [], _)
          when Path.same (Data_types.cstr_res_type_path cd) Predef.path_unit ->
            (None, env, avoid)
        | _ ->
            unsupported ~loc:pat.pat_loc
              "let%%wp patterns other than a variable, () and _"
      in
      Spec_wp { var; ty; mode; e = e'; body = clause env ~avoid body }

let rec has_wp = function
  | Spec_true _ | Spec_eq _ -> false
  | Spec_and (c1, c2) -> has_wp c1 || has_wp c2
  | Spec_wp _ -> true

(* Items *)

(* The source of the item at [loc], when it can go in a comment *)
let source_text =
  let cache = Hashtbl.create 1 in
  fun (loc : Location.t) ->
    let file = loc.loc_start.pos_fname in
    let contents =
      match Hashtbl.find_opt cache file with
      | Some c -> c
      | None ->
          let c =
            try Some (In_channel.with_open_bin file In_channel.input_all)
            with Sys_error _ -> None
          in
          Hashtbl.add cache file c;
          c
    in
    match contents with
    | None -> None
    | Some s ->
        let start = loc.loc_start.pos_cnum and stop = loc.loc_end.pos_cnum in
        if start < 0 || stop > String.length s || start >= stop then None
        else
          let text = String.sub s start (stop - start) in
          let contains sub =
            let n = String.length sub in
            let rec loop i =
              i + n <= String.length text
              && (String.equal (String.sub text i n) sub || loop (i + 1))
            in
            loop 0
          in
          if contains "(*" || contains "*)" || contains "\"" then None
          else Some text

let source_comment ppf loc =
  match source_text loc with
  | None -> ()
  | Some text ->
      let lines = String.split_on_char '\n' text in
      Format.fprintf ppf "@[<v 4>(** [%a] *)@]@."
        (Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string)
        lines

let not_translated st ~loc what reason =
  let line = loc.Location.loc_start.pos_lnum in
  emit st (fun ppf ->
      Format.fprintf ppf
        "@[<hov 3>(* %s (line %d) is not translated:@ %s. *)@]@." what line
        reason)

let register_value st minfo id name info =
  st.values <- Ident.Map.add id info st.values;
  Option.iter (fun m -> Hashtbl.replace m.m_values name info) minfo

let register_type st minfo id name info =
  st.types <- Ident.Map.add id info st.types;
  Option.iter (fun m -> Hashtbl.replace m.m_types name info) minfo

let definition st ~prefix ~display minfo ~loc id name body =
  st.stateful <- false;
  match
    let body = body () in
    if st.stateful && not (is_value body) then
      unsupported ~loc
        "it is stateful but not a value: its effects would be repeated at each \
         use";
    body
  with
  | body ->
      let rocq = define st (prefix ^ name) in
      let is_value = is_value body in
      let stateful = st.stateful in
      if stateful then st.uses_state <- true;
      register_value st minfo id name (Translated { rocq; is_value; stateful });
      st.aliases <- (prefix ^ name, rocq) :: st.aliases;
      emit st (fun ppf ->
          source_comment ppf loc;
          if is_value then
            Format.fprintf ppf "@[<hv 2>Definition %s : val :=@ (%a)%%V.@]@."
              rocq Print.value body
          else
            Format.fprintf ppf "@[<hv 2>Definition %s : expr :=@ (%a)%%E.@]@."
              rocq Print.expr body)
  | exception Unsupported (_, reason) ->
      register_value st minfo id name
        (Untranslated { name = display name; reason });
      not_translated st ~loc (display name) reason

let values st ~prefix ~display minfo ~loc rec_flag vbs =
  let single = match vbs with [ _ ] -> true | _ -> false in
  let loc_of vb = if single then loc else vb.vb_loc in
  match ((rec_flag : Asttypes.rec_flag), vbs) with
  | ( Recursive,
      [ ({ vb_pat = { pat_desc = Tpat_var { id; name; _ }; _ }; _ } as vb) ] )
    ->
      definition st ~prefix ~display minfo ~loc id name.txt (fun () ->
          rec_function st
            (bind empty_env id name.txt)
            ~loc:vb.vb_loc name.txt vb.vb_expr)
  | Recursive, vbs ->
      List.iter
        (fun vb ->
          List.iter
            (fun id ->
              let name = Ident.name id in
              let reason = "mutually recursive definitions are not supported" in
              register_value st minfo id name
                (Untranslated { name = display name; reason });
              not_translated st ~loc:vb.vb_loc (display name) reason)
            (pat_bound_idents vb.vb_pat))
        vbs
  | Nonrecursive, vbs ->
      List.iter
        (fun vb ->
          match vb.vb_pat.pat_desc with
          | Tpat_var { id; name; _ } ->
              definition st ~prefix ~display minfo ~loc:(loc_of vb) id name.txt
                (fun () -> expr st empty_env vb.vb_expr)
          | _ ->
              List.iter
                (fun id ->
                  let name = Ident.name id in
                  let reason = "it is bound by a pattern" in
                  register_value st minfo id name
                    (Untranslated { name = display name; reason });
                  not_translated st ~loc:vb.vb_loc (display name) reason)
                (pat_bound_idents vb.vb_pat))
        vbs

(* A group of type declarations is a group of inductive predicates, or a
   definition for an abbreviation. *)
let types st ~prefix ~display minfo ~loc ~tenv (decls : type_declaration list) =
  let names =
    List.map
      (fun (d : type_declaration) ->
        (d, define st ("ty_" ^ prefix ^ Ident.name d.typ_id)))
      decls
  in
  let group =
    List.map
      (fun ((d : type_declaration), name) ->
        (d.typ_id, name, d.typ_type.type_params))
      names
  in
  let params (d : type_declaration) =
    type_var_names ~avoid:st.used d.typ_type.type_params
  in
  let binders vars =
    String.concat ""
      (List.map (fun (_, a) -> Printf.sprintf " (%s : sem_ty)" a) vars)
  in
  let member_name vars name =
    Sty_app (name, List.map (fun (_, a) -> Sty_var a) vars)
  in
  let refers_to_group ty =
    let found = ref false in
    let rec it ty =
      match get_desc ty with
      | Tconstr (Pident id, _, _)
        when List.exists (fun (id', _, _) -> Ident.same id id') group ->
          found := true
      | _ -> Btype.iter_type_expr it ignore ty
    in
    it ty;
    !found
  in
  let arg_types (d : type_declaration) =
    match d.typ_type.type_kind with
    | Type_variant (cds, _, _) ->
        List.concat_map
          (fun (cd : Types.constructor_declaration) ->
            match cd.cd_args with
            | Cstr_tuple cas ->
                List.map
                  (fun (ca : Types.constructor_argument) -> ca.ca_type)
                  cas
            | Cstr_record _ -> [])
          cds
    | Type_record (lds, _, _) ->
        List.map (fun (ld : Types.label_declaration) -> ld.ld_type) lds
    | _ -> Option.to_list d.typ_type.type_manifest
  in
  let recursive =
    List.exists (fun (d, _) -> List.exists refers_to_group (arg_types d)) names
  in
  (* The constructors of the inductive predicate of [d] *)
  let constructors (d : type_declaration) name vars =
    let loc = d.typ_loc in
    let sem ty =
      if recursive then sem_type st ~loc ~tenv ~vars ~group ~direct:true ty
      else sem_type st ~loc ~tenv ~vars ty
    in
    let constructor cname value_of tys =
      let tys = List.map sem tys in
      let vs = List.mapi (fun i _ -> "v" ^ Int.to_string i) tys in
      (cname, vs, List.combine tys vs, value_of vs)
    in
    let payload vs =
      let rec go = function
        | [] -> "#()"
        | [ v ] -> v
        | v :: vs -> Printf.sprintf "(%s, %s)" v (go vs)
      in
      go vs
    in
    match d.typ_type.type_kind with
    | Type_variant (cds, _, _) ->
        List.mapi
          (fun i (cd : Types.constructor_declaration) ->
            if Option.is_some cd.cd_res then
              unsupported ~loc "generalized algebraic data types";
            match cd.cd_args with
            | Cstr_record _ -> unsupported ~loc "inline records"
            | Cstr_tuple cas ->
                constructor
                  (name ^ "_" ^ Ident.name cd.cd_id)
                  (fun vs -> Printf.sprintf "TAGV %d %s" i (payload vs))
                  (List.map
                     (fun (ca : Types.constructor_argument) -> ca.ca_type)
                     cas))
          cds
    | Type_record (lds, _, _) ->
        List.iter
          (fun (ld : Types.label_declaration) ->
            if Types.is_mutable ld.ld_mutable then
              unsupported ~loc "mutable record fields")
          lds;
        [
          constructor (name ^ "_intro")
            (fun vs -> match vs with [ v ] -> v | vs -> payload vs)
            (List.map (fun (ld : Types.label_declaration) -> ld.ld_type) lds);
        ]
    | Type_abstract _ -> unsupported ~loc "abstract types"
    | Type_record_unboxed_product _ -> unsupported ~loc "unboxed records"
    | Type_open -> unsupported ~loc "extensible variants"
  in
  let print_constructors ppf (head, ctors) =
    List.iter
      (fun (cname, vs, premises, value) ->
        Format.fprintf ppf "@,@[<hov 4>| %s%s :" cname
          (match vs with
          | [] -> ""
          | vs -> Printf.sprintf " (%s : val)" (String.concat " " vs));
        List.iter
          (fun (ty, v) -> Format.fprintf ppf "@ %a →" Print.typing (ty, v))
          premises;
        Format.fprintf ppf "@ %a %s@]" Print.sty head
          (if String.starts_with ~prefix:"TAGV" value then "(" ^ value ^ ")"
           else value))
      ctors
  in
  match
    match names with
    | [
     ( ({
          typ_type = { type_kind = Type_abstract _; type_manifest = Some ty; _ };
          _;
        } as d),
       name );
    ] ->
        if recursive then unsupported ~loc:d.typ_loc "recursive abbreviations";
        let vars = params d in
        let def = sem_type st ~loc:d.typ_loc ~tenv ~vars ty in
        fun ppf ->
          source_comment ppf d.typ_loc;
          Format.fprintf ppf "@[<hv 2>Definition %s%s : sem_ty :=@ %a.@]@." name
            (binders vars) Print.sty def
    | _ ->
        let arity = List.length (List.hd decls).typ_type.type_params in
        if
          recursive
          && List.exists
               (fun (d : type_declaration) ->
                 List.length d.typ_type.type_params <> arity)
               decls
        then
          unsupported ~loc
            "mutually recursive types with different numbers of parameters";
        let bodies =
          List.map
            (fun (d, name) ->
              let vars = params d in
              (d, name, vars, constructors d name vars))
            names
        in
        fun ppf ->
          List.iter
            (fun ((d : type_declaration), _, _, _) ->
              source_comment ppf d.typ_loc)
            bodies;
          List.iteri
            (fun i (_, name, vars, ctors) ->
              Format.fprintf ppf "@[<v 2>%s %s%s : val → Prop :=%a@]"
                (if i = 0 then "Inductive" else "with")
                name (binders vars) print_constructors
                (member_name vars name, ctors);
              if i < List.length bodies - 1 then Format.fprintf ppf "@.")
            bodies;
          Format.fprintf ppf ".@."
  with
  | print ->
      List.iter
        (fun ((d : type_declaration), name) ->
          let tname = Ident.name d.typ_id in
          register_type st minfo d.typ_id tname (Sem name);
          st.aliases <- ("ty_" ^ prefix ^ tname, name) :: st.aliases)
        names;
      emit st print
  | exception Unsupported (_, reason) ->
      List.iter
        (fun ((d : type_declaration), _) ->
          let tname = Ident.name d.typ_id in
          register_type st minfo d.typ_id tname
            (No_sem { name = display tname; reason });
          not_translated st ~loc:d.typ_loc ("The type " ^ display tname) reason)
        names

let rec typed_has_wp (c : law_clause) =
  match c.lc_desc with
  | Tlc_bool _ | Tlc_equal _ -> false
  | Tlc_and (c1, c2) -> typed_has_wp c1 || typed_has_wp c2
  | Tlc_wp _ -> true

let rec typed_has_modes (c : law_clause) =
  match c.lc_desc with
  | Tlc_bool _ | Tlc_equal _ -> false
  | Tlc_and (c1, c2) -> typed_has_modes c1 || typed_has_modes c2
  | Tlc_wp { modes; body; _ } -> modes <> [] || typed_has_modes body

(* The phase of a law, from its syntax and the types of its parameters
   (TARGET.md, section 1) *)
let phase_of_law st (ld : law_declaration) =
  let clauses = ld.law_conclusion :: ld.law_assumptions in
  let stateful_params =
    List.exists
      (fun (lp : law_param) ->
        lp.lp_modes <> []
        || mutable_type ~tenv:lp.lp_type.ctyp_env lp.lp_type.ctyp_type)
      ld.law_params
  in
  if stateful_params || List.exists typed_has_modes clauses then Phase3
  else if List.exists typed_has_wp clauses then
    (* Phase 2 runs only code that does not touch the heap. *)
    if st.uses_state then Phase3 else Phase2
  else Phase1

let law st ~prefix (ld : law_declaration) =
  let name = Ident.name ld.law_id in
  let lw = ld.law_law in
  match
    let phase = phase_of_law st ld in
    let avoid = st.used in
    let newtypes =
      List.map
        (fun (nt : law_newtype) -> (get_id nt.ln_type, nt.ln_name.txt))
        ld.law_newtypes
    in
    let vars =
      type_var_names ~avoid (List.map (fun (_, ty, _) -> ty) lw.law_params)
      |> List.map (fun (id, a) ->
          match List.assoc_opt id newtypes with
          | Some n -> (id, fresh_name ~avoid (String.capitalize_ascii n))
          | None -> (id, a))
    in
    let avoid =
      List.fold_left (fun avoid (_, a) -> String.Set.add a avoid) avoid vars
    in
    let avoid =
      match phase with
      | Phase3 -> String.Set.add "Σ" (String.Set.add "HΣ" avoid)
      | Phase1 | Phase2 -> avoid
    in
    let avoid, params =
      List.fold_left_map
        (fun avoid (id, ty, modes) ->
          let x = fresh_name ~avoid (Ident.name id) in
          (String.Set.add x avoid, (id, x, ty, modes)))
        avoid lw.law_params
    in
    let param_of id =
      List.find_opt
        (fun (lp : law_param) -> Ident.same lp.lp_id id)
        ld.law_params
    in
    let tenv_of id =
      match param_of id with
      | Some lp -> lp.lp_type.ctyp_env
      | None -> Env.empty
    in
    let total id =
      match param_of id with
      | Some lp ->
          List.exists
            (fun (a : Parsetree.attribute) ->
              String.equal a.attr_name.txt "total")
            lp.lp_type.ctyp_attributes
      | None -> false
    in
    let hyps =
      List.map
        (fun (id, x, ty, modes) ->
          match phase with
          | Phase1 | Phase2 ->
              `Hyp (sem_type st ~loc:ld.law_loc ~tenv:(tenv_of id) ~vars ty, x)
          | Phase3 ->
              `Forall
                ( x,
                  type3 st ~loc:ld.law_loc ~tenv:(tenv_of id) ~vars
                    ~total:(total id) ty,
                  mode3 modes ))
        params
    in
    let env =
      {
        empty_env with
        locals =
          List.fold_left
            (fun locals (id, x, _, _) ->
              Ident.Map.add id (Const (x, true)) locals)
            Ident.Map.empty params;
      }
    in
    let clause c = clause st env ~avoid ~phase ~vars c in
    let assumptions = List.map clause ld.law_assumptions in
    let conclusion = clause ld.law_conclusion in
    (phase, vars, params, hyps, assumptions, conclusion)
  with
  | phase, vars, params, hyps, assumptions, conclusion ->
      st.max_phase <-
        (match (st.max_phase, phase) with
        | Phase3, _ | _, Phase3 -> Phase3
        | Phase2, _ | _, Phase2 -> Phase2
        | Phase1, Phase1 -> Phase1);
      let rocq = define st (prefix ^ name ^ "_statement") in
      emit st (fun ppf ->
          source_comment ppf ld.law_loc;
          Format.fprintf ppf "@[<v 2>Definition %s : Prop :=" rocq;
          let type_binders =
            List.map (fun (_, a) -> Printf.sprintf "(%s : sem_ty)" a) vars
          in
          let binders =
            match phase with
            | Phase1 | Phase2 ->
                type_binders
                @ List.map
                    (fun (_, x, _, _) -> Printf.sprintf "(%s : val)" x)
                    params
            | Phase3 -> "(Σ : gFunctors)" :: "(HΣ : heapGS Σ)" :: type_binders
          in
          let indent = if binders = [] then "" else "  " in
          if binders <> [] then
            Format.fprintf ppf "@,∀ %s," (String.concat " " binders);
          match phase with
          | Phase1 ->
              List.iter
                (function
                  | `Hyp h ->
                      Format.fprintf ppf "@,%s%a →" indent Print.typing h
                  | `Forall _ -> ())
                hyps;
              List.iter
                (fun c -> Format.fprintf ppf "@,%s%a →" indent Print.clause1 c)
                assumptions;
              Format.fprintf ppf "@,%s%a.@]@." indent Print.clause1 conclusion
          | Phase2 ->
              List.iter
                (function
                  | `Hyp h ->
                      Format.fprintf ppf "@,%s%a →" indent Print.typing h
                  | `Forall _ -> ())
                hyps;
              Format.fprintf ppf "@,%s@[<v 2>⊢ " indent;
              List.iteri
                (fun i c ->
                  if i > 0 then Format.fprintf ppf "@,";
                  Format.fprintf ppf "%a ===>"
                    (Print.parens_if (has_wp c) Print.clause2)
                    c)
                assumptions;
              if assumptions <> [] then Format.fprintf ppf "@,";
              Format.fprintf ppf "%a.@]@]@." Print.clause2 conclusion
          | Phase3 ->
              Format.fprintf ppf "@,%s@[<v 2>slaw_valid (" indent;
              List.iteri
                (fun i -> function
                  | `Forall (x, ty, mode) ->
                      if i > 0 then Format.fprintf ppf "@,";
                      Format.fprintf ppf "∀ₗ %s : %s @@ %s," x ty mode
                  | `Hyp _ -> ())
                hyps;
              List.iter
                (fun c -> Format.fprintf ppf "@,%a ===>" Print.premise3 c)
                assumptions;
              Format.fprintf ppf "@,%a).@]@]@." Print.clause3 conclusion)
  | exception Unsupported (loc, msg) ->
      Location.raise_errorf ~loc
        "@[The law %s cannot be translated to Rocq:@ %s.@]" name msg

let rec structure st ~prefix ~display minfo (str : structure) =
  List.iter (structure_item st ~prefix ~display minfo) str.str_items

and structure_item st ~prefix ~display minfo item =
  let loc = item.str_loc in
  match item.str_desc with
  | Tstr_value (rec_flag, vbs) ->
      values st ~prefix ~display minfo ~loc rec_flag vbs
  | Tstr_type (_, decls) ->
      types st ~prefix ~display minfo ~loc ~tenv:item.str_env decls
  | Tstr_law ld -> law st ~prefix ld
  | Tstr_module { mb_id = Some id; mb_expr; _ } -> (
      let name = Ident.name id in
      let rec body (me : module_expr) =
        match me.mod_desc with
        | Tmod_structure str -> Some str
        | Tmod_constraint (me, _, _, _) -> body me
        | _ -> None
      in
      match body mb_expr with
      | Some str ->
          let m = new_module () in
          st.modules <- Ident.Map.add id m st.modules;
          Option.iter (fun p -> Hashtbl.replace p.m_modules name m) minfo;
          structure st
            ~prefix:(prefix ^ name ^ "_")
            ~display:(fun x -> display (name ^ "." ^ x))
            (Some m) str
      | None ->
          not_translated st ~loc
            ("The module " ^ display name)
            "only structures are supported")
  | Tstr_eval _ | Tstr_primitive _ | Tstr_typext _ | Tstr_exception _
  | Tstr_module { mb_id = None; _ }
  | Tstr_recmodule _ | Tstr_modtype _ | Tstr_open _ | Tstr_class _
  | Tstr_class_type _ | Tstr_include _ | Tstr_attribute _ | Tstr_jkind _ ->
      ()

let write ~source_file ~unit (impl : implementation) ~output =
  let st =
    {
      values = Ident.Map.empty;
      types = Ident.Map.empty;
      modules = Ident.Map.empty;
      used = String.Set.empty;
      requires = String.Set.empty;
      uses_stuck = false;
      stateful = false;
      uses_state = false;
      max_phase = Phase1;
      counter = 0;
      aliases = [];
      items = [];
    }
  in
  structure st ~prefix:"" ~display:Fun.id None impl.structure;
  let unit_name = Compilation_unit.name_as_string unit in
  Misc.protect_output_to_file output (fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      Format.pp_set_margin ppf 80;
      Format.fprintf ppf
        "@[<v 4>(** Generated from %s by the OxCaml compiler (-laws-rocq);@ do \
         not edit. The laws are stated as [<law>_statement] and proved@ in a \
         separate file. *)@]@."
        (Filename.basename source_file);
      Format.fprintf ppf "From oxiris Require Import %s.@."
        (match (st.max_phase, st.uses_state) with
        | Phase3, _ | _, true -> "law_prelude_state"
        | Phase2, false -> "law_prelude_wp"
        | Phase1, false -> "law_prelude");
      String.Set.iter
        (fun unit ->
          Format.fprintf ppf "From oxiris.generated Require Import %s.@."
            (String.uncapitalize_ascii unit))
        st.requires;
      if st.uses_stuck then
        Format.fprintf ppf
          "@.@[<v 4>(** [Stuck] has no value: it stands for a raised \
           exception@ (a match failure, a failed assertion or a division by \
           zero). *)@]@.Definition Stuck : expr := (#() #())%%E.@.";
      List.iter (fun item -> Format.fprintf ppf "@.%t" item) (List.rev st.items);
      (match List.rev st.aliases with
      | [] -> ()
      | aliases ->
          Format.fprintf ppf
            "@.(** The names by which other units refer to this one. *)@.";
          List.iter
            (fun (name, rocq) ->
              let qualified =
                if String.starts_with ~prefix:"ty_" name then
                  "ty_" ^ unit_name ^ "_"
                  ^ String.sub name 3 (String.length name - 3)
                else unit_name ^ "_" ^ name
              in
              Format.fprintf ppf "Notation %s := %s (only parsing).@." qualified
                rocq)
            aliases);
      Format.pp_print_flush ppf ())
