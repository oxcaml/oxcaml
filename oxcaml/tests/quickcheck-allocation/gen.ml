(* Type-directed program generator. See DESIGN.md, "Generation".

   Structure follows the in-repo precedent of
   testsuite/tests/comprehensions/quickcheck_lists_arrays_haskell_python.ml: a
   type-directed generator that only produces well-typed terms. Expressions are
   built directly as the compiler's own untyped AST ([Parsetree], via
   [Ast_helper]; see [Expr]) and printed with [Pprintast], so there is no
   hand-written printer to maintain and new syntax comes with printing for free.
   A single program is emitted, carrying both annotations on the function under
   test (see [Sample]).

   Types ([Ty]) mirror the compiler's split between type *expressions* and type
   *declarations*: records and variants are nominal ([Ty.Constr] refers to them
   by name, like [Ptyp_constr]), and their structure lives in declarations
   ([Ty.Decl], like [Ptype_record] / [Ptype_variant] in [type_kind]). Every
   generated program gets its own randomly generated declarations ([Ty.Tenv]),
   printed as the prelude. Special layouts are not hard-coded but emerge from
   the generated structure: a record whose fields are all [float] is a flat
   float record, a variant whose constructors are all nullary is immediate, a
   self-referential variant is a list/tree.

   Tier A: construction -- tuples, records, variants, lists, arrays -- plain vs
   [exclave_]-wrapped, plus mode-crossing immediates, glued together with
   integer/float arithmetic, comparisons, [let], and [if].

   Tier B: closures, captures, and nesting -- arrow types can be [let]-bound and
   returned, and lambda bodies freely reference enclosing bindings.

   Tier C: application of in-scope functions, partial application, and labeled /
   optional arguments (optionals both supplied and omitted; labeled and optional
   parameters may be skipped and supplied later or passed out of order, since
   they commute). Following the compiler's own representation, types stay
   curried and binary ([Ty.Arrow], one labeled arrow per parameter) while
   expressions carry the arity ([Expr.Lam] binds a list of parameters), so the
   same arrow chain can be realized either as one n-ary function or as nested
   unary closures -- same type, different allocation behavior. Argument labels
   live in the type ([Arg_label]) and come from a small shared pool; binder
   names are separate and always fresh, so shadowing is impossible.

   Per DESIGN.md, the generator does not predict allocation (the oracles decide
   ground truth); [Mode] only steers the distribution: [Soundness] biases toward
   constructs the frontend is likely to accept (immediates, [exclave_]),
   [Completeness] toward non-allocating computation the frontend may
   conservatively reject (arithmetic, comparisons, branching). *)

module Mode = struct
  type t =
    | Soundness (* bias toward programs the frontend accepts *)
    | Completeness
  (* bias toward non-allocating but conservatively-rejected forms *)

  let to_string = function
    | Soundness -> "soundness"
    | Completeness -> "completeness"

  let of_string = function
    | "soundness" -> Some Soundness
    | "completeness" -> Some Completeness
    | _ -> None
end

module Sample = struct
  type t =
    { prelude_source : string;
          (* the prelude alone (type declarations + values / functions), for the
             oracle's gate compile *)
      source : string (* the full program: prelude + main function *)
    }

  let to_string { prelude_source = _; source } = source
end

module Arg_label = struct
  type t = Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  let equal a b =
    match a, b with
    | Nolabel, Nolabel -> true
    | Labelled l1, Labelled l2 | Optional l1, Optional l2 -> String.equal l1 l2
    | (Nolabel | Labelled _ | Optional _), _ -> false
end

(* Identifiers may be paths ("M0.p3", "M0.t2"): dots become [Longident.Ldot], so
   module members print correctly. Operator names ([+.], [~-.], ...) also
   contain dots but are NOT paths; only split when every dot-separated component
   is an ordinary identifier. *)
let lid_of_string s =
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let components = String.split_on_char '.' s in
  let is_path =
    List.for_all
      (fun part -> String.length part > 0 && String.for_all is_ident_char part)
      components
  in
  match components with
  | first :: (_ :: _ as rest) when is_path ->
    List.fold_left
      (fun acc part ->
        Longident.Ldot (Location.mknoloc acc, Location.mknoloc part))
      (Longident.Lident first) rest
  | _ -> Longident.Lident s

module Ty = struct
  module Constant = struct
    type t =
      | Int
      | Float
      | Bool
    (* CR-soon shsong: extend to support Char, Int64, and unboxed layouts
       (float#, int64#) *)

    let equal a b =
      match a, b with
      | Int, Int | Float, Float | Bool, Bool -> true
      | (Int | Float | Bool), _ -> false

    let name = function Int -> "int" | Float -> "float" | Bool -> "bool"
  end

  (* Identifier (also the source name) of a user-declared nominal type. *)
  module Id = struct
    type t = string

    let equal = String.equal

    let name t = t
  end

  type t =
    | Constant of Constant.t
    | Tuple of t list (* invariant: at least two components *)
    | List of t
    | Array of t
    | Constr of Id.t (* a declared record / variant type, by name *)
    | Arrow of
        { label : Arg_label.t;
          arg : t;
          ret : t
        }

  let rec equal t1 t2 =
    match t1, t2 with
    | Constant c1, Constant c2 -> Constant.equal c1 c2
    | Tuple l1, Tuple l2 -> List.equal equal l1 l2
    | List a, List b | Array a, Array b -> equal a b
    | Constr i1, Constr i2 -> Id.equal i1 i2
    | Arrow a1, Arrow a2 ->
      Arg_label.equal a1.label a2.label
      && equal a1.arg a2.arg && equal a1.ret a2.ret
    | (Constant _ | Tuple _ | List _ | Array _ | Constr _ | Arrow _), _ -> false

  let rec to_core_type ty =
    let constr name args =
      Ast_helper.Typ.constr (Location.mknoloc (lid_of_string name)) args
    in
    match ty with
    | Constant c -> constr (Constant.name c) []
    | Tuple ts ->
      Ast_helper.Typ.tuple (List.map (fun t -> None, to_core_type t) ts)
    | List t -> constr "list" [to_core_type t]
    | Array t -> constr "array" [to_core_type t]
    | Constr id -> constr (Id.name id) []
    | Arrow { label; arg; ret } ->
      Ast_helper.Typ.arrow label (to_core_type arg) (to_core_type ret) [] []

  let rec arrows = function
    | Arrow { label; arg; ret } ->
      let params, result = arrows ret in
      (label, arg) :: params, result
    | ty -> [], ty

  (* Rewrite references to a module-local type into its qualified path, for the
     module's outside view (e.g. [t5] -> [M0.t5]). *)
  let rec qualify ~local_id ~qualified_id ty =
    let q = qualify ~local_id ~qualified_id in
    match ty with
    | Constant _ -> ty
    | Tuple ts -> Tuple (List.map q ts)
    | List t -> List (q t)
    | Array t -> Array (q t)
    | Constr id -> if Id.equal id local_id then Constr qualified_id else ty
    | Arrow { label; arg; ret } -> Arrow { label; arg = q arg; ret = q ret }

  let exclave_candidate = function
    | Tuple _ | List _ | Array _ | Constr _ | Arrow _ -> true
    | Constant _ -> false

  (* Type declarations: the structure behind each [Constr]. *)
  module Decl = struct
    type field =
      { f_name : string;
        f_ty : t;
        f_mutable : bool
            (* CR-soon shsong: declared but assignments are not generated yet
               (mutation wants a unit type and sequencing) *)
      }

    type constructor =
      { c_name : string;
        c_args : t list
      }

    type kind =
      | Record of field list (* invariant: non-empty *)
      | Variant of constructor list
        (* invariants: non-empty, and at least one nullary base constructor so
           values are constructible at exhausted fuel *)
      | Abstract of
          { producer : string; (* a val of type [producer_arg -> this type] *)
            producer_arg : t (* a constant, so construction bottoms out *)
          }
    (* a module type hidden by its signature: values can only be obtained by
       calling [producer] (the constructibility invariant generalized) *)

    type decl =
      { id : Id.t;
        kind : kind
      }

    (* The type_kind of a declaration; [None] for [Abstract], which only ever
       appears in signatures. *)
    let to_type_kind kind =
      match kind with
      | Abstract _ -> None
      | Record fields ->
        Some
          (Parsetree.Ptype_record
             (List.map
                (fun { f_name; f_ty; f_mutable } ->
                  Ast_helper.Type.field
                    ~mut:
                      (if f_mutable
                       then Asttypes.Mutable
                       else Asttypes.Immutable)
                    (Location.mknoloc f_name) (to_core_type f_ty))
                fields))
      | Variant constrs ->
        Some
          (Parsetree.Ptype_variant
             (List.map
                (fun { c_name; c_args } ->
                  Ast_helper.Type.constructor
                    ~args:
                      (Parsetree.Pcstr_tuple
                         (List.map
                            (fun t ->
                              Ast_helper.Type.constructor_arg (to_core_type t))
                            c_args))
                    (Location.mknoloc c_name))
                constrs))

    let to_type_declaration { id; kind } =
      match to_type_kind kind with
      | Some kind -> Ast_helper.Type.mk ~kind (Location.mknoloc (Id.name id))
      | None -> Ast_helper.Type.mk (Location.mknoloc (Id.name id))

    let to_structure_item decl =
      Ast_helper.Str.type_ Asttypes.Recursive [to_type_declaration decl]
  end

  (* The per-program type environment, in declaration order. *)
  module Tenv = struct
    type t = Decl.decl list

    let empty = []

    let decls t = t

    let lookup t id =
      (List.find (fun (d : Decl.decl) -> Id.equal d.id id) t).Decl.kind

    let to_structure_items t = List.map Decl.to_structure_item t
  end
end

module Expr = struct
  let mknoloc = Location.mknoloc

  let lid s = mknoloc (lid_of_string s)

  let var x = Ast_helper.Exp.ident (lid x)

  let int_lit n = Ast_helper.Exp.constant (Ast_helper.Const.int n)

  let float_lit f =
    Ast_helper.Exp.constant (Ast_helper.Const.float (Printf.sprintf "%F" f))

  let bool_lit b = Ast_helper.Exp.construct (lid (string_of_bool b)) None

  type binop =
    | Add
    | Sub
    | Mul
    | Land
    | Lor
    | Lxor
    | Lsl
    | Lsr
    | Asr
    | Min
    | Max
    | Fadd
    | Fsub
    | Fmul
    | Fdiv
    | Leq
    | Lt
    | Geq
    | Gt
    | Eq
    | Neq
    | Phys_eq
    | Phys_neq
    | And
    | Or

  let binop_name = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Land -> "land"
    | Lor -> "lor"
    | Lxor -> "lxor"
    | Lsl -> "lsl"
    | Lsr -> "lsr"
    | Asr -> "asr"
    | Min -> "min"
    | Max -> "max"
    | Fadd -> "+."
    | Fsub -> "-."
    | Fmul -> "*."
    | Fdiv -> "/."
    | Leq -> "<="
    | Lt -> "<"
    | Geq -> ">="
    | Gt -> ">"
    | Eq -> "="
    | Neq -> "<>"
    | Phys_eq -> "=="
    | Phys_neq -> "!="
    | And -> "&&"
    | Or -> "||"

  let binop op a b =
    Ast_helper.Exp.apply
      (var (binop_name op))
      [Arg_label.Nolabel, a; Arg_label.Nolabel, b]

  let unop name e = Ast_helper.Exp.apply (var name) [Arg_label.Nolabel, e]

  let if_ c t e = Ast_helper.Exp.ifthenelse c t (Some e)

  let let_ x e1 e2 =
    Ast_helper.Exp.let_ Asttypes.Immutable Asttypes.Nonrecursive
      [Ast_helper.Vb.mk (Ast_helper.Pat.var (mknoloc x)) e1]
      e2

  let tuple es = Ast_helper.Exp.tuple (List.map (fun e -> None, e) es)

  let nil = Ast_helper.Exp.construct (lid "[]") None

  let cons h t = Ast_helper.Exp.construct (lid "::") (Some (tuple [h; t]))

  let array_lit es = Ast_helper.Exp.array Asttypes.Mutable es

  (* Field names are globally unique, so no type annotation is needed for
     disambiguation. *)
  let record fields =
    Ast_helper.Exp.record (List.map (fun (f, e) -> lid f, e) fields) None

  let construct c args =
    Ast_helper.Exp.construct (lid c)
      (match args with [] -> None | [e] -> Some e | es -> Some (tuple es))

  (* The parser's encoding of [exclave_ e]: an application of the
     [%extension.exclave] extension node (see [mkexp_exclave] in parser.mly). *)
  let exclave e =
    Ast_helper.Exp.apply
      (Ast_helper.Exp.mk
         (Parsetree.Pexp_extension
            (mknoloc "extension.exclave", Parsetree.PStr [])))
      [Arg_label.Nolabel, e]

  (* Parameters of a [lam]. Binders are always fresh -- never shadowing, and
     independent of the type-side label. The annotations pin the payload type
     [ty] so inference cannot generalize differently from the generator's
     bookkeeping. *)

  let typed_pat x ty =
    Ast_helper.Pat.constraint_
      (Ast_helper.Pat.var (mknoloc x))
      (Some (Ty.to_core_type ty))
      []

  let param label default pat =
    { Parsetree.pparam_loc = Location.none;
      pparam_desc = Parsetree.Pparam_val (label, default, pat)
    }

  let param_positional x ty = param Arg_label.Nolabel None (typed_pat x ty)

  let param_labelled l x ty = param (Arg_label.Labelled l) None (typed_pat x ty)

  (* The default is mandatory here, so the binder has type [ty] in the body. A
     default-free optional parameter would bind [ty option] instead.

     CR-soon shsong: default-free optional parameters are NOT generated yet: the
     binder would have type [t option], which needs option types in [Ty]. *)
  let param_optional l x ty ~default =
    let default =
      Ast_helper.Exp.constraint_ default (Some (Ty.to_core_type ty)) []
    in
    param (Arg_label.Optional l) (Some default) (Ast_helper.Pat.var (mknoloc x))

  let no_function_constraint =
    { Parsetree.mode_annotations = [];
      ret_mode_annotations = [];
      ret_type_constraint = None
    }

  (* n-ary: arity = length params. Function arity is syntactic since OCaml 5.2,
     so binding an arrow chain as one n-ary [lam] (a single closure) or as
     nested unary [lam]s (a closure allocating an inner closure per partial
     application) are different programs of the same type. *)
  let lam params body =
    Ast_helper.Exp.function_ params no_function_constraint
      (Parsetree.Pfunction_body body)

  let app f args = Ast_helper.Exp.apply f args
end

(* Generation context: PRNG state, fresh-name supplies, and the per-program type
   declarations. The typed *variable* environment is threaded functionally
   through [gen_expr] so bindings scope correctly. *)
type ctx =
  { rng : Random.State.t;
    mutable next_var : int;
    mutable next_ty : int;
    mutable next_field : int;
    mutable next_constr : int;
    mutable next_prelude : int;
    mutable next_module : int;
    mutable tenv : Ty.Tenv.t;
    mode : Mode.t;
    allow_assume : bool
        (* whether the assume lane renders as [@zero_alloc assume]; without it
           the same draws render as checked [@zero_alloc], keeping programs
           seed-for-seed comparable. See [gen_annot]. *)
  }

let fresh_name counter prefix ctx =
  let n = counter ctx in
  Printf.sprintf "%s%d" prefix n

let fresh ctx =
  fresh_name
    (fun ctx ->
      let n = ctx.next_var in
      ctx.next_var <- n + 1;
      n)
    "v" ctx

let fresh_ty_name ctx =
  fresh_name
    (fun ctx ->
      let n = ctx.next_ty in
      ctx.next_ty <- n + 1;
      n)
    "t" ctx

let fresh_field ctx =
  fresh_name
    (fun ctx ->
      let n = ctx.next_field in
      ctx.next_field <- n + 1;
      n)
    "f" ctx

let fresh_constr ctx =
  fresh_name
    (fun ctx ->
      let n = ctx.next_constr in
      ctx.next_constr <- n + 1;
      n)
    "C" ctx

let fresh_prelude ctx =
  fresh_name
    (fun ctx ->
      let n = ctx.next_prelude in
      ctx.next_prelude <- n + 1;
      n)
    "p" ctx

let fresh_module ctx =
  fresh_name
    (fun ctx ->
      let n = ctx.next_module in
      ctx.next_module <- n + 1;
      n)
    "M" ctx

(* Pick among weighted alternatives; entries with weight 0 are disabled. *)
let choose ctx options =
  let options = List.filter (fun (w, _) -> w > 0) options in
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 options in
  let rec pick n = function
    | [] -> assert false
    | (w, f) :: rest -> if n < w then f () else pick (n - w) rest
  in
  pick (Random.State.int ctx.rng total) options

let pick_decl ctx =
  let decls = Ty.Tenv.decls ctx.tenv in
  List.nth decls (Random.State.int ctx.rng (List.length decls))

let component_ty ctx ~self =
  choose ctx
    ([ (3, fun () -> Ty.Constant Ty.Constant.Int);
       (2, fun () -> Ty.Constant Ty.Constant.Float);
       (1, fun () -> Ty.Constant Ty.Constant.Bool) ]
    @ List.map
        (fun (d : Ty.Decl.decl) -> 1, fun () -> Ty.Constr d.id)
        (Ty.Tenv.decls ctx.tenv)
    @ match self with None -> [] | Some id -> [(2, fun () -> Ty.Constr id)])

let gen_record ctx =
  let flavor =
    choose ctx
      [(6, fun () -> `Mixed); (1, fun () -> `All_float); (1, fun () -> `All_int)]
  in
  let n_fields = 1 + Random.State.int ctx.rng 3 in
  Ty.Decl.Record
    (List.init n_fields (fun _ ->
         { Ty.Decl.f_name = fresh_field ctx;
           f_ty =
             (match flavor with
             | `All_float -> Ty.Constant Ty.Constant.Float
             | `All_int -> Ty.Constant Ty.Constant.Int
             | `Mixed -> component_ty ctx ~self:None);
           f_mutable = Random.State.int ctx.rng 4 = 0
         }))

let gen_variant ctx ~id =
  let n_constrs = 1 + Random.State.int ctx.rng 3 in
  let constrs =
    List.init n_constrs (fun _ ->
        let n_args = Random.State.int ctx.rng 3 in
        { Ty.Decl.c_name = fresh_constr ctx;
          c_args = List.init n_args (fun _ -> component_ty ctx ~self:(Some id))
        })
  in
  (* Constructibility invariant: guarantee a nullary base constructor, so
     generation at exhausted fuel (and recursive constructor arguments) can
     always bottom out. *)
  let has_nullary =
    List.exists
      (fun (c : Ty.Decl.constructor) ->
        match c.c_args with [] -> true | _ :: _ -> false)
      constrs
  in
  let constrs =
    if has_nullary
    then constrs
    else { Ty.Decl.c_name = fresh_constr ctx; c_args = [] } :: constrs
  in
  Ty.Decl.Variant constrs

let gen_decls ctx ~max_decls =
  let lo = min 2 max_decls in
  let n = lo + Random.State.int ctx.rng (max_decls - lo + 1) in
  for _ = 1 to n do
    let id = fresh_ty_name ctx in
    let kind =
      choose ctx
        [(1, fun () -> gen_record ctx); (1, fun () -> gen_variant ctx ~id)]
    in
    ctx.tenv <- ctx.tenv @ [{ Ty.Decl.id; kind }]
  done

let fun_ty ctx =
  let shape =
    choose ctx
      [ (3, fun () -> [`P]);
        (1, fun () -> [`L]);
        (2, fun () -> [`P; `P]);
        (1, fun () -> [`L; `P]);
        (1, fun () -> [`P; `L]);
        (1, fun () -> [`L; `L]);
        (2, fun () -> [`O; `P]) ]
  in
  let l_idx = ref 0 in
  let o_idx = ref 0 in
  let labels =
    List.map
      (fun k ->
        match k with
        | `P -> Arg_label.Nolabel
        | `L ->
          let l = [| "la"; "lb" |].(!l_idx) in
          incr l_idx;
          Arg_label.Labelled l
        | `O ->
          let l = [| "oa"; "ob" |].(!o_idx) in
          incr o_idx;
          Arg_label.Optional l)
      shape
  in
  (* CR-soon shsong: It seems that currently a function type can only be an 1 or
     2-ary function returns small_ty. *)
  List.fold_right
    (fun label ret ->
      Ty.Arrow { label; arg = component_ty ctx ~self:None; ret })
    labels
    (component_ty ctx ~self:None)

let goal_ty ctx =
  let tuple () =
    let n = 2 + Random.State.int ctx.rng 2 in
    Ty.Tuple (List.init n (fun _ -> component_ty ctx ~self:None))
  in
  let constr () = Ty.Constr (pick_decl ctx).id in
  match ctx.mode with
  | Mode.Soundness ->
    choose ctx
      [ (3, fun () -> Ty.Constant Ty.Constant.Int);
        (2, fun () -> Ty.Constant Ty.Constant.Float);
        (1, fun () -> Ty.Constant Ty.Constant.Bool);
        4, constr;
        2, tuple;
        (1, fun () -> Ty.List (component_ty ctx ~self:None));
        (1, fun () -> Ty.Array (component_ty ctx ~self:None));
        (* returning a closure is prime soundness bait: the closure (and its
           captures) must be allocated *)
        (2, fun () -> fun_ty ctx) ]
  | Mode.Completeness ->
    choose ctx
      [ (6, fun () -> Ty.Constant Ty.Constant.Int);
        (2, fun () -> Ty.Constant Ty.Constant.Float);
        (3, fun () -> Ty.Constant Ty.Constant.Bool);
        2, constr;
        1, tuple;
        (1, fun () -> Ty.List (component_ty ctx ~self:None));
        (1, fun () -> fun_ty ctx) ]

let int_lit ctx = Expr.int_lit (Random.State.int ctx.rng 13 - 3)

let float_lit ctx =
  Expr.float_lit (float_of_int (Random.State.int ctx.rng 13 - 3) *. 0.5)

let bool_lit ctx = Expr.bool_lit (Random.State.int ctx.rng 2 = 0)

let is_completeness ctx =
  match ctx.mode with Mode.Completeness -> true | Mode.Soundness -> false

let app_candidates env ty =
  List.concat_map
    (fun (x, ty') ->
      match Ty.arrows ty' with
      | [], _ -> []
      | params, result ->
        let n = List.length params in
        let indices = List.init n (fun i -> i) in
        let nth i = List.nth params i in
        let is_positional i =
          match fst (nth i) with
          | Arg_label.Nolabel -> true
          | Arg_label.Labelled _ | Arg_label.Optional _ -> false
        in
        List.filter_map
          (fun mask ->
            let in_set i = mask land (1 lsl i) <> 0 in
            (* no positional parameter may be supplied after a skipped one *)
            let positionals_form_prefix =
              let rec ok seen_skipped = function
                | [] -> true
                | i :: rest ->
                  if in_set i
                  then (not seen_skipped) && ok false rest
                  else ok true rest
              in
              ok false (List.filter is_positional indices)
            in
            if not positionals_form_prefix
            then None
            else begin
              let erased i =
                (match fst (nth i) with
                  | Arg_label.Optional _ -> not (in_set i)
                  | Arg_label.Nolabel | Arg_label.Labelled _ -> false)
                && List.exists
                     (fun j -> j > i && in_set j && is_positional j)
                     indices
              in
              let remaining =
                List.fold_right
                  (fun i acc ->
                    if in_set i || erased i
                    then acc
                    else
                      let label, arg = nth i in
                      Ty.Arrow { label; arg; ret = acc })
                  indices result
              in
              if Ty.equal remaining ty
              then
                Some
                  ( x,
                    List.filter_map
                      (fun i -> if in_set i then Some (nth i) else None)
                      indices )
              else None
            end)
          (List.init ((1 lsl n) - 1) (fun m -> m + 1)))
    env

let rec gen_expr ctx env ty fuel =
  let completeness = is_completeness ctx in
  let deep w = if fuel > 0 then w else 0 in
  (* Productions available at every type. *)
  let matching_vars = List.filter (fun (_, ty') -> Ty.equal ty' ty) env in
  let candidates = app_candidates env ty in
  let generic =
    [ ( (if matching_vars = [] then 0 else 3),
        fun () ->
          (* pick one of the in-scope variables of the goal type *)
          let x, _ =
            List.nth matching_vars
              (Random.State.int ctx.rng (List.length matching_vars))
          in
          Expr.var x );
      (deep 1, fun () -> gen_let_in ctx env ty fuel);
      (deep (if completeness then 3 else 1), fun () -> gen_if ctx env ty fuel);
      ( (if candidates = [] then 0 else deep (if completeness then 2 else 3)),
        fun () -> gen_apply ctx env fuel candidates ) ]
  in
  choose ctx (generic @ structural_productions ctx env ty fuel)

and gen_let_in ctx env ty fuel =
  let bound_ty =
    choose ctx
      [ (4, fun () -> component_ty ctx ~self:None);
        ((if is_completeness ctx then 1 else 2), fun () -> fun_ty ctx) ]
  in
  let x = fresh ctx in
  let e1 = gen_expr ctx env bound_ty (fuel - 1) in
  let e2 = gen_expr ctx ((x, bound_ty) :: env) ty (fuel - 1) in
  Expr.let_ x e1 e2

and gen_if ctx env ty fuel =
  let c = gen_expr ctx env (Ty.Constant Ty.Constant.Bool) (fuel - 1) in
  Expr.if_ c (gen_expr ctx env ty (fuel - 1)) (gen_expr ctx env ty (fuel - 1))

and gen_apply ctx env fuel candidates =
  let f, supplied =
    List.nth candidates (Random.State.int ctx.rng (List.length candidates))
  in
  let args =
    List.map
      (fun ((label : Arg_label.t), arg_ty) ->
        (* Supplying an [Optional] parameter's PAYLOAD is the [~l:e] form, i.e.
           [Labelled] in [Pexp_apply]; an [Optional]-labeled argument would be
           [?l:e], which passes an option. *)
        let label : Arg_label.t =
          match label with
          | Optional l -> Labelled l
          | Nolabel | Labelled _ -> label
        in
        label, gen_expr ctx env arg_ty (fuel - 1))
      supplied
  in
  (* Labeled arguments also commute syntactically: sometimes emit them ahead of
     the positional ones (the relative order of positional arguments is
     preserved either way). *)
  let args =
    if List.length args > 1 && Random.State.int ctx.rng 2 = 0
    then begin
      let labeled, positional =
        List.partition
          (fun ((l : Arg_label.t), _) ->
            match l with Labelled _ | Optional _ -> true | Nolabel -> false)
          args
      in
      labeled @ positional
    end
    else args
  in
  Expr.app (Expr.var f) args

and gen_lambda ctx env ty fuel =
  let all, _result = Ty.arrows ty in
  let total = List.length all in
  let valid k =
    let prefix = List.filteri (fun i _ -> i < k) all in
    let rec ok = function
      | [] -> true
      | ((l : Arg_label.t), _) :: rest -> (
        match l with
        | Optional _ ->
          List.exists
            (fun ((l', _) : Arg_label.t * _) ->
              match l' with Nolabel -> true | Labelled _ | Optional _ -> false)
            rest
          && ok rest
        | Nolabel | Labelled _ -> ok rest)
    in
    ok prefix
  in
  (* k = total is always valid thanks to [fun_ty]'s erasability invariant, so
     [ks] is never empty. *)
  let ks = List.filter valid (List.init total (fun i -> i + 1)) in
  let k = List.nth ks (Random.State.int ctx.rng (List.length ks)) in
  let prefix = List.filteri (fun i _ -> i < k) all in
  let rec ret_after k chain =
    if k = 0
    then chain
    else
      match chain with
      | Ty.Arrow { ret; _ } -> ret_after (k - 1) ret
      | _ -> assert false
  in
  let rev_params, env' =
    List.fold_left
      (fun (params, env) ((label : Arg_label.t), arg_ty) ->
        let x = fresh ctx in
        let param =
          match label with
          | Nolabel -> Expr.param_positional x arg_ty
          | Labelled l -> Expr.param_labelled l x arg_ty
          | Optional l ->
            (* always with a default for now; see [Expr.param_optional] *)
            Expr.param_optional l x arg_ty
              ~default:(gen_expr ctx env arg_ty (fuel - 1))
        in
        param :: params, (x, arg_ty) :: env)
      ([], env) prefix
  in
  Expr.lam (List.rev rev_params) (gen_expr ctx env' (ret_after k ty) (fuel - 1))

(* Productions specific to the goal type. *)
and structural_productions ctx env ty fuel =
  let completeness = is_completeness ctx in
  let deep w = if fuel > 0 then w else 0 in
  match ty with
  | Ty.Constant Ty.Constant.Int ->
    (* One arithmetic production; the operator is picked uniformly inside so
       adding operators does not inflate the arithmetic-vs-other balance. *)
    let ops = Expr.[Add; Sub; Mul; Land; Lor; Lxor; Lsl; Lsr; Asr; Min; Max] in
    [ (2, fun () -> int_lit ctx);
      ( deep (if completeness then 4 else 2),
        fun () ->
          let op = List.nth ops (Random.State.int ctx.rng (List.length ops)) in
          Expr.binop op
            (gen_expr ctx env ty (fuel - 1))
            (gen_expr ctx env ty (fuel - 1)) );
      (deep 1, fun () -> Expr.unop "~-" (gen_expr ctx env ty (fuel - 1))) ]
  | Ty.Constant Ty.Constant.Float ->
    let ops = Expr.[Fadd; Fsub; Fmul; Fdiv] in
    [ (2, fun () -> float_lit ctx);
      ( deep 2,
        fun () ->
          let op = List.nth ops (Random.State.int ctx.rng (List.length ops)) in
          Expr.binop op
            (gen_expr ctx env ty (fuel - 1))
            (gen_expr ctx env ty (fuel - 1)) );
      (deep 1, fun () -> Expr.unop "~-." (gen_expr ctx env ty (fuel - 1))) ]
  | Ty.Constant Ty.Constant.Bool ->
    let cmp_ops = Expr.[Leq; Lt; Geq; Gt; Eq; Neq; Phys_eq; Phys_neq] in
    let bool_ops = Expr.[And; Or] in
    [ (2, fun () -> bool_lit ctx);
      ( deep (if completeness then 4 else 2),
        fun () ->
          let op =
            List.nth cmp_ops (Random.State.int ctx.rng (List.length cmp_ops))
          in
          let arg_ty =
            if Random.State.int ctx.rng 2 = 0
            then Ty.Constant Ty.Constant.Int
            else Ty.Constant Ty.Constant.Float
          in
          Expr.binop op
            (gen_expr ctx env arg_ty (fuel - 1))
            (gen_expr ctx env arg_ty (fuel - 1)) );
      ( deep 2,
        fun () ->
          let op =
            List.nth bool_ops (Random.State.int ctx.rng (List.length bool_ops))
          in
          Expr.binop op
            (gen_expr ctx env ty (fuel - 1))
            (gen_expr ctx env ty (fuel - 1)) );
      (deep 1, fun () -> Expr.unop "not" (gen_expr ctx env ty (fuel - 1))) ]
  | Ty.Tuple ts ->
    [ ( 4,
        fun () ->
          Expr.tuple (List.map (fun t -> gen_expr ctx env t (fuel - 1)) ts) ) ]
  | Ty.List t ->
    [ (2, fun () -> Expr.nil);
      ( deep 3,
        fun () ->
          Expr.cons
            (gen_expr ctx env t (fuel - 1))
            (gen_expr ctx env ty (fuel - 1)) ) ]
  | Ty.Array t ->
    [ ( 3,
        fun () ->
          let n = Random.State.int ctx.rng 4 in
          Expr.array_lit (List.init n (fun _ -> gen_expr ctx env t (fuel - 1)))
      ) ]
  | Ty.Constr id -> (
    match Ty.Tenv.lookup ctx.tenv id with
    | Ty.Decl.Abstract { producer; producer_arg } ->
      (* An abstract module type: the only construction is calling its producer.
         Not [deep]-gated (it is this type's base case, like a nullary
         constructor); [producer_arg] is a constant, so the argument bottoms
         out. *)
      [ ( 4,
          fun () ->
            Expr.app (Expr.var producer)
              [Arg_label.Nolabel, gen_expr ctx env producer_arg (fuel - 1)] ) ]
    | Ty.Decl.Record fields ->
      [ ( 4,
          fun () ->
            Expr.record
              (List.map
                 (fun (f : Ty.Decl.field) ->
                   f.f_name, gen_expr ctx env f.f_ty (fuel - 1))
                 fields) ) ]
    | Ty.Decl.Variant constrs ->
      (* One production per constructor. Soundness mode favors nullary
         (immediate, mode-crossing) constructors; non-nullary ones recurse and
         are fuel-gated, bottoming out at the guaranteed nullary base
         constructor. *)
      List.map
        (fun (c : Ty.Decl.constructor) ->
          match c.c_args with
          | [] ->
            ( (if completeness then 2 else 4),
              fun () -> Expr.construct c.c_name [] )
          | args ->
            ( deep 2,
              fun () ->
                Expr.construct c.c_name
                  (List.map (fun t -> gen_expr ctx env t (fuel - 1)) args) ))
        constrs)
  (* Not [deep]-gated: like construction, a lambda must be available at
     exhausted fuel (its body then bottoms out in leaves, since [fun_ty]
     components are [component_ty]). *)
  | Ty.Arrow _ -> [(4, fun () -> gen_lambda ctx env ty fuel)]

(* CR-soon shsong: [exclave_] is only placed at the very top of the body (a tail
   position by construction). Extend to tail positions of [if]/[let]. *)
let maybe_exclave ctx ty body =
  match ctx.mode with
  | Mode.Soundness
    when Ty.exclave_candidate ty && Random.State.int ctx.rng 3 = 0 ->
    Expr.exclave body
  | Mode.Soundness | Mode.Completeness -> body

let initial_fuel = 5

(* ---- Prelude values and functions (milestone 1 of the prelude plan) ----

   Top-level items the main function can reference; they enter the generation
   environment, so [app_candidates] calls prelude functions with no extra logic.
   Bodies reuse [gen_expr] at small fuel. Items may only reference strictly
   earlier items, like type declarations. *)

let prelude_fuel = 2

(* Annotation lanes for prelude functions: - [No_annot]: arbitrary body; calls
   to it are BE-rejecting by construction (no annotation for the backend to
   trust) -- contrast material. - checked [Zero_alloc] / [Zero_alloc_strict] and
   [Mode_noalloc_strict]: arbitrary bodies; the oracle's prelude gate CLASSIFIES
   inconsistent annotation/body pairs as [Prelude_reject] rather than filtering
   them silently. - [Zero_alloc_assume]: arbitrary body that the backend trusts
   UNCHECKED -- the deliberate "whitewashing" instrument. The lane is always
   drawn, but without [ctx.allow_assume] (the [-allow-assume] flag, default off)
   it degrades to checked [Zero_alloc]: the PRNG draw sequence is thus identical
   in both configurations, so a seed generates the same program modulo the
   [assume] keyword -- seed-for-seed comparable experiment pairs. Rationale for
   the flag: once the frontend exempts zero_alloc functions, a suspect caused by
   a wrong assumption implicates the assumption, not the mode axis, so
   assume-enabled runs need separate interpretation. *)
type prelude_annot =
  | No_annot
  | Zero_alloc
  | Zero_alloc_strict
  | Zero_alloc_assume
  | Mode_noalloc_strict

let gen_annot ctx =
  choose ctx
    [ (4, fun () -> No_annot);
      (2, fun () -> Zero_alloc);
      (1, fun () -> Zero_alloc_strict);
      (2, fun () -> Mode_noalloc_strict);
      (2, fun () -> if ctx.allow_assume then Zero_alloc_assume else Zero_alloc)
    ]

let zero_alloc_attr payload =
  Ast_helper.Attr.mk
    (Location.mknoloc "zero_alloc")
    (match payload with
    | None -> Parsetree.PStr []
    | Some word ->
      Parsetree.PStr
        [ Ast_helper.Str.eval
            (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident word))) ])

let prelude_binding ~annot name expr =
  let attrs =
    match annot with
    | No_annot | Mode_noalloc_strict -> []
    | Zero_alloc -> [zero_alloc_attr None]
    | Zero_alloc_strict -> [zero_alloc_attr (Some "strict")]
    | Zero_alloc_assume -> [zero_alloc_attr (Some "assume")]
  in
  let modes =
    match annot with
    | Mode_noalloc_strict -> [Location.mknoloc (Parsetree.Mode "noalloc_strict")]
    | No_annot | Zero_alloc | Zero_alloc_strict | Zero_alloc_assume -> []
  in
  Ast_helper.Str.value Asttypes.Nonrecursive
    [ Ast_helper.Vb.mk ~attrs ~modes
        (Ast_helper.Pat.var (Location.mknoloc name))
        expr ]

(* A top-level constant. Unannotated for now. *)
let gen_prelude_value ctx env name =
  let ty = component_ty ctx ~self:None in
  let body = gen_expr ctx env ty prelude_fuel in
  prelude_binding ~annot:No_annot name body, ty

(* A top-level function. Annotated lanes force a syntactic lambda (the
   annotations attach to a function definition); the unannotated lane may
   produce any function-typed expression, e.g. an alias or partial application
   of an earlier prelude function. *)
let gen_prelude_function ctx env name =
  let annot = gen_annot ctx in
  let ty = fun_ty ctx in
  let body =
    match annot with
    | No_annot -> gen_expr ctx env ty prelude_fuel
    | Zero_alloc | Zero_alloc_strict | Zero_alloc_assume | Mode_noalloc_strict
      ->
      gen_lambda ctx env ty prelude_fuel
  in
  prelude_binding ~annot name body, ty

(* The closure-factory pattern, probing "whitewashed" allocations
   ([let[@zero_alloc] f () = let g () = <alloc> in g]): zero_alloc is a
   first-order contract about [f]'s own execution, so allocations under the
   returned [g] do not count against [f] -- but they escape to whoever calls
   [g]. Whether [g] may capture [f]'s parameter is a coin flip: a non-capturing
   [g] is statically allocated (so [f] can genuinely pass a checked zero_alloc),
   while a capturing one allocates a closure when [f] runs. [g] is returned
   either directly or tucked inside a tuple. *)
let gen_prelude_factory ctx env name =
  let annot = gen_annot ctx in
  let p = fresh ctx in
  let p_ty = component_ty ctx ~self:None in
  let g_arg = fresh ctx in
  let g_arg_ty = component_ty ctx ~self:None in
  let g_ret_ty = component_ty ctx ~self:None in
  let may_capture = Random.State.int ctx.rng 2 = 0 in
  let inner_env =
    (g_arg, g_arg_ty) :: (if may_capture then (p, p_ty) :: env else env)
  in
  let g_body = gen_expr ctx inner_env g_ret_ty prelude_fuel in
  let g_ty =
    Ty.Arrow { label = Arg_label.Nolabel; arg = g_arg_ty; ret = g_ret_ty }
  in
  let g_name = fresh ctx in
  let g_lam = Expr.lam [Expr.param_positional g_arg g_arg_ty] g_body in
  let wrap_ty, wrap =
    choose ctx
      [ (2, fun () -> g_ty, Expr.var g_name);
        ( 1,
          fun () ->
            ( Ty.Tuple [g_ty; Ty.Constant Ty.Constant.Int],
              Expr.tuple [Expr.var g_name; Expr.int_lit 0] ) ) ]
  in
  let body = Expr.let_ g_name g_lam wrap in
  let f_lam = Expr.lam [Expr.param_positional p p_ty] body in
  let f_ty =
    Ty.Arrow { label = Arg_label.Nolabel; arg = p_ty; ret = wrap_ty }
  in
  prelude_binding ~annot name f_lam, f_ty

(* A module with a derived, ascribed signature (milestone 2 of the prelude
   plan): [module M0 : sig ... end = struct ... end].

   The struct is generated first and the signature is DERIVED from it, so
   "signature matches struct" holds by construction: - one local type
   declaration, kept transparent in the signature or made abstract (a fresh
   mode-analysis boundary: outside, nothing is known about its layout); - a
   producer [val pN : int -> tK], always exported -- the constructibility
   invariant generalized: when [tK] is abstract, calling the producer is the
   only way to build a value (registered as [Ty.Decl.Abstract]); - optionally a
   consumer [val pN : tK -> int]; - 0..2 further function members via the
   ordinary lanes ([gen_annot]); members may be hidden from the signature, and
   exported ones may carry an allocation MODALITY ([@@ noalloc] / [@@
   noalloc_strict]) -- the signature may thus strengthen, weaken, or hide the
   mode information of the definition underneath, which is exactly the
   ascription surface we want the frontend checked on (inconsistencies are
   caught by the prelude gate).

   The module's exported members enter the environment under qualified names
   ("M0.p3"), and its type -- qualified as "M0.tK" -- is registered in
   [ctx.tenv], so later items and the main function use both with no extra
   logic. *)
let gen_module_modality ctx =
  choose ctx
    [ (3, fun () -> None);
      (1, fun () -> Some "noalloc");
      (2, fun () -> Some "noalloc_strict") ]

(* A module whose abstract type is secretly a FUNCTION type:

   [module M : sig type t val p : int -> t val c : t -> r end = struct type t =
   <arrow> let p (x : int) = <expr of arrow type> ... end]

   This is the "whitewashed allocating closure" scenario: the producer (which
   may carry any annotation lane, including a checked or assumed zero_alloc)
   returns a possibly-allocating function hidden behind the abstraction, and the
   consumer takes a [t] and CALLS it -- so the main function can route an
   allocation through [M.c (M.p e)] without any function type appearing in its
   own interface. The alias is always abstract in the signature (a transparent
   [type t = int -> int] would make [t] and the arrow interchangeable and adds
   nothing); the consumer is therefore always exported, since it is the only way
   the hidden function can be applied. *)
let gen_fun_alias_module ctx env name =
  let local_id = fresh_ty_name ctx in
  let qualified_id = name ^ "." ^ local_id in
  let underlying = fun_ty ctx in
  let params, result = Ty.arrows underlying in
  (* struct: type tK = <underlying> *)
  let alias_item =
    Ast_helper.Str.type_ Asttypes.Recursive
      [ Ast_helper.Type.mk
          ~manifest:(Ty.to_core_type underlying)
          (Location.mknoloc local_id) ]
  in
  (* producer: val pN : int -> tK, body generated at the underlying arrow type
     (the alias makes them equal inside the struct) *)
  let producer_name = fresh_prelude ctx in
  let producer_arg = Ty.Constant Ty.Constant.Int in
  let producer_item =
    let annot = gen_annot ctx in
    let x = fresh ctx in
    prelude_binding ~annot producer_name
      (Expr.lam
         [Expr.param_positional x producer_arg]
         (gen_expr ctx ((x, producer_arg) :: env) underlying prelude_fuel))
  in
  (* consumer: val cN : tK -> <result>, body fully applies the hidden
     function *)
  let consumer_name = fresh_prelude ctx in
  let consumer_item =
    let x = fresh ctx in
    let args =
      List.map
        (fun ((label : Arg_label.t), arg_ty) ->
          let label : Arg_label.t =
            match label with
            | Optional l -> Labelled l
            | Nolabel | Labelled _ -> label
          in
          label, gen_expr ctx env arg_ty (prelude_fuel - 1))
        params
    in
    prelude_binding ~annot:No_annot consumer_name
      (Expr.lam
         [Expr.param_positional x (Ty.Constr local_id)]
         (Expr.app (Expr.var x) args))
  in
  let modality_of ctx =
    match gen_module_modality ctx with
    | None -> []
    | Some m -> [Location.mknoloc (Parsetree.Modality m)]
  in
  let sig_items =
    [ Ast_helper.Sig.type_ Asttypes.Recursive
        [Ast_helper.Type.mk (Location.mknoloc local_id)];
      Ast_helper.Sig.value
        (Ast_helper.Val.mk ~modalities:(modality_of ctx)
           (Location.mknoloc producer_name)
           (Ty.to_core_type
              (Ty.Arrow
                 { label = Arg_label.Nolabel;
                   arg = producer_arg;
                   ret = Ty.Constr local_id
                 })));
      Ast_helper.Sig.value
        (Ast_helper.Val.mk ~modalities:(modality_of ctx)
           (Location.mknoloc consumer_name)
           (Ty.to_core_type
              (Ty.Arrow
                 { label = Arg_label.Nolabel;
                   arg = Ty.Constr local_id;
                   ret = result
                 }))) ]
  in
  let item =
    Ast_helper.Str.module_
      (Ast_helper.Mb.mk
         (Location.mknoloc (Some name))
         (Ast_helper.Mod.constraint_
            (Some (Ast_helper.Mty.signature (Ast_helper.Sg.mk sig_items)))
            []
            (Ast_helper.Mod.structure
               [alias_item; producer_item; consumer_item])))
  in
  (* Outside view: abstract, produced only via the producer. [result] cannot
     mention [tK] ([underlying] is built from [component_ty], which never sees
     the alias), so only the producer / consumer types need qualifying. *)
  ctx.tenv
    <- ctx.tenv
       @ [ { Ty.Decl.id = qualified_id;
             kind =
               Ty.Decl.Abstract
                 { producer = name ^ "." ^ producer_name; producer_arg }
           } ];
  let exported_env =
    [ ( name ^ "." ^ producer_name,
        Ty.Arrow
          { label = Arg_label.Nolabel;
            arg = producer_arg;
            ret = Ty.Constr qualified_id
          } );
      ( name ^ "." ^ consumer_name,
        Ty.Arrow
          { label = Arg_label.Nolabel;
            arg = Ty.Constr qualified_id;
            ret = result
          } ) ]
  in
  item, exported_env

let gen_data_module ctx env name =
  let local_id = fresh_ty_name ctx in
  let qualified_id = name ^ "." ^ local_id in
  let kind =
    choose ctx
      [ (1, fun () -> gen_record ctx);
        (1, fun () -> gen_variant ctx ~id:local_id) ]
  in
  let local_decl = { Ty.Decl.id = local_id; kind } in
  (* While generating member bodies, the local type is in scope unqualified. *)
  let outer_tenv = ctx.tenv in
  ctx.tenv <- ctx.tenv @ [local_decl];
  let local_ty = Ty.Constr local_id in
  let qualify = Ty.qualify ~local_id ~qualified_id in
  (* Producer: always present and always exported. *)
  let producer_name = fresh_prelude ctx in
  let producer_arg = Ty.Constant Ty.Constant.Int in
  let producer_ty =
    Ty.Arrow { label = Arg_label.Nolabel; arg = producer_arg; ret = local_ty }
  in
  let producer_item =
    let x = fresh ctx in
    prelude_binding ~annot:No_annot producer_name
      (Expr.lam
         [Expr.param_positional x producer_arg]
         (gen_expr ctx [x, producer_arg] local_ty prelude_fuel))
  in
  (* Optional consumer [tK -> int]. *)
  let consumer =
    if Random.State.int ctx.rng 2 = 0
    then begin
      let cname = fresh_prelude ctx in
      let cty =
        Ty.Arrow
          { label = Arg_label.Nolabel;
            arg = local_ty;
            ret = Ty.Constant Ty.Constant.Int
          }
      in
      let x = fresh ctx in
      let item =
        prelude_binding ~annot:No_annot cname
          (Expr.lam
             [Expr.param_positional x local_ty]
             (gen_expr ctx ((x, local_ty) :: env) (Ty.Constant Ty.Constant.Int)
                prelude_fuel))
      in
      [cname, cty, item]
    end
    else []
  in
  (* Further ordinary function members. *)
  let extras =
    List.init (Random.State.int ctx.rng 3) (fun _ ->
        let mname = fresh_prelude ctx in
        let item, ty = gen_prelude_function ctx env mname in
        mname, ty, item)
  in
  let members =
    (producer_name, producer_ty, producer_item) :: (consumer @ extras)
  in
  (* Derive the signature. The type is transparent or abstract; the producer is
     always exported; other members are exported (possibly with a modality) or
     hidden. *)
  let abstract = Random.State.int ctx.rng 3 = 0 in
  let type_sig_item =
    let decl =
      if abstract
      then
        { Ty.Decl.id = local_id;
          kind = Ty.Decl.Abstract { producer = ""; producer_arg }
        }
      else local_decl
    in
    Ast_helper.Sig.type_ Asttypes.Recursive [Ty.Decl.to_type_declaration decl]
  in
  let exported =
    List.filter_map
      (fun (mname, ty, _item) ->
        let is_producer = String.equal mname producer_name in
        if (not is_producer) && Random.State.int ctx.rng 4 = 0
        then None (* hidden *)
        else begin
          let modalities =
            match gen_module_modality ctx with
            | None -> []
            | Some m -> [Location.mknoloc (Parsetree.Modality m)]
          in
          Some
            ( mname,
              ty,
              Ast_helper.Sig.value
                (Ast_helper.Val.mk ~modalities (Location.mknoloc mname)
                   (Ty.to_core_type ty)) )
        end)
      members
  in
  let sig_items = type_sig_item :: List.map (fun (_, _, s) -> s) exported in
  let struct_items =
    Ty.Decl.to_structure_item local_decl
    :: List.map (fun (_, _, i) -> i) members
  in
  let item =
    Ast_helper.Str.module_
      (Ast_helper.Mb.mk
         (Location.mknoloc (Some name))
         (Ast_helper.Mod.constraint_
            (Some (Ast_helper.Mty.signature (Ast_helper.Sg.mk sig_items)))
            []
            (Ast_helper.Mod.structure struct_items)))
  in
  (* Outside view: the local type leaves scope; register the qualified type in
     [ctx.tenv] and return the exported members under qualified names. *)
  let registered_kind =
    if abstract
    then
      Ty.Decl.Abstract { producer = name ^ "." ^ producer_name; producer_arg }
    else
      begin match kind with
      | Ty.Decl.Record fields ->
        Ty.Decl.Record
          (List.map
             (fun (f : Ty.Decl.field) ->
               { f with f_name = name ^ "." ^ f.f_name; f_ty = qualify f.f_ty })
             fields)
      | Ty.Decl.Variant constrs ->
        Ty.Decl.Variant
          (List.map
             (fun (c : Ty.Decl.constructor) ->
               { Ty.Decl.c_name = name ^ "." ^ c.c_name;
                 c_args = List.map qualify c.c_args
               })
             constrs)
      | Ty.Decl.Abstract _ -> assert false
      end
  in
  ctx.tenv
    <- outer_tenv @ [{ Ty.Decl.id = qualified_id; kind = registered_kind }];
  let exported_env =
    List.map (fun (mname, ty, _) -> name ^ "." ^ mname, qualify ty) exported
  in
  item, exported_env

let gen_prelude_module ctx env name =
  choose ctx
    [ (2, fun () -> gen_data_module ctx env name);
      (1, fun () -> gen_fun_alias_module ctx env name) ]

(* Generate 0..3 prelude items, each seeing only the strictly earlier ones.
   Returns the structure items and the (name, type) environment they
   contribute. *)
let gen_prelude_items ctx =
  let n = Random.State.int ctx.rng 4 in
  let rec go i env rev_items =
    if i = n
    then List.rev rev_items, List.rev env
    else begin
      let entries, item =
        choose ctx
          [ ( 1,
              fun () ->
                let name = fresh_prelude ctx in
                let item, ty = gen_prelude_value ctx env name in
                [name, ty], item );
            ( 3,
              fun () ->
                let name = fresh_prelude ctx in
                let item, ty = gen_prelude_function ctx env name in
                [name, ty], item );
            ( 2,
              fun () ->
                let name = fresh_prelude ctx in
                let item, ty = gen_prelude_factory ctx env name in
                [name, ty], item );
            ( 2,
              fun () ->
                let name = fresh_module ctx in
                let item, exported = gen_prelude_module ctx env name in
                exported, item ) ]
      in
      go (i + 1) (List.rev_append entries env) (item :: rev_items)
    end
  in
  go 0 [] []

let generate ~max_decls ~allow_assume ~mode ~seed =
  let ctx =
    { rng = Random.State.make [| seed |];
      next_var = 0;
      next_ty = 0;
      next_field = 0;
      next_constr = 0;
      next_prelude = 0;
      next_module = 0;
      tenv = Ty.Tenv.empty;
      mode;
      allow_assume
    }
  in
  gen_decls ctx ~max_decls;
  (* Snapshot the top-level type declarations now: [gen_prelude_items] also
     registers module types (qualified, possibly [Abstract]) in [ctx.tenv] for
     the benefit of later generation, and those must not be printed as top-level
     items. *)
  let type_items = Ty.Tenv.to_structure_items ctx.tenv in
  let prelude_items, prelude_env = gen_prelude_items ctx in
  let int_ty = Ty.Constant Ty.Constant.Int in
  let float_ty = Ty.Constant Ty.Constant.Float in
  let env = ["x0", int_ty; "x1", float_ty] @ prelude_env in
  let ty = goal_ty ctx in
  let body = maybe_exclave ctx ty (gen_expr ctx env ty initial_fuel) in
  (* No return-type annotation is emitted since a plain [: ty] could interact
     with mode inference. Both annotations go on the same function: [(f @
     noalloc_strict)] -- which the parser encodes as [pvb_modes] on the value
     binding, with a plain variable pattern -- drives the frontend check (and
     forces the body's allocations local), and the [@zero_alloc strict]
     attribute on the binding makes the backend check exactly that typed
     program. *)
  let binding =
    Ast_helper.Vb.mk
      ~attrs:[zero_alloc_attr (Some "strict")]
      ~modes:[Location.mknoloc (Parsetree.Mode "noalloc_strict")]
      (Ast_helper.Pat.var (Location.mknoloc "f"))
      (Expr.lam
         [Expr.param_positional "x0" int_ty; Expr.param_positional "x1" float_ty]
         body)
  in
  let prelude_structure = type_items @ prelude_items in
  let structure =
    prelude_structure @ [Ast_helper.Str.value Asttypes.Nonrecursive [binding]]
  in
  let header =
    Printf.sprintf "(* goal: %s *)\n"
      (Format.asprintf "%a" Pprintast.core_type (Ty.to_core_type ty))
  in
  let prelude_source =
    Format.asprintf "%a@." Pprintast.structure prelude_structure
  in
  let source = header ^ Format.asprintf "%a@." Pprintast.structure structure in
  { Sample.prelude_source; source }
