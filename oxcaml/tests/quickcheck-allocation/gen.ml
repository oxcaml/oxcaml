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
  type t = { source : string }

  let to_string { source } = source
end

module Arg_label = struct
  type t = Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  (* CR shsong: Can this be replaced by some existing function in the
     compiler? *)
  let equal a b =
    match a, b with
    | Nolabel, Nolabel -> true
    | Labelled l1, Labelled l2 | Optional l1, Optional l2 -> String.equal l1 l2
    | (Nolabel | Labelled _ | Optional _), _ -> false
end

module Ty = struct
  module Constant = struct
    type t =
      | Int
      | Float
    (* CR-soon shsong: extend to support Char, Int64, and unboxed layouts
       (float#, int64#) *)

    let equal a b =
      match a, b with
      | Int, Int | Float, Float -> true
      | (Int | Float), _ -> false

    let name = function Int -> "int" | Float -> "float"
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
      Ast_helper.Typ.constr (Location.mknoloc (Longident.Lident name)) args
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

    type decl =
      { id : Id.t;
        kind : kind
      }

    let to_structure_item { id; kind } =
      let kind =
        match kind with
        | Record fields ->
          Parsetree.Ptype_record
            (List.map
               (fun { f_name; f_ty; f_mutable } ->
                 Ast_helper.Type.field
                   ~mut:
                     (if f_mutable then Asttypes.Mutable else Asttypes.Immutable)
                   (Location.mknoloc f_name) (to_core_type f_ty))
               fields)
        | Variant constrs ->
          Parsetree.Ptype_variant
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
               constrs)
      in
      Ast_helper.Str.type_ Asttypes.Recursive
        [Ast_helper.Type.mk ~kind (Location.mknoloc (Id.name id))]
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

(* Expression builders over the compiler's own untyped AST: every builder
   returns a [Parsetree.expression], constructed with [Ast_helper] and printed
   with [Pprintast]. Well-typedness is still the generator's job ([gen_expr]);
   this module only knows how to build each node. *)
module Expr = struct
  let mknoloc = Location.mknoloc

  let lid s = mknoloc (Longident.Lident s)

  let var x = Ast_helper.Exp.ident (lid x)

  let int_lit n = Ast_helper.Exp.constant (Ast_helper.Const.int n)

  let float_lit f =
    Ast_helper.Exp.constant (Ast_helper.Const.float (Printf.sprintf "%F" f))

  (* CR shsong: Consider to make it more comprehensive for completeness test. *)
  type binop =
    | Add
    | Fadd
    | Leq

  let binop_name = function Add -> "+" | Fadd -> "+." | Leq -> "<="

  let binop op a b =
    Ast_helper.Exp.apply
      (var (binop_name op))
      [Arg_label.Nolabel, a; Arg_label.Nolabel, b]

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
    mutable tenv : Ty.Tenv.t;
    mode : Mode.t
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

(* Pick among weighted alternatives; entries with weight 0 are disabled. *)
let choose ctx options =
  let options = List.filter (fun (w, _) -> w > 0) options in
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 options in
  let rec pick n = function
    | [] -> assert false
    | (w, f) :: rest -> if n < w then f () else pick (n - w) rest
  in
  pick (Random.State.int ctx.rng total) options

(* CR shsong: Is it ok to pick decl without deciding which type is desired? *)
let pick_decl ctx =
  let decls = Ty.Tenv.decls ctx.tenv in
  List.nth decls (Random.State.int ctx.rng (List.length decls))

(* Component types usable inside a type declaration: constants, strictly earlier
   declarations (those already in [ctx.tenv] while [gen_decls] is running), and
   -- for variant constructor arguments -- the declaring type itself, which is
   what makes recursive variants (lists, trees) possible. *)
let decl_component_ty ctx ~self =
  let earlier = Ty.Tenv.decls ctx.tenv in
  choose ctx
    ([ (3, fun () -> Ty.Constant Ty.Constant.Int);
       (2, fun () -> Ty.Constant Ty.Constant.Float) ]
    (* CR shsong: Why don't put the entire list of decl here? I feel each of
       them should be placed at an equivalent place to Int and Float. It is ok
       to adjust the probability of selecting of them, but I feel it is super
       wierd to pre-select one and then select among the selected one and Int
       and Float (and maybe self). *)
    @ (if earlier = [] then [] else [(2, fun () -> Ty.Constr (pick_decl ctx).id)])
    @ match self with None -> [] | Some id -> [(2, fun () -> Ty.Constr id)])

let gen_record ctx =
  (* CR shsong: Remove the logic to pick favored mode based on ctx.mode.
     Also increase the favor over Mixed mode. *)
  (* All-float (flat float layout) and all-int (all-immediate fields) records
     are the mode-crossing-adjacent layouts; soundness mode favors them. *)
  let flavor =
    choose ctx
      [ (3, fun () -> `Mixed);
        ( (match ctx.mode with Mode.Soundness -> 2 | Mode.Completeness -> 1),
          fun () -> `All_float );
        (1, fun () -> `All_int) ]
  in
  let n_fields = 1 + Random.State.int ctx.rng 3 in
  Ty.Decl.Record
    (List.init n_fields (fun _ ->
         { Ty.Decl.f_name = fresh_field ctx;
           f_ty =
             (match flavor with
             | `All_float -> Ty.Constant Ty.Constant.Float
             | `All_int -> Ty.Constant Ty.Constant.Int
             | `Mixed -> decl_component_ty ctx ~self:None);
           f_mutable = Random.State.int ctx.rng 4 = 0
         }))

let gen_variant ctx ~id =
  let n_constrs = 1 + Random.State.int ctx.rng 3 in
  let constrs =
    List.init n_constrs (fun _ ->
        let n_args = Random.State.int ctx.rng 3 in
        { Ty.Decl.c_name = fresh_constr ctx;
          c_args =
            List.init n_args (fun _ -> decl_component_ty ctx ~self:(Some id))
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

(* CR shsong: Make the max possible number of declarations as a parameter,
   and expose it to the top level. *)
(* Generate this program's type declarations. Each may reference strictly
   earlier ones (and variants may reference themselves), so every declared type
   is constructible by induction on declaration order, with recursive variants
   bottoming out at their nullary constructor. *)
let gen_decls ctx =
  let n = 2 + Random.State.int ctx.rng 3 in
  for _ = 1 to n do
    let id = fresh_ty_name ctx in
    let kind =
      choose ctx
        [(1, fun () -> gen_record ctx); (1, fun () -> gen_variant ctx ~id)]
    in
    ctx.tenv <- ctx.tenv @ [{ Ty.Decl.id; kind }]
  done

(* CR shsong: This function seems to be very similar to decl_component_ty, where both pick some types.
The only difference is that small_ty do not have self. Consider to merge them. *)
(* Component types for tuples / lists / arrays / function payloads: constants
   and declared types. Declared types are constructible in finitely many steps
   (see [gen_decls]), which preserves the termination argument: composite
   children generated at exhausted fuel still bottom out. *)
let small_ty ctx =
  let decls = Ty.Tenv.decls ctx.tenv in
  choose ctx
    ([ (3, fun () -> Ty.Constant Ty.Constant.Int);
       (2, fun () -> Ty.Constant Ty.Constant.Float) ]
    (* CR shsong: Similar issue: should just append the list of all decl here
       with some favor number. *)
    @ if decls = [] then [] else [(2, fun () -> Ty.Constr (pick_decl ctx).id)])

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
    (fun label ret -> Ty.Arrow { label; arg = small_ty ctx; ret })
    labels (small_ty ctx)

let goal_ty ctx =
  let tuple () =
    let n = 2 + Random.State.int ctx.rng 2 in
    Ty.Tuple (List.init n (fun _ -> small_ty ctx))
  in
  let constr () = Ty.Constr (pick_decl ctx).id in
  match ctx.mode with
  | Mode.Soundness ->
    choose ctx
      [ (3, fun () -> Ty.Constant Ty.Constant.Int);
        (2, fun () -> Ty.Constant Ty.Constant.Float);
        4, constr;
        2, tuple;
        (1, fun () -> Ty.List (small_ty ctx));
        (1, fun () -> Ty.Array (small_ty ctx));
        (* returning a closure is prime soundness bait: the closure (and its
           captures) must be allocated *)
        (2, fun () -> fun_ty ctx) ]
  | Mode.Completeness ->
    choose ctx
      [ (6, fun () -> Ty.Constant Ty.Constant.Int);
        (2, fun () -> Ty.Constant Ty.Constant.Float);
        2, constr;
        1, tuple;
        (1, fun () -> Ty.List (small_ty ctx));
        (1, fun () -> fun_ty ctx) ]

let int_lit ctx = Expr.int_lit (Random.State.int ctx.rng 13 - 3)

let float_lit ctx =
  Expr.float_lit (float_of_int (Random.State.int ctx.rng 13 - 3) *. 0.5)

(* CR shsong: Try to split this very long function into multiple functions.
   Specifically, many functions defined in its function should be able to
   moved out. *)
(* Generate a well-typed expression of type [ty] in environment [env]. Every
   recursive call decreases [fuel]; once fuel is exhausted only non-recursive
   productions (and construction whose children bottom out -- literals, nullary
   constructors, strictly-earlier declarations) remain, so generation
   terminates. *)
let rec gen_expr ctx env ty fuel =
  let completeness =
    match ctx.mode with Mode.Completeness -> true | Mode.Soundness -> false
  in
  let deep w = if fuel > 0 then w else 0 in
  (* Productions available at every type. *)
  let vars = List.filter (fun (_, ty') -> Ty.equal ty' ty) env in
  let use_var () =
    let x, _ = List.nth vars (Random.State.int ctx.rng (List.length vars)) in
    Expr.var x
  in
  let let_in () =
    (* XCR shsong: bound_ty cannot be user-defined type here like a record or a
       variant.

       aide on behalf of shsong: [small_ty] now includes declared record /
       variant types, so [let]-bound values cover them too. *)
    let bound_ty =
      choose ctx
        [ (4, fun () -> small_ty ctx);
          ((if completeness then 1 else 2), fun () -> fun_ty ctx) ]
    in
    let x = fresh ctx in
    let e1 = gen_expr ctx env bound_ty (fuel - 1) in
    let e2 = gen_expr ctx ((x, bound_ty) :: env) ty (fuel - 1) in
    Expr.let_ x e1 e2
  in
  let if_ () =
    let int_ty = Ty.Constant Ty.Constant.Int in
    let c =
      Expr.binop Expr.Leq
        (gen_expr ctx env int_ty (fuel - 1))
        (gen_expr ctx env int_ty (fuel - 1))
    in
    Expr.if_ c (gen_expr ctx env ty (fuel - 1)) (gen_expr ctx env ty (fuel - 1))
  in
  (* Tier C: apply an in-scope function. Argument matching follows OCaml's
     rules, so an application may skip parameters and supply them later: -
     labeled / optional parameters commute: any subset of them may be supplied,
     in any order relative to the rest; - positional parameters cannot be
     reordered: the supplied ones must form a prefix of the function's
     positional parameters; - a skipped [Optional] parameter is erased
     (defaulted) if a positional argument matching a later parameter is
     supplied, and otherwise remains in the result type. A candidate is a
     function variable plus a nonempty parameter subset such that the type
     remaining after the application equals the goal type; supplying fewer than
     all parameters is partial application. Subsets are enumerated by bitmask,
     which is fine while [fun_ty] chains stay short. *)
  let app_candidates =
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
  in
  let apply () =
    let f, supplied =
      List.nth app_candidates
        (Random.State.int ctx.rng (List.length app_candidates))
    in
    let args =
      List.map
        (fun ((label : Arg_label.t), arg_ty) ->
          label, gen_expr ctx env arg_ty (fuel - 1))
        supplied
    in
    (* Labeled arguments also commute syntactically: sometimes emit them ahead
       of the positional ones (the relative order of positional arguments is
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
  in
  let generic =
    [ (if vars = [] then 0 else 3), use_var;
      deep 1, let_in;
      deep (if completeness then 3 else 1), if_;
      ( (if app_candidates = [] then 0 else deep (if completeness then 2 else 3)),
        apply ) ]
  in
  (* Productions specific to the goal type. *)
  let structural =
    match ty with
    | Ty.Constant Ty.Constant.Int ->
      [ (2, fun () -> int_lit ctx);
        ( deep (if completeness then 4 else 2),
          fun () ->
            Expr.binop Expr.Add
              (gen_expr ctx env ty (fuel - 1))
              (gen_expr ctx env ty (fuel - 1)) ) ]
    | Ty.Constant Ty.Constant.Float ->
      [ (2, fun () -> float_lit ctx);
        ( deep 2,
          fun () ->
            Expr.binop Expr.Fadd
              (gen_expr ctx env ty (fuel - 1))
              (gen_expr ctx env ty (fuel - 1)) ) ]
    | Ty.Tuple ts ->
      [ ( 4,
          fun () ->
            Expr.tuple (List.map (fun t -> gen_expr ctx env t (fuel - 1)) ts) )
      ]
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
            Expr.array_lit
              (List.init n (fun _ -> gen_expr ctx env t (fuel - 1))) ) ]
    | Ty.Constr id -> (
      match Ty.Tenv.lookup ctx.tenv id with
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
    (* Tier B: lambdas. Bind a prefix of [k] parameters in this [Lam]; k < chain
       length leaves a function-typed body, i.e. nested closures, while k =
       chain length is the n-ary single-closure form -- same type, different
       allocation behavior, so both are generated. Split points where a bound
       [Optional] parameter is not followed by a bound positional one are
       excluded (the optional would be unerasable within this syntactic
       function: warning 16). Parameter binders are fresh and independent of the
       type-side labels; a default expression may refer to earlier parameters of
       the same lambda. Not [deep]-gated: like construction, a lambda must be
       available at exhausted fuel (its body then bottoms out in leaves, since
       [fun_ty] components are [small_ty]). *)
    | Ty.Arrow _ ->
      [ ( 4,
          fun () ->
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
                        match l' with
                        | Nolabel -> true
                        | Labelled _ | Optional _ -> false)
                      rest
                    && ok rest
                  | Nolabel | Labelled _ -> ok rest)
              in
              ok prefix
            in
            (* k = total is always valid thanks to [fun_ty]'s erasability
               invariant, so [ks] is never empty. *)
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
                      (* always with a default for now; see
                         [Expr.param_optional] *)
                      Expr.param_optional l x arg_ty
                        ~default:(gen_expr ctx env arg_ty (fuel - 1))
                  in
                  param :: params, (x, arg_ty) :: env)
                ([], env) prefix
            in
            Expr.lam (List.rev rev_params)
              (gen_expr ctx env' (ret_after k ty) (fuel - 1)) ) ]
  in
  choose ctx (generic @ structural)

(* CR: [exclave_] is only placed at the very top of the body (a tail position by
   construction). Extend to tail positions of [if]/[let] when Tier B lands. *)
let maybe_exclave ctx ty body =
  match ctx.mode with
  | Mode.Soundness
    when Ty.exclave_candidate ty && Random.State.int ctx.rng 3 = 0 ->
    Expr.exclave body
  | Mode.Soundness | Mode.Completeness -> body

let initial_fuel = 5

let generate ~mode ~seed =
  let ctx =
    { rng = Random.State.make [| seed |];
      next_var = 0;
      next_ty = 0;
      next_field = 0;
      next_constr = 0;
      tenv = Ty.Tenv.empty;
      mode
    }
  in
  gen_decls ctx;
  let int_ty = Ty.Constant Ty.Constant.Int in
  let float_ty = Ty.Constant Ty.Constant.Float in
  let env = ["x0", int_ty; "x1", float_ty] in
  let ty = goal_ty ctx in
  let body = maybe_exclave ctx ty (gen_expr ctx env ty initial_fuel) in
  (* No return-type annotation is emitted since a plain [: ty] could interact
     with mode inference. Both annotations go on the same function: [(f @
     noalloc_strict)] -- which the parser encodes as [pvb_modes] on the value
     binding, with a plain variable pattern -- drives the frontend check (and
     forces the body's allocations local), and the [@zero_alloc strict]
     attribute on the binding makes the backend check exactly that typed
     program. *)
  let zero_alloc =
    Ast_helper.Attr.mk
      (Location.mknoloc "zero_alloc")
      (Parsetree.PStr
         [ Ast_helper.Str.eval
             (Ast_helper.Exp.ident
                (Location.mknoloc (Longident.Lident "strict"))) ])
  in
  let binding =
    Ast_helper.Vb.mk ~attrs:[zero_alloc]
      ~modes:[Location.mknoloc (Parsetree.Mode "noalloc_strict")]
      (Ast_helper.Pat.var (Location.mknoloc "f"))
      (Expr.lam
         [Expr.param_positional "x0" int_ty; Expr.param_positional "x1" float_ty]
         body)
  in
  let structure =
    Ty.Tenv.to_structure_items ctx.tenv
    @ [Ast_helper.Str.value Asttypes.Nonrecursive [binding]]
  in
  let header =
    Printf.sprintf "(* goal: %s *)\n"
      (Format.asprintf "%a" Pprintast.core_type (Ty.to_core_type ty))
  in
  let source = header ^ Format.asprintf "%a@." Pprintast.structure structure in
  { Sample.source }
