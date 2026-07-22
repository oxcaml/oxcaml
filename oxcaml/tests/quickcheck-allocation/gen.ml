(* Type-directed program generator. See DESIGN.md, "Generation".

   Structure follows the in-repo precedent of
   testsuite/tests/comprehensions/quickcheck_lists_arrays_haskell_python.ml: a
   typed AST ([Expr]), a type-directed generator that only produces well-typed
   terms, and a pretty-printer. A single program is emitted, carrying both
   annotations on the function under test (see [Sample]).

   Tier A: construction of one layer -- tuples, records (int and float),
   variants, lists, arrays -- plain vs [exclave_]-wrapped, plus mode-crossing
   immediates, glued together with integer/float arithmetic, comparisons, [let],
   and [if].

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

(* Shared type declarations emitted into both files. [r] is an
   all-immediate-field record, [fr] a float record (flat float layout), [v] a
   variant with an immediate and a boxed constructor. *)
let prelude =
  "type r = { a : int; b : int }\n\
   type fr = { fx : float; fy : float }\n\
   type v = A | B of int\n\n"

(* How an argument is passed, mirroring the compiler's [Asttypes.arg_label].
   Part of the function type: it determines call-site syntax. For [Optional],
   the arrow's payload type is the type behind the [?] (whether the *binder* has
   a default is an expression-side property; see [Expr.param_kind]). *)
module Arg_label = struct
  type t =
    | Nolabel (* t1 -> ... call: f e *)
    | Labelled of string (* ~l:t1 -> ... call: f ~l:e *)
    | Optional of string (* ?l:t1 -> ... call: f ~l:e, or omitted *)

  let equal a b =
    match a, b with
    | Nolabel, Nolabel -> true
    | Labelled l1, Labelled l2 | Optional l1, Optional l2 -> String.equal l1 l2
    | (Nolabel | Labelled _ | Optional _), _ -> false

  let to_string = function
    | Nolabel -> ""
    | Labelled l -> l ^ ":"
    | Optional l -> "?" ^ l ^ ":"
end

module Ty = struct
  type t =
    | Int
    | Float
    | Pair of t * t
    | List of t
    | Array of t
    (* CR shsong: need to improve how we generate user-defined types *)
    | Record_int (* r *)
    | Record_float (* fr *)
    | Variant (* v *)
    | Arrow of
        { label : Arg_label.t;
          arg : t; (* payload type; for [Optional], the type behind the ? *)
          ret : t (* possibly another [Arrow]: multi-parameter = a chain *)
        }

  let rec equal t1 t2 =
    match t1, t2 with
    | Int, Int | Float, Float -> true
    | Record_int, Record_int | Record_float, Record_float | Variant, Variant ->
      true
    | Pair (a1, b1), Pair (a2, b2) -> equal a1 a2 && equal b1 b2
    | List a, List b | Array a, Array b -> equal a b
    | Arrow a1, Arrow a2 ->
      Arg_label.equal a1.label a2.label
      && equal a1.arg a2.arg && equal a1.ret a2.ret
    | ( ( Int | Float | Pair _ | List _ | Array _ | Record_int | Record_float
        | Variant | Arrow _ ),
        _ ) ->
      false

  let rec to_string = function
    | Int -> "int"
    | Float -> "float"
    | Pair (a, b) -> "(" ^ to_string a ^ " * " ^ to_string b ^ ")"
    | List t -> to_string t ^ " list"
    | Array t -> to_string t ^ " array"
    | Record_int -> "r"
    | Record_float -> "fr"
    | Variant -> "v"
    | Arrow { label; arg; ret } ->
      "(" ^ Arg_label.to_string label ^ to_string arg ^ " -> " ^ to_string ret
      ^ ")"

  (* The arrow chain of a function type: one (label, payload) per parameter,
     outermost first, plus the final non-arrow result. *)
  let rec arrows = function
    | Arrow { label; arg; ret } ->
      let params, result = arrows ret in
      (label, arg) :: params, result
    | ty -> [], ty

  (* Types whose construction could plausibly be wrapped in [exclave_] (closures
     are allocated too). This is distribution steering, not an allocation
     prediction. *)
  let exclave_candidate = function
    | Pair _ | List _ | Array _ | Record_int | Record_float | Variant | Arrow _
      ->
      true
    | Int | Float -> false
end

module Expr = struct
  type t =
    | Var of string
    | Int_lit of int
    | Float_lit of float
    (* CR shsong: consider to group bop to one variant *)
    | Add of t * t (* int + int *)
    | Fadd of t * t (* float +. float *)
    | Leq of t * t (* int <= int, used only as an [If] condition *)
    | If of t * t * t
    | Let of string * t * t
    | Pair of t * t
    | Nil
    | Cons of t * t
    | Array_lit of t list
    (* CR shsong: in the following four cases, user defined types are hard-coded
       again. Improve this. *)
    | Record_int of t * t (* { a; b } *)
    | Record_float of t * t (* { fx; fy } *)
    | Constr_a (* A -- immediate *)
    | Constr_b of t (* B e -- boxed *)
    | Exclave of t (* exclave_ e; only ever placed in tail position *)
    | Lam of
        { params : param list;
              (* n-ary: arity = length params. Function arity is syntactic since
                 OCaml 5.2, so binding an arrow chain as one n-ary [Lam] (a
                 single closure) or as nested unary [Lam]s (a closure allocating
                 an inner closure per partial application) are different
                 programs of the same type. *)
          body : t
        }
    | App of
        { f : t;
          args : (Arg_label.t * t) list
              (* in parameter order; an [Optional] parameter simply has no entry
                 when omitted (it is erased when the following positional
                 argument is applied) *)
        }

  and param =
    { var : string;
          (* binder, always fresh -- never shadows, and independent of the
             type-side label *)
      p_ty : Ty.t; (* payload type (matches the corresponding [Ty.Arrow.arg]) *)
      kind : param_kind
    }

  and param_kind =
    | Positional (* (var : t) *)
    | Labelled of string (* ~label:(var : t) *)
    | Opt of string * t option
  (* [Some d]: ?label:(var = (d : t)), binder has type [t] in the body. [None]:
     ?label:(var : t option)

     CR shsong: the [None] case is NOT generated yet: the binder would have type
     [t option], which needs option types in [Ty]. *)

  (* Fully parenthesized: ugly but unambiguous, and shrinking (milestone 3) is
     the readability story, not the printer. *)
  let rec to_string = function
    | Var x -> x
    | Int_lit n ->
      if n < 0 then "(" ^ string_of_int n ^ ")" else string_of_int n
    | Float_lit f ->
      let s = Printf.sprintf "%F" f in
      if f < 0.0 then "(" ^ s ^ ")" else s
    | Add (a, b) -> "(" ^ to_string a ^ " + " ^ to_string b ^ ")"
    | Fadd (a, b) -> "(" ^ to_string a ^ " +. " ^ to_string b ^ ")"
    | Leq (a, b) -> "(" ^ to_string a ^ " <= " ^ to_string b ^ ")"
    | If (c, t, e) ->
      "(if " ^ to_string c ^ " then " ^ to_string t ^ " else " ^ to_string e
      ^ ")"
    | Let (x, e1, e2) ->
      "(let " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2 ^ ")"
    | Pair (a, b) -> "(" ^ to_string a ^ ", " ^ to_string b ^ ")"
    | Nil -> "[]"
    | Cons (h, t) -> "(" ^ to_string h ^ " :: " ^ to_string t ^ ")"
    | Array_lit es -> "[| " ^ String.concat "; " (List.map to_string es) ^ " |]"
    | Record_int (a, b) ->
      "{ a = " ^ to_string a ^ "; b = " ^ to_string b ^ " }"
    | Record_float (a, b) ->
      "{ fx = " ^ to_string a ^ "; fy = " ^ to_string b ^ " }"
    | Constr_a -> "A"
    | Constr_b e -> "(B " ^ to_string e ^ ")"
    | Exclave e -> "exclave_ " ^ to_string e
    | Lam { params; body } ->
      "(fun "
      ^ String.concat " " (List.map param_to_string params)
      ^ " -> " ^ to_string body ^ ")"
    | App { f; args } ->
      "(" ^ to_string f ^ " "
      ^ String.concat " " (List.map arg_to_string args)
      ^ ")"

  and param_to_string { var; p_ty; kind } =
    match kind with
    | Positional -> "(" ^ var ^ " : " ^ Ty.to_string p_ty ^ ")"
    | Labelled l -> "~" ^ l ^ ":(" ^ var ^ " : " ^ Ty.to_string p_ty ^ ")"
    | Opt (l, Some d) ->
      "?" ^ l ^ ":(" ^ var ^ " = (" ^ to_string d ^ " : " ^ Ty.to_string p_ty
      ^ "))"
    | Opt (l, None) ->
      "?" ^ l ^ ":(" ^ var ^ " : " ^ Ty.to_string p_ty ^ " option)"

  and arg_to_string (label, e) =
    match (label : Arg_label.t) with
    | Nolabel -> to_string e
    | Labelled l | Optional l -> "~" ^ l ^ ":" ^ to_string e
end

(* Generation context: PRNG state and a fresh-name supply. The typed environment
   is threaded functionally so bindings scope correctly. *)
type ctx =
  { rng : Random.State.t;
    mutable next_var : int;
    mode : Mode.t
  }

let fresh ctx =
  let n = ctx.next_var in
  ctx.next_var <- n + 1;
  Printf.sprintf "v%d" n

(* Pick among weighted alternatives; entries with weight 0 are disabled. *)
let choose ctx options =
  let options = List.filter (fun (w, _) -> w > 0) options in
  let total = List.fold_left (fun acc (w, _) -> acc + w) 0 options in
  let rec pick n = function
    | [] -> assert false
    | (w, f) :: rest -> if n < w then f () else pick (n - w) rest
  in
  pick (Random.State.int ctx.rng total) options

(* Component types for pairs / lists / arrays are leaf types only: Tier A is
   about one layer of construction, and it also guarantees the generator
   terminates (composite children generated at exhausted fuel bottom out in
   literals). *)
let small_ty ctx =
  choose ctx
    [(3, fun () -> Ty.Int); (2, fun () -> Ty.Float); (2, fun () -> Ty.Variant)]

(* Function types, for [let]-bound and returned closures (Tiers B and C).
   Components are leaf types, mirroring [small_ty]'s termination argument.
   Parameter shapes come from a fixed list that respects the erasability
   invariant: every [Optional] parameter is followed by a positional one
   (otherwise it could never be omitted at a call site, and the definition trips
   warning 16).

   Labels come from a tiny fixed pool, distinct within a chain (duplicate labels
   in one type would break argument matching) but deliberately shared across
   function types: a partially-applied remainder with leftover labeled
   parameters can then equal an independently generated goal type, which is what
   makes label-skipping candidates reachable at all. Binders are separate from
   labels, so this reuse cannot cause shadowing. *)
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
  (* CR shsong: It seems that currently a function type can only be an 1 or
     2-ary function returns small_ty. *)
  List.fold_right
    (fun label ret -> Ty.Arrow { label; arg = small_ty ctx; ret })
    labels (small_ty ctx)

(* CR shsong: We should allow more flexibility of generated type here: 1. Avoid
   having small_ty, i.e., allow other types in pair, list, array, and function
   return type; 2. Allow more kinds of variant and record types. *)
let goal_ty ctx =
  let pair () = Ty.Pair (small_ty ctx, small_ty ctx) in
  match ctx.mode with
  | Mode.Soundness ->
    choose ctx
      [ (3, fun () -> Ty.Int);
        (2, fun () -> Ty.Float);
        (2, fun () -> Ty.Variant);
        2, pair;
        (2, fun () -> Ty.Record_float);
        (1, fun () -> Ty.Record_int);
        (1, fun () -> Ty.List (small_ty ctx));
        (1, fun () -> Ty.Array (small_ty ctx));
        (* returning a closure is prime soundness bait: the closure (and its
           captures) must be allocated *)
        (2, fun () -> fun_ty ctx) ]
  | Mode.Completeness ->
    choose ctx
      [ (6, fun () -> Ty.Int);
        (2, fun () -> Ty.Float);
        (2, fun () -> Ty.Variant);
        1, pair;
        (1, fun () -> Ty.List (small_ty ctx));
        (1, fun () -> fun_ty ctx) ]

let int_lit ctx = Expr.Int_lit (Random.State.int ctx.rng 13 - 3)

let float_lit ctx =
  Expr.Float_lit (float_of_int (Random.State.int ctx.rng 13 - 3) *. 0.5)

(* Generate a well-typed expression of type [ty] in environment [env]. Every
   recursive call decreases [fuel]; once fuel is exhausted only non-recursive
   productions (and one-layer construction with literal children) remain, so
   generation terminates. *)
let rec gen_expr ctx env ty fuel =
  let completeness =
    match ctx.mode with Mode.Completeness -> true | Mode.Soundness -> false
  in
  let deep w = if fuel > 0 then w else 0 in
  (* Productions available at every type. *)
  let vars = List.filter (fun (_, ty') -> Ty.equal ty' ty) env in
  let use_var () =
    let x, _ = List.nth vars (Random.State.int ctx.rng (List.length vars)) in
    Expr.Var x
  in
  let let_in () =
    (* CR shsong: bound_ty cannot be user-defined type here like a record or a
       variant. *)
    let bound_ty =
      choose ctx
        [ (4, fun () -> small_ty ctx);
          ((if completeness then 1 else 2), fun () -> fun_ty ctx) ]
    in
    let x = fresh ctx in
    let e1 = gen_expr ctx env bound_ty (fuel - 1) in
    let e2 = gen_expr ctx ((x, bound_ty) :: env) ty (fuel - 1) in
    Expr.Let (x, e1, e2)
  in
  let if_ () =
    let c =
      Expr.Leq
        (gen_expr ctx env Ty.Int (fuel - 1), gen_expr ctx env Ty.Int (fuel - 1))
    in
    Expr.If (c, gen_expr ctx env ty (fuel - 1), gen_expr ctx env ty (fuel - 1))
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
    Expr.App { f = Expr.Var f; args }
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
    | Ty.Int ->
      [ (2, fun () -> int_lit ctx);
        ( deep (if completeness then 4 else 2),
          fun () ->
            Expr.Add
              ( gen_expr ctx env Ty.Int (fuel - 1),
                gen_expr ctx env Ty.Int (fuel - 1) ) ) ]
    | Ty.Float ->
      [ (2, fun () -> float_lit ctx);
        ( deep 2,
          fun () ->
            Expr.Fadd
              ( gen_expr ctx env Ty.Float (fuel - 1),
                gen_expr ctx env Ty.Float (fuel - 1) ) ) ]
    | Ty.Pair (t1, t2) ->
      [ ( 4,
          fun () ->
            Expr.Pair
              (gen_expr ctx env t1 (fuel - 1), gen_expr ctx env t2 (fuel - 1))
        ) ]
    | Ty.List t ->
      [ (2, fun () -> Expr.Nil);
        ( deep 3,
          fun () ->
            Expr.Cons
              (gen_expr ctx env t (fuel - 1), gen_expr ctx env ty (fuel - 1)) )
      ]
    | Ty.Array t ->
      [ ( 3,
          fun () ->
            let n = Random.State.int ctx.rng 4 in
            Expr.Array_lit
              (List.init n (fun _ -> gen_expr ctx env t (fuel - 1))) ) ]
    | Ty.Record_int ->
      [ ( 4,
          fun () ->
            Expr.Record_int
              ( gen_expr ctx env Ty.Int (fuel - 1),
                gen_expr ctx env Ty.Int (fuel - 1) ) ) ]
    | Ty.Record_float ->
      [ ( 4,
          fun () ->
            Expr.Record_float
              ( gen_expr ctx env Ty.Float (fuel - 1),
                gen_expr ctx env Ty.Float (fuel - 1) ) ) ]
    | Ty.Variant ->
      [ (* Soundness mode favors the immediate constructor (mode crossing). *)
        ((if completeness then 2 else 4), fun () -> Expr.Constr_a);
        (deep 2, fun () -> Expr.Constr_b (gen_expr ctx env Ty.Int (fuel - 1)))
      ]
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
       [fun_ty] components are leaf types). *)
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
                  let var = fresh ctx in
                  let kind =
                    match label with
                    | Nolabel -> Expr.Positional
                    | Labelled l -> Expr.Labelled l
                    | Optional l ->
                      (* always with a default for now; see [Expr.param_kind] *)
                      Expr.Opt (l, Some (gen_expr ctx env arg_ty (fuel - 1)))
                  in
                  ( { Expr.var; p_ty = arg_ty; kind } :: params,
                    (var, arg_ty) :: env ))
                ([], env) prefix
            in
            Expr.Lam
              { params = List.rev rev_params;
                body = gen_expr ctx env' (ret_after k ty) (fuel - 1)
              } ) ]
  in
  choose ctx (generic @ structural)

(* CR: [exclave_] is only placed at the very top of the body (a tail position by
   construction). Extend to tail positions of [if]/[let] when Tier B lands. *)
let maybe_exclave ctx ty body =
  match ctx.mode with
  | Mode.Soundness
    when Ty.exclave_candidate ty && Random.State.int ctx.rng 3 = 0 ->
    Expr.Exclave body
  | Mode.Soundness | Mode.Completeness -> body

let initial_fuel = 5

let generate ~mode ~seed =
  let ctx = { rng = Random.State.make [| seed |]; next_var = 0; mode } in
  let env = ["x0", Ty.Int; "x1", Ty.Float] in
  let ty = goal_ty ctx in
  let body = maybe_exclave ctx ty (gen_expr ctx env ty initial_fuel) in
  (* No return-type annotation is emitted since a plain [: ty] could interact
     with mode inference. Both annotations go on the same function: [@
     noalloc_strict] drives the frontend check (and forces the body's
     allocations local), and [@zero_alloc strict] makes the backend check
     exactly that typed program. *)
  let header = Printf.sprintf "(* goal: %s *)\n" (Ty.to_string ty) in
  let body_str = Expr.to_string body in
  let source =
    Printf.sprintf
      "%s%slet[@zero_alloc strict] (f @ noalloc_strict) (x0 : int) (x1 : \
       float) = %s\n"
      prelude header body_str
  in
  { Sample.source }
