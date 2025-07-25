(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*               Gabriel Scherer, Project Parsifal, INRIA Saclay          *)
(*               Alban Reynaud, ENS Lyon                                  *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*   Copyright 2018 Alban Reynaud                                         *)
(*   Copyright 2018 INRIA                                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Static checking of recursive declarations, as described in

      A practical mode system for recursive definitions
      Alban Reynaud, Gabriel Scherer and Jeremy Yallop
      POPL 2021

Some recursive definitions are meaningful
{[
  let rec factorial = function 0 -> 1 | n -> n * factorial (n - 1)
  let rec infinite_list = 0 :: infinite_list
]}
but some other are meaningless
{[
  let rec x = x
  let rec x = x+1
]}

Intuitively, a recursive definition makes sense when the body of the
definition can be evaluated without fully knowing what the recursive
name is yet.

In the [factorial] example, the name [factorial] refers to a function,
evaluating the function definition [function ...] can be done
immediately and will not force a recursive call to [factorial] -- this
will only happen later, when [factorial] is called with an argument.

In the [infinite_list] example, we can evaluate [0 :: infinite_list]
without knowing the full content of [infinite_list], but with just its
address. This is a case of productive/guarded recursion.

On the contrary, [let rec x = x] is unguarded recursion (the meaning
is undetermined), and [let rec x = x+1] would need the value of [x]
while evaluating its definition [x+1].

This file implements a static check to decide which definitions are
known to be meaningful, and which may be meaningless. In the general
case, we handle a set of mutually-recursive definitions
{[
let rec x1 = e1
and x2 = e2
...
and xn = en
]}


Our check (see function [is_valid_recursive_expression] is defined
using two criteria:

Usage of recursive variables: how does each of the [e1 .. en] use the
 recursive variables [x1 .. xn]?

Static or dynamic size: for which of the [ei] can we compute the
  in-memory size of the value without evaluating [ei] (so that we can
  pre-allocate it, and thus know its final address before evaluation).

The "static or dynamic size" is decided by the classify_* functions below.

The "variable usage" question is decided by a static analysis looking
very much like a type system. The idea is to assign "access modes" to
variables, where an "access mode" [m] is defined as either

    m ::= Ignore (* the value is not used at all *)
        | Delay (* the value is not needed at definition time *)
        | Guard (* the value is stored under a data constructor *)
        | Return (* the value result is directly returned *)
        | Dereference (* full access and inspection of the value *)

The access modes of an expression [e] are represented by a "context"
[G], which is simply a mapping from variables (the variables used in
[e]) to access modes.

The core notion of the static check is a type-system-like judgment of
the form [G |- e : m], which can be interpreted as meaning either of:

- If we are allowed to use the variables of [e] at the modes in [G]
  (but not more), then it is safe to use [e] at the mode [m].

- If we want to use [e] at the mode [m], then its variables are
  used at the modes in [G].

In practice, for a given expression [e], our implementation takes the
desired mode of use [m] as *input*, and returns a context [G] as
*output*, which is (uniquely determined as) the most permissive choice
of modes [G] for the variables of [e] such that [G |- e : m] holds.
*)

open Asttypes
open Typedtree
open Types

(** {1 Static or dynamic size} *)

type sd = Value_rec_types.recursive_binding_kind

let is_ref : Types.value_description -> bool = function
  | { Types.val_kind =
        Types.Val_prim { Primitive.prim_name = "%makemutable";
                          prim_arity = 1 } } ->
        true
  | _ -> false

(* See the note on abstracted arguments in the documentation for
    Typedtree.Texp_apply *)
let is_abstracted_arg : arg_label * apply_arg -> bool = function
  | (_, Omitted _) -> true
  | (_, Arg _) -> false

let classify_expression : Typedtree.expression -> sd =
  (* We need to keep track of the size of expressions
      bound by local declarations, to be able to predict
      the size of variables. Compare:

        let rec r =
          let y = fun () -> r ()
          in y

      and

        let rec r =
          let y = if Random.bool () then ignore else fun () -> r ()
          in y

    In both cases the final address of `r` must be known before `y` is compiled,
    and this is only possible if `r` has a statically-known size.

    The first definition can be allowed (`y` has a statically-known
    size) but the second one is unsound (`y` has no statically-known size).
  *)
  let rec classify_expression env e : sd =
    match e.exp_desc with
    (* binding and variable cases *)
    | Texp_let (rec_flag, vb, e) ->
        let env = classify_value_bindings rec_flag env vb in
        classify_expression env e
    | Texp_letmutable (vb, e) ->
        let env = classify_value_bindings Nonrecursive env [vb] in
        classify_expression env e
    | Texp_letmodule (Some mid, _, _, mexp, e) ->
        (* Note on module presence:
           For absent modules (i.e. module aliases), the module being bound
           does not have a physical representation, but its size can still be
           derived from the alias itself, so we can re-use the same code as
           for modules that are present. *)
        let size = classify_module_expression env mexp in
        let env = Ident.add mid size env in
        classify_expression env e
    | Texp_ident (path, _, _, _, _) ->
        classify_path env path

    (* non-binding cases *)
    | Texp_open (_, e)
    | Texp_letmodule (None, _, _, _, e)
    | Texp_sequence (_, _, e)
    | Texp_letexception (_, e)
    | Texp_exclave e ->
        classify_expression env e

    | Texp_construct (_, {cstr_repr = Variant_unboxed}, [e], _) ->
        classify_expression env e
    | Texp_construct _ ->
        Static

    | Texp_record { representation = Record_unboxed;
                    fields = [| _, Overridden (_,e) |] } ->
        classify_expression env e
    | Texp_record { representation = Record_ufloat; _ } ->
        Dynamic
    | Texp_record _ ->
        Static

    | Texp_record_unboxed_product { representation = Record_unboxed_product;
                                    fields = [| _, Overridden (_,e) |] } ->
        classify_expression env e
    | Texp_record_unboxed_product _ ->
        Dynamic

    | Texp_variant _
    | Texp_tuple _
    | Texp_extension_constructor _
    | Texp_constant _
    | Texp_src_pos ->
        Static

    | Texp_unboxed_tuple _ ->
      Dynamic

    | Texp_overwrite _
    | Texp_hole _ ->
      Dynamic (* Disallowed for now *)

    | Texp_for _
    | Texp_setfield _
    | Texp_while _
    | Texp_setinstvar _ ->
        (* Unit-returning expressions *)
        Static

    | Texp_mutvar _
    | Texp_setmutvar _ ->
        Static

    | Texp_unreachable ->
        Static

    | Texp_probe _
    | Texp_probe_is_enabled _ ->
        (* CR vlaviron: Dynamic would probably be a better choice *)
        Static

    | Texp_apply ({exp_desc = Texp_ident (_, _, vd, Id_prim _, _)}, _, _, _, _)
      when is_ref vd ->
        Static
    | Texp_apply (_, args, _, _, _)
      when List.exists is_abstracted_arg args ->
        Static
    | Texp_apply _ ->
        Dynamic

    | Texp_array _ ->
        Static
    | Texp_pack mexp ->
        classify_module_expression env mexp
    | Texp_function _ ->
        Static
    | Texp_lazy e ->
      (* The code below was copied (in part) from translcore.ml *)
      begin match Typeopt.classify_lazy_argument e with
      | `Constant_or_function ->
        (* A constant expr (of type <> float if [Config.flat_float_array] is
           true) gets compiled as itself. *)
          classify_expression env e
      | `Float_that_cannot_be_shortcut
      | `Identifier `Forward_value ->
          (* Forward blocks *)
          Static
      | `Identifier `Other ->
          classify_expression env e
      | `Other ->
          (* other cases compile to a lazy block holding a function *)
          Static
      end

    | Texp_new _
    | Texp_instvar _
    | Texp_object _
    | Texp_match _
    | Texp_list_comprehension _
    | Texp_array_comprehension _
    | Texp_ifthenelse _
    | Texp_send _
    | Texp_field _
    | Texp_unboxed_field _
    | Texp_assert _
    | Texp_try _
    | Texp_override _
    | Texp_letop _ ->
        Dynamic
  and classify_value_bindings rec_flag env bindings =
    (* We use a non-recursive classification, classifying each
        binding with respect to the old environment
        (before all definitions), even if the bindings are recursive.

        Note: computing a fixpoint in some way would be more
        precise, as the following could be allowed:

          let rec topdef =
            let rec x = y and y = fun () -> topdef ()
            in x
    *)
    ignore rec_flag;
    let old_env = env in
    let add_value_binding env vb =
      match vb.vb_pat.pat_desc with
      | Tpat_var (id, _loc, _uid, _mode) ->
          let size = classify_expression old_env vb.vb_expr in
          Ident.add id size env
      | _ ->
          (* Note: we don't try to compute any size for complex patterns *)
          env
    in
    List.fold_left add_value_binding env bindings
  and classify_path env : _ -> Value_rec_types.recursive_binding_kind = function
    | Path.Pident x ->
        begin
          try Ident.find_same x env
          with Not_found ->
            (* an identifier will be missing from the map if either:
                - it is a non-local identifier
                  (bound outside the letrec-binding we are analyzing)
                - or it is bound by a complex (let p = e in ...) local binding
                - or it is bound within a module (let module M = ... in ...)
                  that we are not traversing for size computation

                For non-local identifiers it might be reasonable (although
                not completely clear) to consider them Static (they have
                already been evaluated), but for the others we must
                under-approximate with Not_recursive.

                This could be fixed by a more complete implementation.
            *)
            Dynamic
        end
    | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ ->
        (* local modules could have such paths to local definitions;
            classify_expression could be extend to compute module
            shapes more precisely *)
        Dynamic
  and classify_module_expression env mexp : sd =
    match mexp.mod_desc with
    | Tmod_ident (path, _) ->
        classify_path env path
    | Tmod_structure _ ->
        Static
    | Tmod_functor _ ->
        Static
    | Tmod_apply _ ->
        Dynamic
    | Tmod_apply_unit _ ->
        Dynamic
    | Tmod_constraint (mexp, _, _, coe) ->
        begin match coe with
        | Tcoerce_none ->
            classify_module_expression env mexp
        | Tcoerce_structure _ ->
            Static
        | Tcoerce_functor _ ->
            Static
        | Tcoerce_primitive _ ->
            Misc.fatal_error "letrec: primitive coercion on a module"
        | Tcoerce_alias _ ->
            Misc.fatal_error "letrec: alias coercion on a module"
        end
    | Tmod_unpack (e, _) ->
        classify_expression env e
  in classify_expression Ident.empty


(** {1 Usage of recursive variables} *)

module Mode = struct
  (** For an expression in a program, its "usage mode" represents
      static information about how the value produced by the expression
      will be used by the context around it. *)
  type t =
    | Ignore
    (** [Ignore] is for subexpressions that are not used at all during
       the evaluation of the whole program. This is the mode of
       a variable in an expression in which it does not occur. *)

    | Delay
    (** A [Delay] context can be fully evaluated without evaluating its argument
        , which will only be needed at a later point of program execution. For
        example, [fun x -> ?] or [lazy ?] are [Delay] contexts. *)

    | Guard
    (** A [Guard] context returns the value as a member of a data structure,
        for example a variant constructor or record. The value can safely be
        defined mutually-recursively with their context, for example in
        [let rec li = 1 :: li].
        When these subexpressions participate in a cyclic definition,
        this definition is productive/guarded.

        The [Guard] mode is also used when a value is not dereferenced,
        it is returned by a sub-expression, but the result of this
        sub-expression is discarded instead of being returned.
        For example, the subterm [?] is in a [Guard] context
        in [let _ = ? in e] and in [?; e].
        When these subexpressions participate in a cyclic definition,
        they cannot create a self-loop.
    *)

    | Return
    (** A [Return] context returns its value without further inspection.
        This value cannot be defined mutually-recursively with its context,
        as there is a risk of self-loop: in [let rec x = y and y = x], the
        two definitions use a single variable in [Return] context. *)

    | Dereference
    (** A [Dereference] context consumes, inspects and uses the value
        in arbitrary ways. Such a value must be fully defined at the point
        of usage, it cannot be defined mutually-recursively with its context. *)

  let equal = ((=) : t -> t -> bool)

  (* Lower-ranked modes demand/use less of the variable/expression they qualify
     -- so they allow more recursive definitions.

     Ignore < Delay < Guard < Return < Dereference
  *)
  let rank = function
    | Ignore -> 0
    | Delay -> 1
    | Guard -> 2
    | Return -> 3
    | Dereference -> 4

  (* Returns the more conservative (highest-ranking) mode of the two
     arguments.

     In judgments we write (m + m') for (join m m').
  *)
  let join m m' =
    if rank m >= rank m' then m else m'

  (* If x is used with the mode m in e[x], and e[x] is used with mode
     m' in e'[e[x]], then x is used with mode m'[m] (our notation for
     "compose m' m") in e'[e[x]].

     Return is neutral for composition: m[Return] = m = Return[m].

     Composition is associative and [Ignore] is a zero/annihilator for
     it: (compose Ignore m) and (compose m Ignore) are both Ignore. *)
  let compose m' m = match m', m with
    | Ignore, _ | _, Ignore -> Ignore
    | Dereference, _ -> Dereference
    | Delay, _ -> Delay
    | Guard, Return -> Guard
    | Guard, ((Dereference | Guard | Delay) as m) -> m
    | Return, Return -> Return
    | Return, ((Dereference | Guard | Delay) as m) -> m
end

type mode = Mode.t = Ignore | Delay | Guard | Return | Dereference

module Env :
sig
  type t

  val single : Ident.t -> Mode.t -> t
  (** Create an environment with a single identifier used with a given mode.
  *)

  val empty : t
  (** An environment with no used identifiers. *)

  val find : Ident.t -> t -> Mode.t
  (** Find the mode of an identifier in an environment.  The default mode is
      Ignore. *)

  val unguarded : t -> Ident.t list -> Ident.t list
  (** unguarded e l: the list of all identifiers in l that are dereferenced or
      returned in the environment e. *)

  val dependent : t -> Ident.t list -> Ident.t list
  (** dependent e l: the list of all identifiers in l that are used in e
      (not ignored). *)

  val join : t -> t -> t
  val join_list : t list -> t
  (** Environments can be joined pointwise (variable per variable) *)

  val compose : Mode.t -> t -> t
  (** Environment composition m[G] extends mode composition m1[m2]
      by composing each mode in G pointwise *)

  val remove : Ident.t -> t -> t
  (** Remove an identifier from an environment. *)

  val take: Ident.t -> t -> Mode.t * t
  (** Remove an identifier from an environment, and return its mode *)

  val remove_list : Ident.t list -> t -> t
  (** Remove all the identifiers of a list from an environment. *)

  val equal : t -> t -> bool
end = struct
  module M = Map.Make(Ident)

  (** A "t" maps each rec-bound variable to an access status *)
  type t = Mode.t M.t

  let equal = M.equal Mode.equal

  let find (id: Ident.t) (tbl: t) =
    try M.find id tbl with Not_found -> Ignore

  let empty = M.empty

  let join (x: t) (y: t) =
    M.fold
      (fun (id: Ident.t) (v: Mode.t) (tbl: t) ->
         let v' = find id tbl in
         M.add id (Mode.join v v') tbl)
      x y

  let join_list li = List.fold_left join empty li

  let compose m env =
    M.map (Mode.compose m) env

  let single id mode = M.add id mode empty

  let unguarded env li =
    List.filter (fun id -> Mode.rank (find id env) > Mode.rank Guard) li

  let dependent env li =
    List.filter (fun id -> Mode.rank (find id env) > Mode.rank Ignore) li

  let remove = M.remove

  let take id env = (find id env, remove id env)

  let remove_list l env =
    List.fold_left (fun env id -> M.remove id env) env l
end

let remove_pat pat env =
  Env.remove_list (pat_bound_idents pat) env

let remove_patlist pats env =
  List.fold_right remove_pat pats env

(* Usage mode judgments.

   There are two main groups of judgment functions:

   - Judgments of the form "G |- ... : m"
     compute the environment G of a subterm ... from its mode m, so
     the corresponding function has type [... -> Mode.t -> Env.t].

     We write [... -> term_judg] in this case.

   - Judgments of the form "G |- ... : m -| G'"

     correspond to binding constructs (for example "let x = e" in the
     term "let x = e in body") that have both an exterior environment
     G (the environment of the whole term "let x = e in body") and an
     interior environment G' (the environment at the "in", after the
     binding construct has introduced new names in scope).

     For example, let-binding could be given the following rule:

       G |- e : m + m'
       -----------------------------------
       G+G' |- (let x = e) : m -| x:m', G'

     Checking the whole term composes this judgment
     with the "G |- e : m" form for the let body:

       G  |- (let x = e) : m -| G'
       G' |- body : m
       -------------------------------
       G |- let x = e in body : m

     To this judgment "G |- e : m -| G'" our implementation gives the
     type [... -> Mode.t -> Env.t -> Env.t]: it takes the mode and
     interior environment as inputs, and returns the exterior
     environment.

     We write [... -> bind_judg] in this case.
*)
type term_judg = Mode.t -> Env.t
type bind_judg = Mode.t -> Env.t -> Env.t

let option : 'a. ('a -> term_judg) -> 'a option -> term_judg =
  fun f o m -> match o with
    | None -> Env.empty
    | Some v -> f v m
let list : 'a. ('a -> term_judg) -> 'a list -> term_judg =
  fun f li m ->
    List.fold_left (fun env item -> Env.join env (f item m)) Env.empty li
let listi : 'a. (int -> 'a -> term_judg) -> 'a list -> term_judg =
  fun f li m ->
    List.fold_left (fun (idx, env) item -> idx+1, Env.join env (f idx item m))
      (0, Env.empty) li
    |> (snd : (int * Env.t) -> Env.t)
let array : 'a. ('a -> term_judg) -> 'a array -> term_judg =
  fun f ar m ->
    Array.fold_left (fun env item -> Env.join env (f item m)) Env.empty ar

let single : Ident.t -> term_judg = Env.single
let remove_id : Ident.t -> term_judg -> term_judg =
  fun id f m -> Env.remove id (f m)
let remove_ids : Ident.t list -> term_judg -> term_judg =
  fun ids f m -> Env.remove_list ids (f m)

let join : term_judg list -> term_judg =
  fun li m -> Env.join_list (List.map (fun f -> f m) li)

let empty = fun _ -> Env.empty

(* A judgment [judg] takes a mode from the context as input, and
   returns an environment. The judgment [judg << m], given a mode [m']
   from the context, evaluates [judg] in the composed mode [m'[m]]. *)
let (<<) : term_judg -> Mode.t -> term_judg =
  fun f inner_mode -> fun outer_mode -> f (Mode.compose outer_mode inner_mode)

(* A binding judgment [binder] expects a mode and an inner environment,
   and returns an outer environment. [binder >> judg] computes
   the inner environment as the environment returned by [judg]
   in the ambient mode. *)
let (>>) : bind_judg -> term_judg -> term_judg =
  fun binder term mode -> binder mode (term mode)

(* Compute the appropriate [mode] for an array expression *)
let array_mode exp elt_sort = match Typeopt.array_kind exp elt_sort with
  | Lambda.Pfloatarray ->
    (* (flat) float arrays unbox their elements *)
    Dereference
  | Lambda.Pgenarray ->
    (* This is counted as a use, because constructing a generic array
       involves inspecting to decide whether to unbox (PR#6939). *)
    Dereference
  | Lambda.Paddrarray | Lambda.Pintarray ->
    (* non-generic, non-float arrays act as constructors *)
    Guard
  | Lambda.Punboxedfloatarray _ | Lambda.Punboxedintarray _
  | Lambda.Punboxedvectorarray _
  | Lambda.Pgcscannableproductarray _ | Lambda.Pgcignorableproductarray _ ->
    Dereference

(* Expression judgment:
     G |- e : m
   where (m) is an input of the code and (G) is an output;
   in the Prolog mode notation, this is (+G |- -e : -m).
*)
let rec expression : Typedtree.expression -> term_judg =
  fun exp -> match exp.exp_desc with
    | Texp_ident (pth, _, _, _, _) ->
      path pth
    | Texp_let (rec_flag, bindings, body) ->
      (*
         G  |- <bindings> : m -| G'
         G' |- body : m
         -------------------------------
         G |- let <bindings> in body : m
      *)
      value_bindings rec_flag bindings >> expression body
    | Texp_letmutable (binding,body) ->
      (*
         G  |- <bindings> : m -| G'
         G' |- body : m
         --------------------------------
         G |- let mutable <bindings> in body : m
      *)
      value_bindings Nonrecursive [binding] >> expression body
    | Texp_letmodule (x, _, _, mexp, e) ->
      module_binding (x, mexp) >> expression e
    | Texp_match (e, _, cases, _) ->
      (*
         (Gi; mi |- pi -> ei : m)^i
         G |- e : sum(mi)^i
         ----------------------------------------------
         G + sum(Gi)^i |- match e with (pi -> ei)^i : m
       *)
      (fun mode ->
        let pat_envs, pat_modes =
          List.split (List.map (fun c -> case c mode) cases) in
        let env_e = expression e (List.fold_left Mode.join Ignore pat_modes) in
        Env.join_list (env_e :: pat_envs))
    | Texp_for tf ->
      (*
        G1 |- low: m[Dereference]
        G2 |- high: m[Dereference]
        G3 |- body: m[Guard]
        ---
        G1 + G2 + G3 |- for _ = low to high do body done: m
      *)
      join [
        expression tf.for_from << Dereference;
        expression tf.for_to << Dereference;
        expression tf.for_body << Guard;
      ]
    | Texp_constant _ ->
      empty
    | Texp_new (pth, _, _, _) ->
      (*
        G |- c: m[Dereference]
        -----------------------
        G |- new c: m
      *)
      path pth << Dereference
    | Texp_instvar (self_path, pth, _inst_var) ->
        join [path self_path << Dereference; path pth]
    | Texp_mutvar id ->
        single id.txt << Dereference
    | Texp_apply
        ({exp_desc = Texp_ident (_, _, vd, Id_prim _, _)}, [_, Arg (arg, _)], _,
         _, _)
      when is_ref vd ->
      (*
        G |- e: m[Guard]
        ------------------
        G |- ref e: m
      *)
      expression arg << Guard
    | Texp_apply (e, args, _, _, _)  ->
        (* [args] may contain omitted arguments, corresponding to labels in
           the function's type that were not passed in the actual application.
           The arguments before the first omitted argument are passed to the
           function immediately, so they are dereferenced. The arguments after
           the first omitted one are stored in a closure, so guarded.
           The function itself is called immediately (dereferenced) if there
           is at least one argument before the first omitted one.
           On the other hand, if the first argument is omitted then the
           function is stored in the closure without being called. *)
        let rec split_args ~has_omitted_arg = function
          | [] -> [], []
          | (_, Omitted _) :: rest -> split_args ~has_omitted_arg:true rest
          | (_, Arg (arg, _)) :: rest ->
            let applied, delayed = split_args ~has_omitted_arg rest in
            if has_omitted_arg
            then applied, arg :: delayed
            else arg :: applied, delayed
        in
        let applied, delayed = split_args ~has_omitted_arg:false args in
        let function_mode =
          match applied with
          | [] -> Guard
          | _ :: _ -> Dereference
        in
        join [expression e << function_mode;
              list expression applied << Dereference;
              list expression delayed << Guard]
    | Texp_tuple (exprs, _) ->
      list expression (List.map snd exprs) << Guard
    | Texp_unboxed_tuple exprs ->
      list expression (List.map (fun (_, e, _) -> e) exprs) << Return
    | Texp_array (_, elt_sort, exprs, _) ->
      let elt_sort = Jkind.Sort.default_for_transl_and_get elt_sort in
      list expression exprs << array_mode exp elt_sort
    | Texp_list_comprehension { comp_body; comp_clauses } ->
      join ((expression comp_body << Guard) ::
            comprehension_clauses comp_clauses)
    | Texp_array_comprehension (_, elt_sort, { comp_body; comp_clauses }) ->
      let elt_sort = Jkind.Sort.default_for_transl_and_get elt_sort in
      join ((expression comp_body << array_mode exp elt_sort) ::
            comprehension_clauses comp_clauses)
    | Texp_construct (_, desc, exprs, _) ->
      let access_constructor =
        match desc.cstr_tag with
        | Extension pth ->
          path pth << Dereference
        | _ -> empty
      in
      let arg_mode i = match desc.cstr_repr with
        | Variant_unboxed | Variant_with_null ->
          Return
        | Variant_boxed _ | Variant_extensible ->
           (match desc.cstr_shape with
            | Constructor_uniform_value -> Guard
            | Constructor_mixed mixed_shape ->
                (match mixed_shape.(i) with
                 | Value | Float_boxed -> Guard
                 | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64
                 | Vec128 | Vec256 | Vec512 | Word | Void | Product _ ->
                   Dereference))
      in
      let arg i e = expression e << arg_mode i in
      join [
        access_constructor;
        listi arg exprs;
      ]
    | Texp_variant (_, eo) ->
      (*
        G |- e: m[Guard]
        ------------------   -----------
        G |- `A e: m         [] |- `A: m
      *)
      option (fun (e, _) -> expression e) eo << Guard
    | Texp_record { fields = es; extended_expression = eo;
                    representation = rep } ->
        let field_mode i = match rep with
          | Record_float | Record_ufloat -> Dereference
          | Record_unboxed | Record_inlined (_, _, Variant_unboxed) -> Return
          | Record_boxed _ | Record_inlined (_, Constructor_uniform_value, _) ->
              Guard
          | Record_inlined (_, Constructor_mixed mixed_shape, _)
          | Record_mixed mixed_shape ->
            (match mixed_shape.(i) with
             | Value | Float_boxed -> Guard
             | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64
             | Vec128 | Vec256 | Vec512 | Word | Void | Product _ ->
               Dereference)
        in
        let field (label, field_def) =
          let env =
            match field_def with
            | Kept _ -> empty
            | Overridden (_, e) -> expression e
          in
          env << field_mode label.lbl_pos
        in
        join [
          array field es;
          option expression (Option.map Misc.fst3 eo) << Dereference
        ]
    | Texp_record_unboxed_product { fields = es; extended_expression = eo;
                                    representation = rep } ->
      begin match rep with
      | Record_unboxed_product ->
        let field (_, field_def) =
          let env =
            match field_def with
            | Kept _ -> empty
            | Overridden (_, e) -> expression e
          in
          env << Return
        in
        join [
          array field es;
          option expression (Option.map fst eo) << Dereference
        ]
      end
    | Texp_ifthenelse (cond, ifso, ifnot) ->
      (*
        Gc |- c: m[Dereference]
        G1 |- e1: m
        G2 |- e2: m
        ---
        Gc + G1 + G2 |- if c then e1 else e2: m

      Note: `if c then e1 else e2` is treated in the same way as
      `match c with true -> e1 | false -> e2`
      *)
      join [
        expression cond << Dereference;
        expression ifso;
        option expression ifnot;
      ]
    | Texp_setfield (e1, _, _, _, e2) ->
      (*
        G1 |- e1: m[Dereference]
        G2 |- e2: m[Dereference]
        ---
        G1 + G2 |- e1.x <- e2: m

        Note: e2 is dereferenced in the case of a field assignment to
        a record of unboxed floats in that case, e2 evaluates to
        a boxed float and it is unboxed on assignment.
      *)
      join [
        expression e1 << Dereference;
        expression e2 << Dereference;
      ]
    | Texp_sequence (e1, _, e2) ->
      (*
        G1 |- e1: m[Guard]
        G2 |- e2: m
        --------------------
        G1 + G2 |- e1; e2: m

        Note: `e1; e2` is treated in the same way as `let _ = e1 in e2`
      *)
      join [
        expression e1 << Guard;
        expression e2;
      ]
    | Texp_while {wh_cond; wh_body} ->
      (*
        G1 |- cond: m[Dereference]
        G2 |- body: m[Guard]
        ---------------------------------
        G1 + G2 |- while cond do body done: m
      *)
      join [
        expression wh_cond << Dereference;
        expression wh_body << Guard;
      ]
    | Texp_send (e1, _, _) ->
      (*
        G |- e: m[Dereference]
        ---------------------- (plus weird 'eo' option)
        G |- e#x: m
      *)
      join [
        expression e1 << Dereference
      ]
    | Texp_field (e, _, _, _, _, _) ->
      (*
        G |- e: m[Dereference]
        -----------------------
        G |- e.x: m
      *)
      expression e << Dereference
    | Texp_unboxed_field (e, _, _, _, _) ->
      expression e << Dereference
    | Texp_setinstvar (pth,_,_,e) ->
      (*
        G |- e: m[Dereference]
        ----------------------
        G |- x <- e: m
      *)
      join [
        path pth << Dereference;
        expression e << Dereference;
      ]
    | Texp_setmutvar (_id,_sort,e) ->
      (*
        G |- e: m[Dereference]
        ----------------------
        G |- x <- e: m
      *)
        expression e << Dereference
    | Texp_letexception ({ext_id}, e) ->
      (* G |- e: m
         ----------------------------
         G |- let exception A in e: m
      *)
      remove_id ext_id (expression e)
    | Texp_assert (e, _) ->
      (*
        G |- e: m[Dereference]
        -----------------------
        G |- assert e: m

        Note: `assert e` is treated just as if `assert` was a function.
      *)
      expression e << Dereference
    | Texp_pack mexp ->
      (*
        G |- M: m
        ----------------
        G |- module M: m
      *)
      modexp mexp
    | Texp_object (clsstrct, _) ->
      class_structure clsstrct
    | Texp_try (e, cases) ->
      (*
        G |- e: m      (Gi; _ |- pi -> ei : m)^i
        --------------------------------------------
        G + sum(Gi)^i |- try e with (pi -> ei)^i : m

        Contrarily to match, the patterns p do not inspect
        the value of e, so their mode does not influence the
        mode of e.
      *)
      let case_env c m = fst (case c m) in
      join [
        expression e;
        list case_env cases;
      ]
    | Texp_override (pth, fields) ->
      (*
         G |- pth : m   (Gi |- ei : m[Dereference])^i
         ----------------------------------------------------
         G + sum(Gi)^i |- {< (xi = ei)^i >} (at path pth) : m

         Note: {< .. >} is desugared to a function application, but
         the function implementation might still use its arguments in
         a guarded way only -- intuitively it should behave as a constructor.
         We could possibly refine the arguments' Dereference into Guard here.
      *)
      let field (_, _, arg) = expression arg in
      join [
        path pth << Dereference;
        list field fields << Dereference;
      ]
    | Texp_function { params; body } ->
      (*
         G      |-{body} b  : m[Delay]
         (Hj    |-{def}  Pj : m[Delay])^j
         H  := sum(Hj)^j
         ps := sum(pat(Pj))^j
         -----------------------------------
         G + H - ps |- fun (Pj)^j -> b : m
      *)
        let param_pat param =
          (* param P ::=
              | ?(pat = expr)
              | pat

             Define pat(P) as
                pat if P = ?(pat = expr)
                pat if P = pat
          *)
          match param.fp_kind with
          | Tparam_pat pat -> pat
          | Tparam_optional_default (pat, _, _) -> pat
        in
        (* Optional argument defaults.
           G |-{def} P : m
        *)
        let param_default param =
          match param.fp_kind with
          | Tparam_optional_default (_, default, _) ->
                  (*
                      G |- e : m
                      ------------------
                      G |-{def} ?(p=e) : m
                  *)
              expression default
          | Tparam_pat _ ->
                  (*
                      ------------------
                      . |-{def} p : m
                  *)
              empty
        in
        let patterns = List.map param_pat params in
        let defaults = List.map param_default params in
        let body = function_body body in
        let f = join (body :: defaults) << Delay in
        (fun m ->
          let env = f m in
          remove_patlist patterns env)
    | Texp_lazy e ->
      (*
        G |- e: m[Delay]
        ----------------  (modulo some subtle compiler optimizations)
        G |- lazy e: m
      *)
      let lazy_mode = match Typeopt.classify_lazy_argument e with
        | `Constant_or_function
        | `Identifier _
        | `Float_that_cannot_be_shortcut ->
          Return
        | `Other ->
          Delay
      in
      expression e << lazy_mode
    | Texp_letop{let_; ands; body; _} ->
        let case_env c m = fst (case c m) in
        join [
          list binding_op (let_ :: ands) << Dereference;
          case_env body << Delay
        ]
    | Texp_unreachable ->
      (*
        ----------
        [] |- .: m
      *)
      empty
    | Texp_extension_constructor (_lid, pth) ->
      path pth << Dereference
    | Texp_open (od, e) ->
      open_declaration od >> expression e
    | Texp_probe {handler} ->
      expression handler << Dereference
    | Texp_probe_is_enabled _ -> empty
    | Texp_exclave e -> expression e
    | Texp_src_pos -> empty
    | Texp_overwrite (exp1, exp2) ->
      (* This is untested, since we currently mark Texp_overwrite as Dynamic and
         the analysis always stops if there is an overwrite_ in a recursive expression.
         We dereference the cell to be overwritten, since it would not be sound to
         overwrite a cell that is not yet constructed. The new value to be written into
         the cell may itself be recursive. We do not put a guard on here, but this is done
         by the tuple/record/constructor that is contained in the overwritten expression.
      *)
      join [
        expression exp1 << Dereference;
        expression exp2
      ]
    | Texp_hole _ -> empty

(* Function bodies.
    G |-{body} b : m
*)
and function_body body =
  match body with
  | Tfunction_body body ->
    (*
        G |- e : m
        ------------------
        G |-{body} e : m (**)

      (**) The "e" here stands for [Tfunction_body] as opposed to
           [Tfunction_cases].
    *)
      expression body
  | Tfunction_cases { fc_cases = cases; _ } ->
    (*
        (Gi; _ |- pi -> ei : m)^i    (**)
        ------------------
        sum(Gi)^i |-{body} function (pi -> ei)^i : m

      (**) Contrarily to match, the values that are pattern-matched
           are bound locally, so the pattern modes do not influence
           the final environment.
    *)
      List.map (fun c mode -> fst (case c mode)) cases
      |> join

and comprehension_clauses clauses =
  List.concat_map
    (function
      | Texp_comp_for bindings ->
          List.concat_map
            (fun { comp_cb_iterator; comp_cb_attributes = _ } ->
               match comp_cb_iterator with
               | Texp_comp_range { ident = _; start; stop; direction = _ } ->
                   [expression start << Dereference; expression stop << Dereference]
               | Texp_comp_in { pattern = _; sequence } ->
                   [expression sequence << Dereference])
            bindings
      | Texp_comp_when guard ->
          [expression guard << Dereference])
    clauses

and binding_op : Typedtree.binding_op -> term_judg =
  fun bop ->
    join [path bop.bop_op_path; expression bop.bop_exp]

and class_structure : Typedtree.class_structure -> term_judg =
  fun cs -> list class_field cs.cstr_fields

and class_field : Typedtree.class_field -> term_judg =
  fun cf -> match cf.cf_desc with
    | Tcf_inherit (_, ce, _super, _inh_vars, _inh_meths) ->
      class_expr ce << Dereference
    | Tcf_val (_lab, _mut, _, cfk, _) ->
      class_field_kind cfk
    | Tcf_method (_, _, cfk) ->
      class_field_kind cfk
    | Tcf_constraint _ ->
      empty
    | Tcf_initializer e ->
      expression e << Dereference
    | Tcf_attribute _ ->
      empty

and class_field_kind : Typedtree.class_field_kind -> term_judg =
  fun cfk -> match cfk with
    | Tcfk_virtual _ ->
      empty
    | Tcfk_concrete (_, e) ->
      expression e << Dereference

and modexp : Typedtree.module_expr -> term_judg =
  fun mexp -> match mexp.mod_desc with
    | Tmod_ident (pth, _) ->
      path pth
    | Tmod_structure s ->
      structure s
    | Tmod_functor (_, e) ->
      modexp e << Delay
    | Tmod_apply (f, p, _) ->
      join [
        modexp f << Dereference;
        modexp p << Dereference;
      ]
    | Tmod_apply_unit f ->
      modexp f << Dereference
    | Tmod_constraint (mexp, _, _, coe) ->
      let rec coercion coe k = match coe with
        | Tcoerce_none ->
          k Return
        | Tcoerce_structure _
        | Tcoerce_functor _ ->
          (* These coercions perform a shallow copy of the input module,
             by creating a new module with fields obtained by accessing
             the same fields in the input module. *)
           k Dereference
        | Tcoerce_primitive _ ->
          (* This corresponds to 'external' declarations,
             and the coercion ignores its argument *)
          k Ignore
        | Tcoerce_alias (_, pth, coe) ->
          (* Alias coercions ignore their arguments, but they evaluate
             their alias module 'pth' under another coercion. *)
          coercion coe (fun m -> path pth << m)
      in
      coercion coe (fun m -> modexp mexp << m)
    | Tmod_unpack (e, _) ->
      expression e


(* G |- pth : m *)
and path : Path.t -> term_judg =
  (*
    ------------
    x: m |- x: m

    G |- A: m[Dereference]
    -----------------------
    G |- A.x: m

    G1 |- A: m[Dereference]
    G2 |- B: m[Dereference]
    ------------------------ (as for term application)
    G1 + G2 |- A(B): m
  *)
  fun pth -> match pth with
    | Path.Pident x ->
        single x
    | Path.Pdot (t, _) ->
        path t << Dereference
    | Path.Papply (f, p) ->
        join [
          path f << Dereference;
          path p << Dereference;
        ]
    | Path.Pextra_ty (p, _extra) ->
        path p

(* G |- struct ... end : m *)
and structure : Typedtree.structure -> term_judg =
  (*
    G1, {x: _, x in vars(G1)} |- item1: G2 + ... + Gn in m
    G2, {x: _, x in vars(G2)} |- item2: G3 + ... + Gn in m
    ...
    Gn, {x: _, x in vars(Gn)} |- itemn: [] in m
    ---
    (G1 + ... + Gn) - V |- struct item1 ... itemn end: m
  *)
  fun s m ->
    List.fold_right (fun it env -> structure_item it m env)
      s.str_items Env.empty

(* G |- <structure item> : m -| G'
   where G is an output and m, G' are inputs *)
and structure_item : Typedtree.structure_item -> bind_judg =
  fun s m env -> match s.str_desc with
    | Tstr_eval (e, _, _) ->
      (*
        Ge |- e: m[Guard]
        G |- items: m -| G'
        ---------------------------------
        Ge + G |- (e;; items): m -| G'

        The expression `e` is treated in the same way as let _ = e
      *)
      let judg_e = expression e << Guard in
      Env.join (judg_e m) env
    | Tstr_value (rec_flag, bindings) ->
      value_bindings rec_flag bindings m env
    | Tstr_module {mb_id; mb_expr} ->
      module_binding (mb_id, mb_expr) m env
    | Tstr_recmodule mbs ->
      let bindings = List.map (fun {mb_id; mb_expr} -> (mb_id, mb_expr)) mbs in
      recursive_module_bindings bindings m env
    | Tstr_primitive _ ->
      env
    | Tstr_type _ ->
      (*
        -------------------
        G |- type t: m -| G
      *)
      env
    | Tstr_typext {tyext_constructors = exts; _} ->
      let ext_ids = List.map (fun {ext_id = id; _} -> id) exts in
      Env.join
        (list extension_constructor exts m)
        (Env.remove_list ext_ids env)
    | Tstr_exception {tyexn_constructor = ext; _} ->
      Env.join
        (extension_constructor ext m)
        (Env.remove ext.ext_id env)
    | Tstr_modtype _
    | Tstr_class_type _
    | Tstr_attribute _ ->
      env
    | Tstr_open od ->
      open_declaration od m env
    | Tstr_class classes ->
        let class_ids =
          let class_id ({ci_id_class = id; _}, _) = id in
          List.map class_id classes in
        let class_declaration ({ci_expr; _}, _) m =
          Env.remove_list class_ids (class_expr ci_expr m) in
        Env.join
          (list class_declaration classes m)
          (Env.remove_list class_ids env)
    | Tstr_include { incl_mod = mexp; incl_type = mty; _ } ->
      let included_ids = List.map Types.signature_item_id mty in
      Env.join (modexp mexp m) (Env.remove_list included_ids env)

(* G |- module M = E : m -| G *)
and module_binding : (Ident.t option * Typedtree.module_expr) -> bind_judg =
  fun (id, mexp) m env ->
      (*
        GE |- E: m[mM + Guard]
        -------------------------------------
        GE + G |- module M = E : m -| M:mM, G
      *)
      let judg_E, env =
        match id with
        | None -> modexp mexp << Guard, env
        | Some id ->
          let mM, env = Env.take id env in
          let judg_E = modexp mexp << (Mode.join mM Guard) in
          judg_E, env
      in
      Env.join (judg_E m) env

and open_declaration : Typedtree.open_declaration -> bind_judg =
  fun { open_expr = mexp; open_bound_items = sg; _ } m env ->
      let judg_E = modexp mexp in
      let bound_ids = List.map Types.signature_item_id sg in
      Env.join (judg_E m) (Env.remove_list bound_ids env)

and recursive_module_bindings
  : (Ident.t option * Typedtree.module_expr) list -> bind_judg =
  fun m_bindings m env ->
    let mids = List.filter_map fst m_bindings in
    let binding (mid, mexp) m =
      let judg_E =
        match mid with
        | None -> modexp mexp << Guard
        | Some mid ->
          let mM = Env.find mid env in
          modexp mexp << (Mode.join mM Guard)
      in
      Env.remove_list mids (judg_E m)
    in
    Env.join (list binding m_bindings m) (Env.remove_list mids env)

and class_expr : Typedtree.class_expr -> term_judg =
  fun ce -> match ce.cl_desc with
    | Tcl_ident (pth, _, _) ->
        path pth << Dereference
    | Tcl_structure cs ->
        class_structure cs
    | Tcl_fun (_, _, args, ce, _) ->
        let ids = List.map fst args in
        remove_ids ids (class_expr ce << Delay)
    | Tcl_apply (ce, args) ->
        let arg (_, arg) =
          match arg with
          | Omitted _ -> empty
          | Arg (e, _) -> expression e
        in
        join [
          class_expr ce << Dereference;
          list arg args << Dereference;
        ]
    | Tcl_let (rec_flag, bindings, _, ce) ->
      value_bindings rec_flag bindings >> class_expr ce
    | Tcl_constraint (ce, _, _, _, _) ->
        class_expr ce
    | Tcl_open (_, ce) ->
        class_expr ce

and extension_constructor : Typedtree.extension_constructor -> term_judg =
  fun ec -> match ec.ext_kind with
    | Text_decl _ ->
      empty
    | Text_rebind (pth, _lid) ->
      path pth

(* G |- let (rec?) (pi = ei)^i : m -| G' *)
and value_bindings : rec_flag -> Typedtree.value_binding list -> bind_judg =
  fun rec_flag bindings mode bound_env ->
    let all_bound_pats = List.map (fun vb -> vb.vb_pat) bindings in
    let outer_env = remove_patlist all_bound_pats bound_env in
    let bindings_env =
      match rec_flag with
      | Nonrecursive ->
        (*
           (Gi, pi:_ |- ei : m[mbody_i])^i   (pi : mbody_i -| D)^i
           ------------------------------------------------------------
           Sum(Gi) + (D - (pi)^i) |- let (pi=ei)^i : m -| D
        *)
          let binding_env {vb_pat; vb_expr; _} m =
            let m' = Mode.compose m (pattern vb_pat bound_env) in
            remove_pat vb_pat (expression vb_expr m') in
          list binding_env bindings mode
      | Recursive ->
        (*
           (Gi, (xj : mdef_ij)^j |- ei : m[mbody_i])^i   (xi : mbody_i -| D)^i
           G'i = Gi + mdef_ij[G'j]
           -------------------------------------------------------------------
           Sum(G'i) + (D - (pi)^i) |- let rec (xi=ei)^i : m -| D

           The (mdef_ij)^i,j are a family of modes over two indices:
           mdef_ij represents the mode of use, within e_i the definition of x_i,
           of the mutually-recursive variable x_j.

           The (G'i)^i are defined from the (Gi)^i as a family of equations,
           whose smallest solution is computed as a least fixpoint.

           The (Gi)^i are the "immediate" dependencies of each (ei)^i
           on the outer context (excluding the mutually-defined
           variables).
           The (G'i)^i contain the "transitive" dependencies as well:
           if ei depends on xj, then the dependencies of G'i of xi
           must contain the dependencies of G'j, composed by
           the mode mdef_ij of use of xj in ei.

           For example, consider:

             let rec z =
               let rec x = ref y
               and y = ref z
               in f x

           this definition should be rejected as the body [f x]
           dereferences [x], which can be used to access the
           yet-unitialized value [z]. This requires realizing that [x]
           depends on [z] through [y], which requires the transitive
           closure computation.

           An earlier version of our check would take only the (Gi)^i
           instead of the (G'i)^i, which is incorrect and would accept
           the example above.
        *)
          (* [binding_env] takes a binding (x_i = e_i)
             and computes (Gi, (mdef_ij)^j). *)
          let binding_env {vb_pat = x_i; vb_expr = e_i; _} =
            let mbody_i = pattern x_i bound_env in
            (* Gi, (x_j:mdef_ij)^j  *)
            let rhs_env_i = expression e_i (Mode.compose mode mbody_i) in
            (* (mdef_ij)^j (for a fixed i) *)
            let mutual_modes =
              let mdef_ij {vb_pat = x_j; _} = pattern x_j rhs_env_i in
              List.map mdef_ij bindings in
            (* Gi *)
            let env_i = remove_patlist all_bound_pats rhs_env_i in
            (* (Gi, (mdef_ij)^j) *)
            (env_i, mutual_modes) in
          let env, mdef =
            List.split (List.map binding_env bindings) in
          let rec transitive_closure env =
            let transitive_deps env_i mdef_i =
              (* Gi, (mdef_ij)^j => Gi + Sum_j mdef_ij[Gj] *)
              Env.join env_i
                (Env.join_list (List.map2 Env.compose mdef_i env)) in
            let env' = List.map2 transitive_deps env mdef in
            if List.for_all2 Env.equal env env'
            then env'
            else transitive_closure env'
          in
          let env'_i = transitive_closure env in
          Env.join_list env'_i
    in Env.join bindings_env outer_env

(* G; m' |- (p -> e) : m
   with outputs G, m' and input m

   m' is the mode under which the scrutinee of p
   (the value matched against p) is placed.
*)
and case
    : 'k . 'k Typedtree.case -> mode -> Env.t * mode
  = fun { Typedtree.c_lhs; c_guard; c_rhs } ->
    (*
       Ge |- e : m    Gg |- g : m[Dereference]
       G := Ge+Gg     p : mp -| G
       ----------------------------------------
       G - p; m[mp] |- (p (when g)? -> e) : m
    *)
    let judg = join [
        option expression c_guard << Dereference;
        expression c_rhs;
      ] in
    (fun m ->
       let env = judg m in
       (remove_pat c_lhs env), Mode.compose m (pattern c_lhs env))

(* p : m -| G
   with output m and input G

   m is the mode under which the scrutinee of p is placed.
*)
and pattern : type k . k general_pattern -> Env.t -> mode = fun pat env ->
  (*
    mp := | Dereference if p is destructuring
          | Guard       otherwise
    me := sum{G(x), x in vars(p)}
    --------------------------------------------
    p : (mp + me) -| G
  *)
  let m_pat = if is_destructuring_pattern pat
              then Dereference
              else Guard
  in
  let m_env =
    pat_bound_idents pat
    |> List.map (fun id -> Env.find id env)
    |> List.fold_left Mode.join Ignore
  in
  Mode.join m_pat m_env

and is_destructuring_pattern : type k . k general_pattern -> bool =
  fun pat -> match pat.pat_desc with
    | Tpat_any -> false
    | Tpat_var (_, _, _, _) -> false
    | Tpat_alias (pat, _, _, _, _, _) -> is_destructuring_pattern pat
    | Tpat_constant _ -> true
    | Tpat_tuple _ -> true
    | Tpat_unboxed_tuple _ -> true
    | Tpat_construct _ -> true
    | Tpat_variant _ -> true
    | Tpat_record (_, _) -> true
    | Tpat_record_unboxed_product (_, _) -> true
    | Tpat_array _ -> true
    | Tpat_lazy _ -> true
    | Tpat_value pat -> is_destructuring_pattern (pat :> pattern)
    | Tpat_exception _ -> false
    | Tpat_or (l,r,_) ->
        is_destructuring_pattern l || is_destructuring_pattern r

let is_valid_recursive_expression idlist expr : sd option =
  match expr.exp_desc with
  | Texp_function _ ->
     (* Fast path: functions can never have invalid recursive references *)
     Some Static
  | _ ->
     let rkind = classify_expression expr in
     let is_valid =
       match rkind with
       | Static ->
         (* The expression has known size or is constant *)
         let ty = expression expr Return in
         Env.unguarded ty idlist = []
       | Dynamic ->
         (* The expression has unknown size *)
         let ty = expression expr Return in
         Env.unguarded ty idlist = [] && Env.dependent ty idlist = []
     in
     if is_valid then Some rkind else None

(* A class declaration may contain let-bindings. If they are recursive,
   their validity will already be checked by [is_valid_recursive_expression]
   during type-checking. This function here prevents a different kind of
   invalid recursion, which is the unsafe creations of objects of this class
   in the let-binding. For example,
   {|class a = let x = new a in object ... end|}
   is forbidden, but
   {|class a = let x () = new a in object ... end|}
   is allowed.
*)
let is_valid_class_expr idlist ce =
  let rec class_expr : mode -> Typedtree.class_expr -> Env.t =
    fun mode ce -> match ce.cl_desc with
      | Tcl_ident (_, _, _) ->
        (*
          ----------
          [] |- a: m
        *)
        Env.empty
      | Tcl_structure _ ->
        (*
          -----------------------
          [] |- struct ... end: m
        *)
        Env.empty
      | Tcl_fun (_, _, _, _, _) -> Env.empty
        (*
          ---------------------------
          [] |- fun x1 ... xn -> C: m
        *)
      | Tcl_apply (_, _) -> Env.empty
      | Tcl_let (rec_flag, bindings, _, ce) ->
        value_bindings rec_flag bindings mode (class_expr mode ce)
      | Tcl_constraint (ce, _, _, _, _) ->
        class_expr mode ce
      | Tcl_open (_, ce) ->
        class_expr mode ce
  in
  match Env.unguarded (class_expr Return ce) idlist with
  | [] -> true
  | _ :: _ -> false
