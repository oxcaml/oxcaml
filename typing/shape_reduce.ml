(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Ulysse Gérard, Thomas Refis, Tarides                   *)
(*                    Nathanaëlle Courant, OCamlPro                       *)
(*              Gabriel Scherer, projet Picube, INRIA Paris               *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Shape

type result =
  | Resolved of Uid.t
  | Resolved_alias of Uid.t * result
  | Unresolved of t
  | Approximated of Uid.t option
  | Internal_error_missing_uid

let rec print_result fmt result =
  match result with
  | Resolved uid ->
      Format.fprintf fmt "@[Resolved: %a@]@;" Uid.print uid
  | Resolved_alias (uid, r) ->
      Format.fprintf fmt "@[Alias: %a -> %a@]@;"
        Uid.print uid print_result r
  | Unresolved shape ->
      Format.fprintf fmt "@[Unresolved: %a@]@;" print shape
  | Approximated (Some uid) ->
      Format.fprintf fmt "@[Approximated: %a@]@;" Uid.print uid
  | Approximated None ->
      Format.fprintf fmt "@[Approximated: No uid@]@;"
  | Internal_error_missing_uid ->
      Format.fprintf fmt "@[Missing uid@]@;"


let find_shape env id =
  let namespace = Shape.Sig_component_kind.Module in
  Env.shape_of_path ~namespace env (Pident id)

module Make(Params : sig
  val fuel : int
  val read_unit_shape : unit_name:string -> t option
end) = struct
  (* We implement a strong call-by-need reduction, following an
     evaluator from Nathanaelle Courant. *)

  type nf = { uid: Uid.t option; desc: nf_desc; approximated: bool }
  and nf_desc =
    | NVar of var
    | NApp of nf * nf
    | NAbs of local_env * var * t * delayed_nf
    | NStruct of delayed_nf Item.Map.t
    | NAlias of delayed_nf
    | NProj of nf * Item.t
    | NLeaf
    | NComp_unit of string
    | NError of string
    | NConstr of Ident.t * nf list
    | NTuple of nf list
    | NUnboxed_tuple of nf list
    | NPredef of Predef.t * nf list
    | NArrow of nf * nf
    | NPoly_variant of delayed_nf poly_variant_constructors
    | NVariant of {
      simple_constructors: string list;
      complex_constructors: (delayed_nf * Layout.t) complex_constructors
    }
    | NVariant_unboxed of
      { name : string;
        arg_name : string option;
        arg_shape : delayed_nf;
        arg_layout : Layout.t
      }
    | NRecord of
        { fields : (string * delayed_nf * Layout.t) list;
          kind : record_kind
        }

  (* A type of normal forms for strong call-by-need evaluation.
     The normal form of an abstraction
       Abs(x, t)
     is a closure
       NAbs(env, x, t, dnf)
     when [env] is the local environment, and [dnf] is a delayed
     normal form of [t].

     A "delayed normal form" is morally equivalent to (nf Lazy.t), but
     we use a different representation that is compatible with
     memoization (lazy values are not hashable/comparable by default
     comparison functions): we represent a delayed normal form as
     just a not-yet-computed pair [local_env * t] of a term in a
     local environment -- we could also see this as a term under
     an explicit substitution. This delayed thunked is "forced"
     by calling the normalization function as usual, but duplicate
     computations are precisely avoided by memoization.
   *)
  and delayed_nf = Thunk of local_env * t

  and local_env = delayed_nf option Ident.Map.t
  (* When reducing in the body of an abstraction [Abs(x, body)], we
     bind [x] to [None] in the environment. [Some v] is used for
     actual substitutions, for example in [App(Abs(x, body), t)], when
     [v] is a thunk that will evaluate to the normal form of [t]. *)

  let approx_nf nf = { nf with approximated = true }

  let rec equal_local_env t1 t2 =
    Ident.Map.equal (Option.equal equal_delayed_nf) t1 t2

  and equal_delayed_nf t1 t2 =
    match t1, t2 with
    | Thunk (l1, t1), Thunk (l2, t2) ->
      if equal t1 t2 then equal_local_env l1 l2
      else false

  and equal_nf_desc d1 d2 =
    match d1, d2 with
    | NVar v1, NVar v2 -> Ident.equal v1 v2
    | NAbs (l1, v1, t1, nf1), NAbs (l2, v2, t2, nf2) ->
      if not (Ident.equal v1 v2) then false
      else if not (equal t1 t2) then false
      else if not (equal_delayed_nf nf1 nf2) then false
      else equal_local_env l1 l2
    | NApp (v1, t1), NApp (v2, t2) ->
      if equal_nf v1 v2 then equal_nf t1 t2
      else false
    | NLeaf, NLeaf -> true
    | NStruct t1, NStruct t2 ->
      Item.Map.equal equal_delayed_nf t1 t2
    | NProj (t1, i1), NProj (t2, i2) ->
      if Item.compare i1 i2 <> 0 then false
      else equal_nf t1 t2
    | NComp_unit c1, NComp_unit c2 -> String.equal c1 c2
    | NAlias a1, NAlias a2 -> equal_delayed_nf a1 a2
    | NError e1, NError e2 -> String.equal e1 e2
    | NConstr (id1, args1), NConstr (id2, args2) ->
      Ident.equal id1 id2 && List.equal equal_nf args1 args2
    | NTuple args1, NTuple args2 ->
      List.equal equal_nf args1 args2
    | NUnboxed_tuple args1, NUnboxed_tuple args2 ->
      List.equal equal_nf args1 args2
    | NPredef (p1, args1), NPredef (p2, args2) ->
      Predef.equal p1 p2 && List.equal equal_nf args1 args2
    | NArrow (arg1, ret1), NArrow (arg2, ret2) ->
      equal_nf arg1 arg2 && equal_nf ret1 ret2
    | NPoly_variant constrs1, NPoly_variant constrs2 ->
      let equal_pv_constructor c1 c2 =
        String.equal c1.pv_constr_name c2.pv_constr_name &&
        List.equal equal_delayed_nf c1.pv_constr_args c2.pv_constr_args
      in
      List.equal equal_pv_constructor constrs1 constrs2
    | NVariant { simple_constructors = sc1; complex_constructors = cc1 },
      NVariant { simple_constructors = sc2; complex_constructors = cc2 } ->
      List.equal String.equal sc1 sc2 &&
      List.equal
        (Shape.equal_complex_constructor
          (fun (dnf1, ly1) (dnf2, ly2) ->
            Layout.equal ly1 ly2 && equal_delayed_nf dnf1 dnf2))
        cc1 cc2
    | NVariant_unboxed { name = n1; arg_name = an1; arg_shape = as1;
                         arg_layout = al1 },
      NVariant_unboxed { name = n2; arg_name = an2; arg_shape = as2;
                         arg_layout = al2 } ->
      String.equal n1 n2 &&
      Option.equal String.equal an1 an2 &&
      Layout.equal al1 al2 &&
      equal_delayed_nf as1 as2
    | NRecord { fields = f1; kind = k1 }, NRecord { fields = f2; kind = k2 } ->
      Shape.equal_record_kind k1 k2 &&
      List.equal
        (fun (name1, dnf1, ly1) (name2, dnf2, ly2) ->
          String.equal name1 name2 &&
          Layout.equal ly1 ly2 &&
          equal_delayed_nf dnf1 dnf2)
        f1 f2
    | ( ( NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _
        | NAlias _ | NError _ | NConstr _ | NTuple _ | NUnboxed_tuple _
        | NPredef _ | NArrow _ | NPoly_variant _ | NVariant _
        | NVariant_unboxed _ | NRecord _ ), _ ) -> false

  and equal_nf t1 t2 =
    if not (Option.equal Uid.equal t1.uid t2.uid) then false
    else equal_nf_desc t1.desc t2.desc

  module ReduceMemoTable = Hashtbl.Make(struct
      type nonrec t = local_env * t

      let hash t = Hashtbl.hash t

      let equal (env1, t1) (env2, t2) =
        if equal t1 t2 then equal_local_env env1 env2
        else false
  end)

  module ReadBackMemoTable = Hashtbl.Make(struct
      type nonrec t = nf

      let hash t = Hashtbl.hash t

  let equal a b = equal_nf a b
  end)

  let in_reduce_memo_table memo_table memo_key f arg =
    match ReduceMemoTable.find memo_table memo_key with
        | res -> res
    | exception Not_found ->
        let res = f arg in
        ReduceMemoTable.replace memo_table memo_key res;
        res

  let in_read_back_memo_table memo_table memo_key f arg =
    match ReadBackMemoTable.find memo_table memo_key with
    | res -> res
    | exception Not_found ->
        let res = f arg in
        ReadBackMemoTable.replace memo_table memo_key res;
        res

  type env = {
    fuel: int ref;
    global_env: Env.t;
    local_env: local_env;
    reduce_memo_table: nf ReduceMemoTable.t;
    read_back_memo_table: t ReadBackMemoTable.t;
  }

  let bind env var shape =
    { env with local_env = Ident.Map.add var shape env.local_env }

  let rec reduce_ env t =
    let local_env = env.local_env in
    let memo_key = (local_env, t) in
    in_reduce_memo_table env.reduce_memo_table memo_key (reduce__ env) t
  (* Memoization is absolutely essential for performance on this
     problem, because the normal forms we build can in some real-world
     cases contain an exponential amount of redundancy. Memoization
     can avoid the repeated evaluation of identical subterms,
     providing a large speedup, but even more importantly it
     implicitly shares the memory of the repeated results, providing
     much smaller normal forms (that blow up again if printed back
     as trees). A functor-heavy file from Irmin has its shape normal
     form decrease from 100Mio to 2.5Mio when memoization is enabled.

     Note: the local environment is part of the memoization key, while
     it is defined using a type Ident.Map.t of non-canonical balanced
     trees: two maps could have exactly the same items, but be
     balanced differently and therefore hash differently, reducing
     the effectivenss of memoization.
     This could in theory happen, say, with the two programs
       (fun x -> fun y -> ...)
     and
       (fun y -> fun x -> ...)
     having "the same" local environments, with additions done in
     a different order, giving non-structurally-equal trees. Should we
     define our own hash functions to provide robust hashing on
     environments?

     We believe that the answer is "no": this problem does not occur
     in practice. We can assume that identifiers are unique on valid
     typedtree fragments (identifier "stamps" distinguish
     binding positions); in particular the two program fragments above
     in fact bind *distinct* identifiers x (with different stamps) and
     different identifiers y, so the environments are distinct. If two
     environments are structurally the same, they must correspond to
     the evaluation environments of two sub-terms that are under
     exactly the same scope of binders. So the two environments were
     obtained by the same term traversal, adding binders in the same
     order, giving the same balanced trees: the environments have the
     same hash.
  *)

  and force env (Thunk (local_env, t)) =
    reduce_ { env with local_env } t

  and reduce__
    ({fuel; global_env; local_env; _} as env) (t : t) =
    let reduce env t = reduce_ env t in
    let delay_reduce env t = Thunk (env.local_env, t) in
    let return desc = { uid = t.uid; desc; approximated = t.approximated } in
    let rec force_aliases nf = match nf.desc with
      | NAlias delayed_nf ->
          let nf = force env delayed_nf in
          force_aliases nf
      | _ -> nf
    in
    let reset_uid_if_new_binding t' =
      match t.uid with
      | None -> t'
      | Some _ as uid -> { t' with uid }
    in
    if !fuel < 0 then approx_nf (return (NError "NoFuelLeft"))
    else
      match t.desc with
      | Comp_unit unit_name ->
          begin match Params.read_unit_shape ~unit_name with
          | Some t -> reduce env t
          | None -> return (NComp_unit unit_name)
          end
      | App(f, arg) ->
          let f = reduce env f |> force_aliases in
          begin match f.desc with
          | NAbs(clos_env, var, body, _body_nf) ->
              let arg = delay_reduce env arg in
              let env = bind { env with local_env = clos_env } var (Some arg) in
              reduce env body |> reset_uid_if_new_binding
          | _ ->
              let arg = reduce env arg in
              return (NApp(f, arg))
          end
      | Proj(str, item) ->
          let str = reduce env str |> force_aliases in
          let nored () = return (NProj(str, item)) in
          begin match str.desc with
          | NStruct (items) ->
              begin match Item.Map.find item items with
              | exception Not_found -> nored ()
              | nf -> force env nf |> reset_uid_if_new_binding
              end
          | _ ->
              nored ()
          end
      | Abs(var, body) ->
          let body_nf = delay_reduce (bind env var None) body in
          return (NAbs(local_env, var, body, body_nf))
      | Var id ->
          begin match Ident.Map.find id local_env with
          (* Note: instead of binding abstraction-bound variables to
             [None], we could unify it with the [Some v] case by
             binding the bound variable [x] to [NVar x].

             One reason to distinguish the situations is that we can
             provide a different [Uid.t] location; for bound
             variables, we use the [Uid.t] of the bound occurrence
             (not the binding site), whereas for bound values we use
             their binding-time [Uid.t]. *)
          | None -> return (NVar id)
          | Some def ->
              begin match force env def with
              | { uid = Some _; _  } as nf -> nf
                  (* This var already has a binding uid *)
              | { uid = None; _ } as nf -> { nf with uid = t.uid }
                  (* Set the var's binding uid *)
              end
          | exception Not_found ->
          match find_shape global_env id with
          | exception Not_found -> return (NVar id)
          | res when res = t -> return (NVar id)
          | res ->
              decr fuel;
              reduce env res
          end
      | Leaf -> return NLeaf
      | Struct m ->
          let mnf = Item.Map.map (delay_reduce env) m in
          return (NStruct mnf)
      | Alias t -> return (NAlias (delay_reduce env t))
      | Error s -> approx_nf (return (NError s))
      | Constr (id, args) ->
          let nfs = List.map (reduce env) args in
          return (NConstr (id, nfs))
      | Tuple args ->
          let nfs = List.map (reduce env) args in
          return (NTuple nfs)
      | Unboxed_tuple args ->
          let nfs = List.map (reduce env) args in
          return (NUnboxed_tuple nfs)
      | Predef (p, args) ->
          let nfs = List.map (reduce env) args in
          return (NPredef (p, nfs))
      | Arrow (arg, ret) ->
          let arg_nf = reduce env arg in
          let ret_nf = reduce env ret in
          return (NArrow (arg_nf, ret_nf))
      | Poly_variant constrs ->
          let dnf_constrs =
            poly_variant_constructors_map (delay_reduce env) constrs
          in
          return (NPoly_variant dnf_constrs)
      | Variant { simple_constructors; complex_constructors } ->
          let dnf_complex_constructors =
            complex_constructors_map (fun (t, ly) -> (delay_reduce env t, ly))
              complex_constructors
          in
          return (NVariant { simple_constructors;
                             complex_constructors = dnf_complex_constructors })
      | Variant_unboxed { name; arg_name; arg_shape; arg_layout } ->
          let dnf_arg_shape = delay_reduce env arg_shape in
          return (NVariant_unboxed { name; arg_name;
                                     arg_shape = dnf_arg_shape; arg_layout })
      | Record { fields; kind } ->
          let dnf_fields =
            List.map (fun (name, t, ly) -> (name, delay_reduce env t, ly))
              fields
          in
          return (NRecord { fields = dnf_fields; kind })

  and read_back env (nf : nf) : t =
  in_read_back_memo_table env.read_back_memo_table nf (read_back_ env) nf
  (* The [nf] normal form we receive may contain a lot of internal
     sharing due to the use of memoization in the evaluator. We have
     to memoize here again, otherwise the sharing is lost by mapping
     over the term as a tree. *)

  and read_back_ env (nf : nf) : t =
    read_back_desc ~uid:nf.uid env nf.desc

  and read_back_desc ~uid env desc =
    let read_back nf = read_back env nf in
    let read_back_force dnf = read_back (force env dnf) in
    match desc with
    | NVar v ->
      var' uid v
    | NApp (nft, nfu) ->
        let f = read_back nft in
        let arg = read_back nfu in
        app ?uid f ~arg
    | NAbs (_env, x, _t, nf) ->
      let body = read_back_force nf in
      abs ?uid x body
    | NStruct nstr ->
      let map = Item.Map.map read_back_force nstr in
      str ?uid map
    | NProj (nf, item) ->
        let t = read_back nf in
        proj ?uid t item
    | NLeaf -> leaf' uid
    | NComp_unit s -> comp_unit ?uid s
    | NAlias nf -> alias ?uid (read_back_force nf)
    | NError t -> error ?uid t
    | NConstr (id, args) ->
      let t_args = List.map read_back args in
      constr ?uid id t_args
    | NTuple args ->
      let t_args = List.map read_back args in
      tuple ?uid t_args
    | NUnboxed_tuple args ->
      let t_args = List.map read_back args in
      unboxed_tuple ?uid t_args
    | NPredef (p, args) ->
      let t_args = List.map read_back args in
      predef ?uid p t_args
    | NArrow (arg, ret) ->
      let t_arg = read_back arg in
      let t_ret = read_back ret in
      arrow ?uid t_arg t_ret
    | NPoly_variant constrs ->
      let t_constrs = poly_variant_constructors_map read_back_force constrs in
      poly_variant ?uid t_constrs
    | NVariant { simple_constructors; complex_constructors } ->
      let t_complex_constructors =
        complex_constructors_map
          (fun (dnf, ly) -> (read_back_force dnf, ly))
          complex_constructors
      in
      variant ?uid simple_constructors t_complex_constructors
    | NVariant_unboxed { name; arg_name; arg_shape; arg_layout } ->
      let t_arg_shape = read_back_force arg_shape in
      variant_unboxed ?uid name arg_name t_arg_shape arg_layout
    | NRecord { fields; kind } ->
      let t_fields =
        List.map (fun (name, dnf, ly) -> (name, read_back_force dnf, ly))
          fields
      in
      record ?uid kind t_fields

  (* Sharing the memo tables is safe at the level of a compilation unit since
    idents should be unique *)
  let reduce_memo_table = Local_store.s_table ReduceMemoTable.create 42
  let read_back_memo_table = Local_store.s_table ReadBackMemoTable.create 42

  let reduce global_env t =
    let fuel = ref Params.fuel in
    let local_env = Ident.Map.empty in
    let env = {
      fuel;
      global_env;
      reduce_memo_table = !reduce_memo_table;
      read_back_memo_table = !read_back_memo_table;
      local_env;
    } in
    reduce_ env t |> read_back env

  let rec is_stuck_on_comp_unit (nf : nf) =
    match nf.desc with
    | NVar _ ->
        (* This should not happen if we only reduce closed terms *)
        false
    | NApp (nf, _) | NProj (nf, _) -> is_stuck_on_comp_unit nf
    | NStruct _ | NAbs _ -> false
    | NAlias _ -> false
    | NComp_unit _ -> true
    | NError _ -> false
    | NLeaf -> false
    | NConstr _ | NTuple _ | NUnboxed_tuple _
    | NPredef _ | NArrow _ | NPoly_variant _ | NVariant _ | NVariant_unboxed _
    | NRecord _ -> false

  let rec reduce_aliases_for_uid env (nf : nf) =
    match nf with
    | { uid = Some uid; desc = NAlias dnf; approximated = false; _ } ->
        let result = reduce_aliases_for_uid env (force env dnf) in
        Resolved_alias (uid, result)
    | { uid = Some uid; approximated = false; _ } -> Resolved uid
    | { uid; approximated = true } -> Approximated uid
    | { uid = None; approximated = false; _ } ->
      (* A missing Uid after a complete reduction means the Uid was first
         missing in the shape which is a code error. Having the
         [Missing_uid] reported will allow Merlin (or another tool working
         with the index) to ask users to report the issue if it does happen.
      *)
      Internal_error_missing_uid

  let reduce_for_uid global_env t =
    let fuel = ref Params.fuel in
    let local_env = Ident.Map.empty in
    let env = {
      fuel;
      global_env;
      reduce_memo_table = !reduce_memo_table;
      read_back_memo_table = !read_back_memo_table;
      local_env;
    } in
    let nf = reduce_ env t in
    if is_stuck_on_comp_unit nf then
      Unresolved (read_back env nf)
    else
      reduce_aliases_for_uid env nf
end

module Local_reduce =
  Make(struct
    let fuel = 10
    let read_unit_shape ~unit_name:_ = None
  end)

let local_reduce = Local_reduce.reduce
let local_reduce_for_uid = Local_reduce.reduce_for_uid
