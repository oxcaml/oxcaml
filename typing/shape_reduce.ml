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
  | Resolved_decl of Uid.t
  | Resolved_alias of Uid.t * result
  | Unresolved of t
  | Approximated of Uid.t option
  | Missing_uid of t
  | Internal_error_missing_uid

let rec print_result fmt result =
  match result with
  | Resolved uid ->
      Format.fprintf fmt "@[Resolved:@ %a@]" Uid.print uid
  | Resolved_decl uid ->
      Format.fprintf fmt "@[Resolved decl:@ %a@]" Uid.print uid
  | Resolved_alias (uid, r) ->
      Format.fprintf fmt "@[Alias:@ %a@] ->@ %a"
        Uid.print uid print_result r
  | Unresolved shape ->
      Format.fprintf fmt "@[Unresolved:@ %a@]" print shape
  | Approximated (Some uid) ->
      Format.fprintf fmt "@[Approximated:@ %a@]" Uid.print uid
  | Approximated None ->
      Format.fprintf fmt "Approximated: No uid"
  | Missing_uid shape ->
      Format.fprintf fmt "Missing uid: %a" print shape
  | Internal_error_missing_uid ->
      Format.fprintf fmt "Missing uid"


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
    | NPack of Ident.t
    | NComp_unit of string
    | NError of string

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

  (* [_print_nf] is an (incomplete) printer for normal forms
     useful for debugging purposes *)
  let _print_nf fmt nf =
    let print_uid_opt =
      Format.pp_print_option (fun fmt -> Format.fprintf fmt "<%a>" Uid.print)
    in
    let rec aux fmt { uid; desc; _ }=
      match desc with
      | NComp_unit name -> Format.fprintf fmt "CU %s" name
      | NLeaf ->
          Format.fprintf fmt "<%a>" print_uid_opt uid
      | NPack id ->
          Format.fprintf fmt "<%a>" Ident.print id
      | NVar var ->
          Format.fprintf fmt "%a%a" Ident.print var print_uid_opt uid
      | NProj (nf, item) ->
        Format.fprintf fmt "(%a.%a)%a" aux nf Item.print item print_uid_opt uid
      | NApp (nf1, nf2) ->
          Format.fprintf fmt "@[%a(@,%a)%a@]" aux nf1 aux nf2
            print_uid_opt uid
      | NStruct map ->
          let print_map fmt =
            Item.Map.iter (fun item (Thunk (_, t)) ->
                Format.fprintf fmt "@[<hv 2>%a ->@ %a;@]@,"
                  Item.print item
                  print t
              )
          in
          if Item.Map.is_empty map then
            Format.fprintf fmt "@[<hv>{%a}@]" print_uid_opt uid
          else
            Format.fprintf fmt "{@[<v>%a@,%a@]}" print_uid_opt uid print_map map
      | NAlias (Thunk (_, t)) ->
        Format.fprintf fmt "Alias@[(@[<v>%a@,<delayed:%a>@])@]"
          print_uid_opt uid print t
      | NError s ->
          Format.fprintf fmt "Error %s" s
      | NAbs _ -> ()
    in
    Format.fprintf fmt "@[%a@]@;" aux nf

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
    | NPack id1, NPack id2 -> Ident.equal id1 id2
    | NVar _, (NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NAlias _ | NError _ | NPack _)
    | NLeaf, (NVar _ | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NAlias _ | NError _ | NPack _)
    | NApp _, (NVar _ | NLeaf | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NAlias _ | NError _ | NPack _)
    | NAbs _, (NVar _ | NLeaf | NApp _ | NStruct _ | NProj _ | NComp_unit _ | NAlias _ | NError _ | NPack _)
    | NStruct _, (NVar _ | NLeaf | NApp _ | NAbs _ | NProj _ | NComp_unit _ | NAlias _ | NError _ | NPack _)
    | NProj _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NComp_unit _ | NAlias _ | NError _ | NPack _)
    | NComp_unit _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NAlias _ | NError _ | NPack _)
    | NAlias _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NError _ | NPack _)
    | NError _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NAlias _ | NPack _)
    | NPack _, (NVar _ | NLeaf | NApp _ | NAbs _ | NStruct _ | NProj _ | NComp_unit _ | NAlias _ | NError _)
    -> false

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
      | Pack id -> return (NPack id)
      | Struct m ->
          let mnf = Item.Map.map (delay_reduce env) m in
          return (NStruct mnf)
      | Alias t -> return (NAlias (delay_reduce env t))
      | Error s -> approx_nf (return (NError s))

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
      var (Option.get uid) v
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
    | NPack path -> leaf_for_unpack path
    | NComp_unit s -> comp_unit ?uid s
    | NAlias nf -> alias ?uid (read_back_force nf)
    | NError t -> error ?uid t

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
    | NLeaf | NPack _ -> false

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
      (* Format.eprintf "IEMUID: \n%a\n%a\n%!"
      _print_nf nf
      Shape.print (read_back env nf); *)
      (* Internal_error_missing_uid *)
      Missing_uid (read_back env nf)

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

module Ident_and_uid = Identifiable.Make (Identifiable.Pair (Ident) (Uid))

let uid_memo : Uid.t Ident_and_uid.Tbl.t ref =
  Local_store.s_table Ident_and_uid.Tbl.create 16

let make_definition_uid ~current_unit parent_id decl_uid =
  match Ident_and_uid.Tbl.find_opt !uid_memo (parent_id, decl_uid) with
  | Some uid -> uid
  | None ->
    let uid = Uid.mk_ghost ~current_unit in
    Uid.Deps.record_declaration_dependency
      (Definition_to_declaration, uid, decl_uid);
      Ident_and_uid.Tbl.add !uid_memo (parent_id, decl_uid) uid;
    uid

let find_uid_by_path env namespace path =
  try
    Option.some @@ match (namespace : Sig_component_kind.t) with
      | Value ->
        let vd = Env.find_value path env in
        vd.val_uid
      | Type | Extension_constructor | Constructor | Label | Unboxed_label ->
        let td = Env.find_type path env in
        td.type_uid
      | Module ->
        let md = Env.find_module path env in
        md.md_uid
      | Module_type ->
        let mtd = Env.find_modtype path env in
        mtd.mtd_uid
      | Class ->
        let cty = Env.find_class path env in
        cty.cty_uid
      | Class_type ->
        let clty = Env.find_cltype path env in
        clty.clty_uid
  with Not_found -> None

let rec stuck_on_var_or_pack (t : t) =
  match t.desc with
  | Var id | Pack id -> Some id
  | App (t, _) | Proj (t, _) -> stuck_on_var_or_pack t
  | Struct _ | Abs _ -> None
  | Alias _ -> None
  | Comp_unit _ -> None
  | Error _ -> None
  | Leaf -> None

let local_reduce_for_uid env ~namespace path shape =
  match Local_reduce.reduce_for_uid env shape with
  | Missing_uid t ->
    begin match stuck_on_var_or_pack t with
    | None -> Internal_error_missing_uid
    | Some parent_id ->
      begin match find_uid_by_path env namespace path with
        | Some uid ->
            let current_unit = Env.get_unit_name () in
            let uid = make_definition_uid ~current_unit parent_id uid in
            Resolved_decl uid
        | None -> Internal_error_missing_uid
      end
    end
  | otherwise -> otherwise
