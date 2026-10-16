type exp =
  | Var of string
  | App of exp * exp
  | Fun of string * exp
  | Let of string * exp * exp
  | Annot of exp * ty

and ty =
  | Tvar of tyvar
  | Tunivar of tyunivar
  | Tconstr of string * ty list
  | Tarrow of ty * ty
  | Tforall of univar list * ty

and tyunivar =
  { name : int; scope : int }

and tyvar =
  { id : int
  ; mutable level : int
  ; mutable link : ty option
  ; mutable guards : guard list
  ; mutable waiters : (ty -> unit) list
  ; mutable instances : (copy_scope * ty) list
  }

and copy_scope =
  { src_level : int
  ; dst_level : int
  ; mutable copies : (tyvar * ty) list
  }

and guard =
  { mutable alive : bool
  ; mutable vars : tyvar list
  }

type scheme = { level : int; body : ty }

let counter = ref 0
let fresh () =
  incr counter;
  !counter

let current_level = ref 0

let with_level l f =
  let old = !current_level in
  current := l;
  Fun.protect ~finally:(fun () -> current_level := old) f

let with_new_level f = with_level (!current_level + 1) f

let new_tyunivar ?(scope = !current_level) () = { name = fresh (); scope }
let new_tyvar ?(level = !current_level) () =
  { id = fresh (); level; link = None; guards = []; waiters = []; instances = [] }

let rec repr = function
  | Tvar { link = Some t; _ } -> repr t
  | t -> t

let tvar ?level () = Tvar (new_tyvar ?region ())
let tunivar ?scope () = Tunivar (new_tyunivar ?scope ())
let tarrow ty1 ty2 = Tarrow (ty1, ty2)
let tconstr c tys = Tconstr (c, tys)

let tforall us t =
  match us, repr t with
  | [], t -> t
  | us, Tforall (us', t) -> Tforall (us @ us', t)
  | us, t -> Tforall (us, t)

let rec subst_univars us t =
  match repr t with
  | Tunivar u as t -> Option.value (List.assoc_opt u us) ~default:t
  | Tvar _ as t -> t
  | Tcon (c, ts) -> Tcon (c, List.map (subst_tforall us) ts)
  | Tarrow (ta, tb) -> Tarrow (subst_tforall us ta, subst_tforall us tb)
  | Tforall (us', t) ->
    Tforall (us', subst_tforall (List.filter (fun (u, _) -> not (List.mem u us')) us) t)

let instance_poly t =
  match repr t with
  | Tforall (us, t) -> subst_univars (List.map (fun u -> u, tvar ()) us) t
  | t -> t

let is_unguarded v =
  v.guards <- List.filter (fun g -> g.alive) v.guards;
  List.is_empty v.guards

let is_guarded v = not (is_unguarded v)

let add_guards_var gs v =
  List.iter
    (fun g ->
      if not (List.memq g v.guards)
      then (
        v.guards <- g :: v.guards;
        g.vars <- v :: g.vars))
    gs

let add_guards gs t =
  let rec loop t =
    match repr t with
    | Tvar v -> add_guards_var v gs
    | Tunivar _ -> ()
    | Tcon (_, ts) -> List.iter loop ts
    | Tarrow (ta, tb) ->
      loop ta;
      loop tb
    | Tforall (_, t) -> loop t
  in
  loop t

let alloc_guard () = { alive = true; vars = [] }

let free_guard g =
  g.alive <- false;
  List.iter (fun v -> if is_unguarded v then v.instances <- []) g.vars;
  g.vars <- []

let rec match_ t ~closure k =
  match repr t with
  | Tvar v as t ->
    let g = alloc_guard () in
    let l = !current_level in
    List.iter (add_guards [ g ]) (t :: captures);
    let rec wait t =
      match repr t with
      | Tvar v -> v.waiters <- wait :: v.waiters
      | t ->
        with_level l (fun () -> k t);
        free_guard g
    in
    wait t
  | t -> k t

let rec copy cs t =
  match repr t with
  | Tvar v when v.level >= i.src_level ->
    (match List.assq_opt v i.copies with
    | Some t' -> t'
    | None ->
      let t' = tvar ~level:i.dst_level () in
      cs.copies <- (v, t') :: cs.copies;
      if is_guarded v
      then (
        add_guards v.guards t';
        v.instances <- (cs, t') :: v.instances);
      t')
  | (Tvar _ | Tunivar _) as t -> t
  | Tcon (c, ts) -> Tcon (c, List.map (copy i) ts)
  | Tarrow (ta, tb) -> Tarrow (copy i ta, copy i tb)
  | Tforall (us, t) -> Tforall (us, copy i t)

let generalize ty =
  { level = !current_level; body = ty }

let instance { level; body } =
  copy { src_level = level; dst_level = !current_level; copies = [] } body

let rec unify t1 t2 =
  match repr t1, repr t2 with
  | Tunivar u1, Tunivar u2 when u1 = u2 -> ()
  | Tvar v1, Tvar v2 when v1 == v2 -> ()
  | Tvar v, t | t, Tvar v -> unify_var v t
  | Tcon (c1, ts1), Tcon (c2, ts2) when c1 = c2 ->
    List.iter2 unify ts1 ts2
  | Tarrow (t11, t12), Tarrow (t21, t22) ->
    unify t11 t21;
    unify t12 t22
  | Tforall (us1, t1), Tforall (us2, t2) ->
    with_new_level (fun () ->
      let sks = List.map (fun _ -> tunivar ()) us1 in
      let t1 = subst_univars (List.combine us1 sks) t1 in
      let t2 = subst_univars (List.combine us2 sks) t2 in
      unify t1 t2)
  | _ -> failwith "cannot unify"

and unify_var v t =
  (* Lowering levels may trigger some new equations *)
  let after_unify_var = update_levels v t in
  if is_guarded v then add_guards v.guards t;
  v.link <- Some t;
  (* [v] is now linked: safe to unify again. *)
  let is = v.instances in
  let ws = v.waiters in
  v.instances <- [];
  v.waiters <- [];
  List.iter (fun (v, t') -> unify t' (Tvar v)) after_unify_var;
  List.iter (fun (cs, t') -> unify t' (copy cs t)) is;
  List.iter (fun w -> w t) ws

and update_levels v t =
  let after_unify_var = ref [] in
  let rec loop bound_univars t =
    match repr t with
    | Tvar v' ->
      if v == v' then failwith "occurs check";
      if v'.level > v.level then
      after_unify_var := lower_level v' v.level @ !after_unify_var
    | Tunivar u ->
      if (not (List.mem u bound)) && u.scope > v.level
      then failwith "univar scope escape"
    | Tcon (_, ts) -> List.iter (loop bound_univars) ts
    | Tarrow (ta, tb) ->
      loop bound_univars ta;
      loop bound_univars tb
    | Tforall (us, t) -> loop (us @ bound_univars) t
  in
  loop [] t;
  !after_unify_var

and lower_level v l =
  v.level <- l;
  let inner, outer = List.partition (fun (cs, _) -> cs.src_level > l) v.instances in
  v.instances <- outer;
  List.map (fun (_, t') -> v, t') inner
