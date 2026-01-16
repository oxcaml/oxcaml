type param = Fexpr.prim_param =
  | Labeled of
      { label : string;
        value : string Fexpr.located
      }
  | Positional of string Fexpr.located
  | Flag of string

type t = Fexpr.prim_op =
  { prim : string;
    params : param list
  }

type to_fl_env = Fexpr_to_flambda_commons.env

type of_fl_env = Flambda_to_fexpr_commons.Env.t

type 'p cons0 = to_fl_env -> 'p -> Flambda_primitive.nullary_primitive

type 'p cons1 = to_fl_env -> 'p -> Flambda_primitive.unary_primitive

type 'p cons2 = to_fl_env -> 'p -> Flambda_primitive.binary_primitive

type 'p cons3 = to_fl_env -> 'p -> Flambda_primitive.ternary_primitive

type 'p cons4 = to_fl_env -> 'p -> Flambda_primitive.quaternary_primitive

type 'p consN = to_fl_env -> 'p -> int -> Flambda_primitive.variadic_primitive

type ('p, 't, 'r) lens =
  { of_fl : of_fl_env -> 'p -> 't;
    to_fl : to_fl_env -> 't -> 'r
  }

type ('a, 'b) map_lens = ('a, 'b, 'a) lens

type 'p value_lens = ('p, string Fexpr.located) map_lens

type 'p params_lens = ('p, param list) map_lens

type 'p prim_lens = ('p, t, Simple.t list -> Flambda_primitive.t) lens

type 'p conv = of_fl_env -> 'p -> t

type _ param_cons =
  | Pos : 'p value_lens -> 'p param_cons
  | Lbl : string * 'p value_lens -> 'p param_cons
  | Flg : string -> unit param_cons
  | Opt : 'p param_cons -> 'p option param_cons
  | Def : 'p param_cons * 'p * ('p -> 'p -> bool) -> 'p param_cons
  | Lst : 'p param_cons list -> 'p list param_cons
  | Etr : 'p case_cons list * ('p -> unit) -> 'p param_cons
  | Map : 'a param_cons * ('b, 'a) map_lens -> 'b param_cons
  | Pa0 : unit param_cons
  | Pa2 : 'a param_cons * 'b param_cons -> ('a * 'b) param_cons
  | Pa3 :
      'a param_cons * 'b param_cons * 'c param_cons
      -> ('a * 'b * 'c) param_cons

and 'p case_cons =
  | Case : ('p, 'c option, 'p) lens * 'c param_cons -> 'p case_cons

let wrap_loc txt = Fexpr.{ txt; loc = Debuginfo.Scoped_location.Loc_unknown }

let unwrap_loc located = located.Fexpr.txt

(* Parsing fexpr parameters. Low-brow iteration through constructors in
   declaration order, consuming the param list on match. *)
let extract_param (env : to_fl_env) (params : param list) (cons : 'p param_cons)
    : 'p * param list =
  let rec pos conv lp params =
    match params with
    | [] -> None, lp
    | Positional p :: rp -> Some (conv env p), lp @ rp
    | ((Labeled _ | Flag _) as p) :: rp -> pos conv (lp @ [p]) rp
  in
  let rec lbl l conv lp params =
    match params with
    | [] -> None, lp
    | (Labeled { label; value } as p) :: rp ->
      if String.equal l label
      then Some (conv env value), lp @ rp
      else lbl l conv (lp @ [p]) rp
    | ((Positional _ | Flag _) as p) :: rp -> lbl l conv (lp @ [p]) rp
  in
  let rec flg f lp params =
    match params with
    | [] -> None, lp
    | (Flag flag as p) :: rp ->
      if String.equal f flag then Some (), lp @ rp else flg f (lp @ [p]) rp
    | ((Positional _ | Labeled _) as p) :: rp -> flg f (lp @ [p]) rp
  in
  let no_param params = Some (), params in
  let rec def : type p. p -> p param_cons -> param list -> p option * param list
      =
   fun d cons params ->
    match aux cons params with
    | None, params -> Some d, params
    | found, params -> found, params
  and opt : type p. p param_cons -> param list -> p option option * param list =
   fun cons params ->
    let (found : p option), params = aux cons params in
    Some found, params
  and lst : type p.
      p param_cons list -> param list -> p list option * param list =
   fun consl params ->
    List.fold_left
      (fun (pl, ps) cons ->
        match pl with
        | None -> None, params
        | Some pl -> (
          match aux cons ps with
          | Some p, ps -> Some (p :: pl), ps
          | None, _ -> None, params))
      (Some [], params) consl
  and etr : type p. p case_cons list -> param list -> p option * param list =
   fun cases params ->
    match cases with
    | [] -> None, params
    | Case (conv, param_cons) :: cases -> (
      match aux param_cons params with
      | Some p, params -> Some (conv.to_fl env (Some p)), params
      | None, _ -> etr cases params)
  and map : type a b.
      a param_cons ->
      (to_fl_env -> a -> b) ->
      param list ->
      b option * param list =
   fun cons f params ->
    let p, params = aux cons params in
    Option.map (f env) p, params
  and param2 : type p q.
      p param_cons -> q param_cons -> param list -> (p * q) option * param list
      =
   fun pc1 pc2 params ->
    let p1, params = aux pc1 params in
    let p2, params = aux pc2 params in
    Option.bind p1 (fun p1 -> Option.bind p2 (fun p2 -> Some (p1, p2))), params
  and param3 : type p q r.
      p param_cons ->
      q param_cons ->
      r param_cons ->
      param list ->
      (p * q * r) option * param list =
   fun pc1 pc2 pc3 params ->
    let p1, params = aux pc1 params in
    let p2, params = aux pc2 params in
    let p3, params = aux pc3 params in
    ( Option.bind p1 (fun p1 ->
          Option.bind p2 (fun p2 ->
              Option.bind p3 (fun p3 -> Some (p1, p2, p3)))),
      params )
  and aux : type p. p param_cons -> param list -> p option * param list =
    function
    | Pos plens -> pos plens.to_fl []
    | Lbl (l, plens) -> lbl l plens.to_fl []
    | Flg f -> flg f []
    | Def (pcons, default, _) -> def default pcons
    | Opt pcons -> opt pcons
    | Lst pconsl -> lst pconsl
    | Etr (cases, _) -> etr cases
    | Map (pcons, l) -> map pcons l.to_fl
    | Pa0 -> no_param
    | Pa2 (pc1, pc2) -> param2 pc1 pc2
    | Pa3 (pc1, pc2, pc3) -> param3 pc1 pc2 pc3
  in
  match aux cons params with
  | None, _ -> Misc.fatal_error "Missing parameter"
  | Some p, params -> p, params

let rec build_param : type p. of_fl_env -> p -> p param_cons -> param list =
 fun env p cons ->
  match cons with
  | Pos vl -> [Positional (vl.of_fl env p)]
  | Lbl (label, vl) -> [Labeled { label; value = vl.of_fl env p }]
  | Flg flag -> [Flag flag]
  | Opt pcons -> (
    match p with None -> [] | Some p -> build_param env p pcons)
  | Def (pcons, default, eq) ->
    if eq p default then [] else build_param env p pcons
  | Lst pconsl -> List.flatten @@ List.map2 (build_param env) p pconsl
  | Etr ([], failure) ->
    failure p;
    []
  | Etr (Case (conv, param_cons) :: cases, f) -> (
    match conv.of_fl env p with
    | None -> build_param env p (Etr (cases, f))
    | Some p -> build_param env p param_cons)
  | Map (pcons, f) -> build_param env (f.of_fl env p) pcons
  | Pa0 -> []
  | Pa2 (pc1, pc2) ->
    let p1, p2 = p in
    build_param env p1 pc1 @ build_param env p2 pc2
  | Pa3 (pc1, pc2, pc3) ->
    let p1, p2, p3 = p in
    build_param env p1 pc1 @ build_param env p2 pc2 @ build_param env p3 pc3

let lens_of_cons (cons : 'p param_cons) : 'p params_lens =
  { of_fl = (fun env p -> build_param env p cons);
    to_fl =
      (fun env params ->
        let ps, rem = extract_param env params cons in
        match rem with
        | [] -> ps
        | _ ->
          Format.eprintf "Unexpected parameter %a\n" Print_fexpr.prim_params rem;
          ps)
  }

let prim_table = Hashtbl.create 32

let register_lens id l =
  Hashtbl.add prim_table id l.to_fl;
  l.of_fl

let lookup_prim p = Hashtbl.find_opt prim_table p.prim

module Describe = struct
  let todo0 s =
    let f _ _ = Misc.fatal_errorf "TODO: %s" s in
    { of_fl = f; to_fl = f }

  let todops s = todo0 ("parameter " ^ s)

  let todop s = Pos (todops s)

  let todo s = register_lens s @@ todo0 s

  let int : int value_lens =
    { of_fl = (fun _ i -> wrap_loc @@ string_of_int i);
      to_fl = (fun _ s -> int_of_string (unwrap_loc s))
    }

  let string : string Fexpr.located value_lens =
    { of_fl = (fun _ s -> s); to_fl = (fun _ s -> s) }

  let diy = string

  let constructor_value (constrs : (string * 'a) list) : 'a value_lens =
    let rec findc c = function
      | [] -> Misc.fatal_error "Undefined constructor"
      | (s, c') :: l -> if Stdlib.( = ) c c' then s else findc c l
    in
    let rec finds s = function
      | [] -> Misc.fatal_error "Undefined constructor"
      | (s', c) :: l -> if String.equal s s' then c else finds s l
    in
    { of_fl = (fun _ c -> wrap_loc @@ findc c constrs);
      to_fl = (fun _ s -> finds (unwrap_loc s) constrs)
    }

  let positional (plens : 'a value_lens) : 'a param_cons = Pos plens

  let labeled label (plens : 'a value_lens) : 'a param_cons = Lbl (label, plens)

  let flag flag : unit param_cons = Flg flag

  let default ~(def : 'p) ?(eq : 'p -> 'p -> bool = Stdlib.( = ))
      (pcons : 'p param_cons) : 'p param_cons =
    Def (pcons, def, eq)

  let option (pcons : 'p param_cons) : 'p option param_cons = Opt pcons

  let list (pconsl : 'p param_cons list) : 'p list param_cons = Lst pconsl

  let either ?(no_match_handler = fun _ -> ()) (cases : 'p case_cons list) :
      'p param_cons =
    Etr (cases, no_match_handler)

  let case ~(box : _ -> 'c -> 'p) ~(unbox : _ -> 'p -> 'c option)
      (param_cons : 'c param_cons) : 'p case_cons =
    Case
      ( { of_fl = unbox;
          to_fl = (fun e -> function Some p -> box e p | None -> assert false)
        },
        param_cons )

  let id_case (param_cons : 'p param_cons) : 'p case_cons =
    Case
      ( { of_fl = (fun _ p -> Some p);
          to_fl = (fun _ -> function Some p -> p | None -> assert false)
        },
        param_cons )

  let maps ~(to_ : of_fl_env -> 'b -> 'a) ~(from : to_fl_env -> 'a -> 'b)
      (pcons : 'a param_cons) : 'b param_cons =
    Map (pcons, { to_fl = from; of_fl = to_ })

  let opt_presence (pcons : unit option param_cons) : bool param_cons =
    maps pcons
      ~to_:(fun _ b -> if b then Some () else None)
      ~from:(fun _ -> Option.is_some)

  let bool_flag f : bool param_cons = opt_presence @@ option @@ flag f

  let constructor_flag ?no_match_handler (flags : (string * 'p) list) :
      'p param_cons =
    either ?no_match_handler
      (List.map
         (fun (f, constr) ->
           case (flag f)
             ~box:(fun _ () -> constr)
             ~unbox:(fun _ p -> if Stdlib.( = ) p constr then Some () else None))
         flags)

  let no_param = Pa0

  let param2 pc1 pc2 = Pa2 (pc1, pc2)

  let param3 pc1 pc2 pc3 = Pa3 (pc1, pc2, pc3)

  let nullary : type p. string -> params:p param_cons -> p cons0 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons params in
    let of_fl env p =
      let params = params.of_fl env p in
      { prim = id; params }
    in
    let to_fl env t args =
      match args with
      | [] -> Misc.fatal_errorf "Primitive %s takes no arguments" id
      | _ ->
        let params = params.to_fl env t.params in
        Flambda_primitive.Nullary (cons env params)
    in
    register_lens id { of_fl; to_fl }

  let unary : type p. string -> params:p param_cons -> p cons1 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons params in
    let of_fl env p =
      let params = params.of_fl env p in
      { prim = id; params }
    in
    let to_fl env t args =
      match args with
      | [arg] ->
        let params = params.to_fl env t.params in
        Flambda_primitive.Unary (cons env params, arg)
      | _ -> Misc.fatal_errorf "Primitive %s takes one argument" id
    in
    register_lens id { of_fl; to_fl }

  let binary : type p. string -> params:p param_cons -> p cons2 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons params in
    let of_fl env p =
      let params = params.of_fl env p in
      { prim = id; params }
    in
    let to_fl env t args =
      match args with
      | [arg1; arg2] ->
        let params = params.to_fl env t.params in
        Flambda_primitive.Binary (cons env params, arg1, arg2)
      | _ -> Misc.fatal_errorf "Primitive %s takes two argument" id
    in
    register_lens id { of_fl; to_fl }

  let ternary : type p. string -> params:p param_cons -> p cons3 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons params in
    let of_fl env p =
      let params = params.of_fl env p in
      { prim = id; params }
    in
    let to_fl env t args =
      match args with
      | [arg1; arg2; arg3] ->
        let params = params.to_fl env t.params in
        Flambda_primitive.Ternary (cons env params, arg1, arg2, arg3)
      | _ -> Misc.fatal_errorf "Primitive %s takes three argument" id
    in
    register_lens id { of_fl; to_fl }

  let quaternary : type p. string -> params:p param_cons -> p cons4 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons params in
    let of_fl env p =
      let params = params.of_fl env p in
      { prim = id; params }
    in
    let to_fl env t args =
      match args with
      | [arg1; arg2; arg3; arg4] ->
        let params = params.to_fl env t.params in
        Flambda_primitive.Quaternary (cons env params, arg1, arg2, arg3, arg4)
      | _ -> Misc.fatal_errorf "Primitive %s takes four argument" id
    in
    register_lens id { of_fl; to_fl }

  let variadic : type p. string -> params:p param_cons -> p consN -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons params in
    let of_fl env p =
      let params = params.of_fl env p in
      { prim = id; params }
    in
    let to_fl env t args =
      let params = params.to_fl env t.params in
      Flambda_primitive.Variadic (cons env params (List.length args), args)
    in
    register_lens id { of_fl; to_fl }
end
