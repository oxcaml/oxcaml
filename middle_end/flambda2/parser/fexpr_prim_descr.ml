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

type decode_env = Fexpr_to_flambda_commons.env

type encode_env = Flambda_to_fexpr_commons.Env.t

type 'p cons0 = decode_env -> 'p -> Flambda_primitive.nullary_primitive

type 'p cons1 = decode_env -> 'p -> Flambda_primitive.unary_primitive

type 'p cons2 = decode_env -> 'p -> Flambda_primitive.binary_primitive

type 'p cons3 = decode_env -> 'p -> Flambda_primitive.ternary_primitive

type 'p cons4 = decode_env -> 'p -> Flambda_primitive.quaternary_primitive

type 'p consN = decode_env -> 'p -> int -> Flambda_primitive.variadic_primitive

type ('p, 't, 'r) lens =
  { encode : encode_env -> 'p -> 't;
    decode : decode_env -> 't -> 'r
  }

type ('a, 'b) map_lens = ('a, 'b, 'a) lens

type 'p value_lens = ('p, string Fexpr.located) map_lens

type 'p params_lens = ('p, param list) map_lens

type 'p prim_lens = ('p, t, Simple.t list -> Flambda_primitive.t) lens

type 'p conv = encode_env -> 'p -> t

type _ param_cons =
  | CPositional : 'p value_lens -> 'p param_cons
  | CLabeled : string * 'p value_lens -> 'p param_cons
  | CFlag : string -> unit param_cons
  | COptional : 'p param_cons -> 'p option param_cons
  | CDefault : 'p param_cons * 'p * ('p -> 'p -> bool) -> 'p param_cons
  | CEither : 'p case_cons list * ('p -> unit) -> 'p param_cons
  | CMap : 'a param_cons * ('b, 'a) map_lens -> 'b param_cons
  | CParam0 : unit param_cons
  | CParam2 : 'a param_cons * 'b param_cons -> ('a * 'b) param_cons
  | CParam3 :
      'a param_cons * 'b param_cons * 'c param_cons
      -> ('a * 'b * 'c) param_cons

and 'p case_cons =
  | Case : ('p, 'c option, 'p) lens * 'c param_cons -> 'p case_cons

let wrap_loc txt = Fexpr.{ txt; loc = Debuginfo.Scoped_location.Loc_unknown }

let unwrap_loc located = located.Fexpr.txt

(* Parsing fexpr parameters. Low-brow iteration through constructors in
   declaration order, consuming the param list on match. *)
let extract_param (env : decode_env) (params : param list)
    (cons : 'p param_cons) : 'p option * param list =
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
  let param0 params = Some (), params in
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
  and etr : type p. p case_cons list -> param list -> p option * param list =
   fun cases params ->
    match cases with
    | [] -> None, params
    | Case (conv, param_cons) :: cases -> (
      match aux param_cons params with
      | Some p, params -> Some (conv.decode env (Some p)), params
      | None, _ -> etr cases params)
  and map : type a b.
      a param_cons ->
      (decode_env -> a -> b) ->
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
    | CPositional plens -> pos plens.decode []
    | CLabeled (l, plens) -> lbl l plens.decode []
    | CFlag f -> flg f []
    | CDefault (pcons, default, _) -> def default pcons
    | COptional pcons -> opt pcons
    | CEither (cases, _) -> etr cases
    | CMap (pcons, l) -> map pcons l.decode
    | CParam0 -> param0
    | CParam2 (pc1, pc2) -> param2 pc1 pc2
    | CParam3 (pc1, pc2, pc3) -> param3 pc1 pc2 pc3
  in
  aux cons params

let rec build_param : type p. encode_env -> p -> p param_cons -> param list =
 fun env p cons ->
  match cons with
  | CPositional vl -> [Positional (vl.encode env p)]
  | CLabeled (label, vl) -> [Labeled { label; value = vl.encode env p }]
  | CFlag flag -> [Flag flag]
  | COptional pcons -> (
    match p with None -> [] | Some p -> build_param env p pcons)
  | CDefault (pcons, default, eq) ->
    if eq p default then [] else build_param env p pcons
  | CEither ([], failure) ->
    failure p;
    []
  | CEither (Case (conv, param_cons) :: cases, f) -> (
    match conv.encode env p with
    | None -> build_param env p (CEither (cases, f))
    | Some p -> build_param env p param_cons)
  | CMap (pcons, f) -> build_param env (f.encode env p) pcons
  | CParam0 -> []
  | CParam2 (pc1, pc2) ->
    let p1, p2 = p in
    build_param env p1 pc1 @ build_param env p2 pc2
  | CParam3 (pc1, pc2, pc3) ->
    let p1, p2, p3 = p in
    build_param env p1 pc1 @ build_param env p2 pc2 @ build_param env p3 pc3

let lens_of_cons id (cons : 'p param_cons) : 'p params_lens =
  { encode = (fun env p -> build_param env p cons);
    decode =
      (fun env params ->
        let ps, rem = extract_param env params cons in
        match ps with
        | None -> Misc.fatal_errorf "Missing parameter for %s" id
        | Some ps -> (
          match rem with
          | [] -> ps
          | _ ->
            Format.eprintf "Unexpected parameter %a\n" Print_fexpr.prim_params
              rem;
            ps))
  }

let prim_table = Hashtbl.create 32

let register_lens id l =
  if not @@ String.starts_with ~prefix:"%" id
  then Misc.fatal_errorf "Registered primitive '%s' do not start with %%." id;
  Hashtbl.add prim_table id l.decode;
  l.encode

let lookup_prim p = Hashtbl.find_opt prim_table p.prim

module Describe = struct
  let todo0 s =
    let f _ _ = Misc.fatal_errorf "TODO: %s" s in
    { encode = f; decode = f }

  let todops s = todo0 ("parameter " ^ s)

  let todop s = CPositional (todops s)

  let todo s = register_lens s @@ todo0 s

  let int : int value_lens =
    { encode = (fun _ i -> wrap_loc @@ string_of_int i);
      decode = (fun _ s -> int_of_string (unwrap_loc s))
    }

  let string : string Fexpr.located value_lens =
    { encode = (fun _ s -> s); decode = (fun _ s -> s) }

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
    { encode = (fun _ c -> wrap_loc @@ findc c constrs);
      decode = (fun _ s -> finds (unwrap_loc s) constrs)
    }

  let positional (plens : 'a value_lens) : 'a param_cons = CPositional plens

  let labeled label (plens : 'a value_lens) : 'a param_cons =
    CLabeled (label, plens)

  let flag flag : unit param_cons = CFlag flag

  let default ~(def : 'p) ?(eq : 'p -> 'p -> bool = Stdlib.( = ))
      (pcons : 'p param_cons) : 'p param_cons =
    CDefault (pcons, def, eq)

  let option (pcons : 'p param_cons) : 'p option param_cons = COptional pcons

  let either ?(no_match_handler = fun _ -> ()) (cases : 'p case_cons list) :
      'p param_cons =
    CEither (cases, no_match_handler)

  let case ~(box : _ -> 'c -> 'p) ~(unbox : _ -> 'p -> 'c option)
      (param_cons : 'c param_cons) : 'p case_cons =
    Case
      ( { encode = unbox;
          decode =
            (fun e -> function Some p -> box e p | None -> assert false)
        },
        param_cons )

  let id_case (param_cons : 'p param_cons) : 'p case_cons =
    Case
      ( { encode = (fun _ p -> Some p);
          decode = (fun _ -> function Some p -> p | None -> assert false)
        },
        param_cons )

  let maps ~(to_ : encode_env -> 'b -> 'a) ~(from : decode_env -> 'a -> 'b)
      (pcons : 'a param_cons) : 'b param_cons =
    CMap (pcons, { decode = from; encode = to_ })

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

  let param0 = CParam0

  let param2 pc1 pc2 = CParam2 (pc1, pc2)

  let param3 pc1 pc2 pc3 = CParam3 (pc1, pc2, pc3)

  let nullary : type p. string -> params:p param_cons -> p cons0 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons id params in
    let encode env p =
      let params = params.encode env p in
      { prim = id; params }
    in
    let decode env t args =
      match args with
      | [] ->
        let params = params.decode env t.params in
        Flambda_primitive.Nullary (cons env params)
      | _ -> Misc.fatal_errorf "Primitive %s takes no arguments" id
    in
    register_lens id { encode; decode }

  let unary : type p. string -> params:p param_cons -> p cons1 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons id params in
    let encode env p =
      let params = params.encode env p in
      { prim = id; params }
    in
    let decode env t args =
      match args with
      | [arg] ->
        let params = params.decode env t.params in
        Flambda_primitive.Unary (cons env params, arg)
      | _ -> Misc.fatal_errorf "Primitive %s takes one argument" id
    in
    register_lens id { encode; decode }

  let binary : type p. string -> params:p param_cons -> p cons2 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons id params in
    let encode env p =
      let params = params.encode env p in
      { prim = id; params }
    in
    let decode env t args =
      match args with
      | [arg1; arg2] ->
        let params = params.decode env t.params in
        Flambda_primitive.Binary (cons env params, arg1, arg2)
      | _ -> Misc.fatal_errorf "Primitive %s takes two argument" id
    in
    register_lens id { encode; decode }

  let ternary : type p. string -> params:p param_cons -> p cons3 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons id params in
    let encode env p =
      let params = params.encode env p in
      { prim = id; params }
    in
    let decode env t args =
      match args with
      | [arg1; arg2; arg3] ->
        let params = params.decode env t.params in
        Flambda_primitive.Ternary (cons env params, arg1, arg2, arg3)
      | _ -> Misc.fatal_errorf "Primitive %s takes three argument" id
    in
    register_lens id { encode; decode }

  let quaternary : type p. string -> params:p param_cons -> p cons4 -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons id params in
    let encode env p =
      let params = params.encode env p in
      { prim = id; params }
    in
    let decode env t args =
      match args with
      | [arg1; arg2; arg3; arg4] ->
        let params = params.decode env t.params in
        Flambda_primitive.Quaternary (cons env params, arg1, arg2, arg3, arg4)
      | _ -> Misc.fatal_errorf "Primitive %s takes four argument" id
    in
    register_lens id { encode; decode }

  let variadic : type p. string -> params:p param_cons -> p consN -> p conv =
   fun id ~params cons ->
    let params = lens_of_cons id params in
    let encode env p =
      let params = params.encode env p in
      { prim = id; params }
    in
    let decode env t args =
      let params = params.decode env t.params in
      Flambda_primitive.Variadic (cons env params (List.length args), args)
    in
    register_lens id { encode; decode }
end
