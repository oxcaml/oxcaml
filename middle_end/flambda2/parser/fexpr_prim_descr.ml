type param = Fexpr.prim_param =
  | Labeled of string Fexpr.located * param list
  | Anonymous of param list

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

type ('p, 'args) labeled_lens =
  ('p, string Fexpr.located * 'args, 'p option) lens

type 'p params_lens = ('p, param list) map_lens

type 'p prim_lens = ('p, t, Simple.t list -> Flambda_primitive.t) lens

type 'p conv = encode_env -> 'p -> t

type _ param_cons =
  | CVoid : unit param_cons
  | CAtom : 'p param_cons -> 'p param_cons
  | CLazy : 'p param_cons Lazy.t -> 'p param_cons
  | CLabeled : ('p, 'a) labeled_lens * 'a param_cons -> 'p param_cons
  | COptional : 'p param_cons -> 'p option param_cons
  | CDefault : 'p param_cons * 'p * ('p -> 'p -> bool) -> 'p param_cons
  | CEither :
      (encode_env -> 'p -> param list) * 'p case_cons list
      -> 'p param_cons
  | CMap : 'a param_cons * ('b, 'a) map_lens -> 'b param_cons
  | CList : 'a param_cons -> 'a list param_cons
  | CParam2 : 'a param_cons * 'b param_cons -> ('a * 'b) param_cons
  | CParam3 :
      'a param_cons * 'b param_cons * 'c param_cons
      -> ('a * 'b * 'c) param_cons
  | CParam4 :
      'a param_cons * 'b param_cons * 'c param_cons * 'd param_cons
      -> ('a * 'b * 'c * 'd) param_cons
  | CParam5 :
      'a param_cons
      * 'b param_cons
      * 'c param_cons
      * 'd param_cons
      * 'e param_cons
      -> ('a * 'b * 'c * 'd * 'e) param_cons

and 'p case_cons =
  | Case : 'a param_cons * (decode_env -> 'a -> 'p) -> 'p case_cons

let wrap_loc txt = Fexpr.{ txt; loc = Debuginfo.Scoped_location.Loc_unknown }

let unwrap_loc located = located.Fexpr.txt

(* Parsing fexpr parameters. Low-brow iteration through constructors in
   declaration order, consuming the param list on match. *)
let extract_param (env : decode_env) (params : param list)
    (cons : 'p param_cons) : ('p * param list) option =
  let void params = Some ((), params) in
  let rec atom : type p.
      p param_cons -> param list -> param list -> (p * param list) option =
   fun cons lp params ->
    match params with
    | [] -> None
    | (Labeled _ as p) :: rp -> atom cons (p :: lp) rp
    | Anonymous ps :: rp -> (
      match aux cons ps with
      | None | Some (_, _ :: _) -> None
      | Some (p, []) -> Some (p, List.rev_append lp rp))
  and lbl : type p a.
      (p, a) labeled_lens ->
      a param_cons ->
      param list ->
      param list ->
      (p * param list) option =
   fun lens cons lp params ->
    match params with
    | [] -> None
    | (Anonymous _ as p) :: rp -> lbl lens cons (p :: lp) rp
    | (Labeled (label, args) as p) :: rp -> (
      match aux cons args with
      | None | Some (_, _ :: _) -> lbl lens cons (p :: lp) rp
      | Some (args, []) ->
        Option.map
          (fun p -> p, List.rev_append lp rp)
          (lens.decode env (label, args)))
  and def : type p. p -> p param_cons -> param list -> (p * param list) option =
   fun d cons params ->
    match aux cons params with None -> Some (d, params) | found -> found
  and opt : type p. p param_cons -> param list -> (p option * param list) option
      =
   fun cons params ->
    match aux cons params with
    | None -> Some (None, params)
    | Some (p, params) -> Some (Some p, params)
  and etr : type p. p case_cons list -> param list -> (p * param list) option =
   fun cases params ->
    match cases with
    | [] -> None
    | Case (param_cons, decode) :: cases -> (
      match aux param_cons params with
      | Some (p, params) -> Some (decode env p, params)
      | None -> etr cases params)
  and list : type a.
      a param_cons -> a list -> param list -> (a list * param list) option =
   fun cons acc params ->
    match aux cons params with
    | None -> Some (List.rev acc, params) (* Empty list always matchable *)
    | Some (e, params) -> list cons (e :: acc) params
  and map : type a b.
      a param_cons ->
      (decode_env -> a -> b) ->
      param list ->
      (b * param list) option =
   fun cons f params ->
    Option.map (fun (p, params) -> f env p, params) (aux cons params)
  and param2 : type p q.
      p param_cons ->
      q param_cons ->
      param list ->
      ((p * q) * param list) option =
   fun pc1 pc2 params ->
    Option.bind (aux pc1 params) (fun (p1, params) ->
        Option.bind (aux pc2 params) (fun (p2, params) ->
            Some ((p1, p2), params)))
  and param3 : type p q r.
      p param_cons ->
      q param_cons ->
      r param_cons ->
      param list ->
      ((p * q * r) * param list) option =
   fun pc1 pc2 pc3 params ->
    Option.bind (aux pc1 params) (fun (p1, params) ->
        Option.bind (aux pc2 params) (fun (p2, params) ->
            Option.bind (aux pc3 params) (fun (p3, params) ->
                Some ((p1, p2, p3), params))))
  and param4 : type p q r s.
      p param_cons ->
      q param_cons ->
      r param_cons ->
      s param_cons ->
      param list ->
      ((p * q * r * s) * param list) option =
   fun pc1 pc2 pc3 pc4 params ->
    Option.bind (aux pc1 params) (fun (p1, params) ->
        Option.bind (aux pc2 params) (fun (p2, params) ->
            Option.bind (aux pc3 params) (fun (p3, params) ->
                Option.bind (aux pc4 params) (fun (p4, params) ->
                    Some ((p1, p2, p3, p4), params)))))
  and param5 : type p q r s t.
      p param_cons ->
      q param_cons ->
      r param_cons ->
      s param_cons ->
      t param_cons ->
      param list ->
      ((p * q * r * s * t) * param list) option =
   fun pc1 pc2 pc3 pc4 pc5 params ->
    Option.bind (aux pc1 params) (fun (p1, params) ->
        Option.bind (aux pc2 params) (fun (p2, params) ->
            Option.bind (aux pc3 params) (fun (p3, params) ->
                Option.bind (aux pc4 params) (fun (p4, params) ->
                    Option.bind (aux pc5 params) (fun (p5, params) ->
                        Some ((p1, p2, p3, p4, p5), params))))))
  and aux : type p. p param_cons -> param list -> (p * param list) option =
    function
    | CLabeled (llens, pc) -> lbl llens pc []
    | CDefault (pcons, default, _) -> def default pcons
    | COptional pcons -> opt pcons
    | CEither (_, cases) -> etr cases
    | CMap (pcons, l) -> map pcons l.decode
    | CList pcons -> list pcons []
    | CVoid -> void
    | CAtom pc -> atom pc []
    | CLazy l -> aux (Lazy.force l)
    | CParam2 (pc1, pc2) -> param2 pc1 pc2
    | CParam3 (pc1, pc2, pc3) -> param3 pc1 pc2 pc3
    | CParam4 (pc1, pc2, pc3, pc4) -> param4 pc1 pc2 pc3 pc4
    | CParam5 (pc1, pc2, pc3, pc4, pc5) -> param5 pc1 pc2 pc3 pc4 pc5
  in
  aux cons params

let rec build_param : type p. encode_env -> p -> p param_cons -> param list =
 fun env p cons ->
  match cons with
  | CVoid -> []
  | CLazy l -> build_param env p (Lazy.force l)
  | CLabeled (lens, pcons) ->
    let label, args = lens.encode env p in
    let args = build_param env args pcons in
    [Labeled (label, args)]
  | CAtom pcons -> [Anonymous (build_param env p pcons)]
  | COptional pcons -> (
    match p with None -> [] | Some p -> build_param env p pcons)
  | CDefault (pcons, default, eq) ->
    if eq p default then [] else build_param env p pcons
  | CEither (encode, _) -> encode env p
  | CList pcons -> List.concat_map (fun pe -> build_param env pe pcons) p
  | CMap (pcons, f) -> build_param env (f.encode env p) pcons
  | CParam2 (pc1, pc2) ->
    let p1, p2 = p in
    build_param env p1 pc1 @ build_param env p2 pc2
  | CParam3 (pc1, pc2, pc3) ->
    let p1, p2, p3 = p in
    build_param env p1 pc1 @ build_param env p2 pc2 @ build_param env p3 pc3
  | CParam4 (pc1, pc2, pc3, pc4) ->
    let p1, p2, p3, p4 = p in
    build_param env p1 pc1 @ build_param env p2 pc2 @ build_param env p3 pc3
    @ build_param env p4 pc4
  | CParam5 (pc1, pc2, pc3, pc4, pc5) ->
    let p1, p2, p3, p4, p5 = p in
    build_param env p1 pc1 @ build_param env p2 pc2 @ build_param env p3 pc3
    @ build_param env p4 pc4 @ build_param env p5 pc5

let lens_of_cons id (cons : 'p param_cons) : 'p params_lens =
  { encode = (fun env p -> build_param env p cons);
    decode =
      (fun env params ->
        match extract_param env params cons with
        | None ->
          Misc.fatal_errorf "Missing parameter for@ %s%a" id
            Print_fexpr.prim_params params
        | Some (ps, rem) -> (
          match rem with
          | [] -> ps
          | _ ->
            Format.eprintf "Unexpected parameter %a@ in %a\n"
              Print_fexpr.prim_params rem Print_fexpr.prim_params params;
            ps))
  }

let prim_table = Hashtbl.create 32

let register_lens id l =
  if not @@ String.starts_with ~prefix:"%" id
  then Misc.fatal_errorf "Registered primitive '%s' does not start with %%." id;
  Hashtbl.add prim_table id l.decode;
  l.encode

let lookup_prim p = Hashtbl.find_opt prim_table p.prim

module Describe = struct
  let todo0 s =
    let f _ _ = Misc.fatal_errorf "TODO: %s" s in
    { encode = f; decode = f }

  let todops s = todo0 ("parameter " ^ s)

  let todo s = register_lens s @@ todo0 s

  let value (vlens : 'a value_lens) : 'a param_cons =
    let lens =
      { encode = (fun env p -> vlens.encode env p, ());
        decode = (fun env (l, ()) -> Some (vlens.decode env l))
      }
    in
    CLabeled (lens, CVoid)

  let todop s = value (todops s)

  let int : int param_cons =
    value
      { encode = (fun _ i -> wrap_loc @@ string_of_int i);
        decode = (fun _ s -> int_of_string (unwrap_loc s))
      }

  let bool : bool param_cons =
    value
      { encode = (fun _ i -> wrap_loc @@ string_of_bool i);
        decode = (fun _ s -> bool_of_string (unwrap_loc s))
      }

  let string : string Fexpr.located param_cons =
    value { encode = (fun _ s -> s); decode = (fun _ s -> s) }

  let diy = string

  let constructor_flag ?no_match_handler (constrs : (string * 'a) list) :
      'a param_cons =
    let rec findc c = function
      | [] ->
        Option.iter (( |> ) c) no_match_handler;
        Misc.fatal_error "Undefined constructor"
      | (s, c') :: l -> if Stdlib.( = ) c c' then s else findc c l
    in
    let rec finds s = function
      | [] -> None
      | (s', c) :: l -> if String.equal s s' then Some c else finds s l
    in
    let lens =
      { encode = (fun _ c -> wrap_loc @@ findc c constrs, ());
        decode = (fun _ (s, ()) -> finds (unwrap_loc s) constrs)
      }
    in
    CLabeled (lens, CVoid)

  let positional (type a) (pcons : a param_cons) : a param_cons =
    (* We do not want to default out positional parameters. We can still twist
       constructors to end with a deeper default, but this should avoid most
       cases as defaults tend to be at root level *)
    match pcons with
    | CDefault (_pcons, _, _) ->
      Misc.fatal_error "Positional parameter does not support defaulting values"
    | CVoid | CAtom _ | CLazy _ | CLabeled _ | COptional _ | CEither _ | CList _ | CMap _
    | CParam2 _ | CParam3 _ | CParam4 _ | CParam5 _ ->
      CAtom pcons

  let labeled label (pcons : 'a param_cons) : 'a param_cons =
    let lens =
      { encode = (fun _ p -> wrap_loc label, p);
        decode =
          (fun _ (l, p) ->
            if String.equal label (unwrap_loc l) then Some p else None)
      }
    in
    CLabeled (lens, pcons)

  let flag flag : unit param_cons = labeled flag CVoid

  let default ~(def : 'p) ?(eq : 'p -> 'p -> bool = Stdlib.( = ))
      (pcons : 'p param_cons) : 'p param_cons =
    CDefault (pcons, def, eq)

  let option (pcons : 'p param_cons) : 'p option param_cons = COptional pcons

  let list (pcons : 'p param_cons) : 'p list param_cons =
    (* Atomize the list to render explicitely the empty list and avoid
       interleaving of other values *)
    CAtom (CList pcons)

  type 'p encode_case = encode_env -> 'p -> param list

  type 'p build_either = 'p case_cons list -> 'p case_cons list * 'p encode_case

  let bind_case (cases : 'p case_cons list) (cons : 'case param_cons)
      (box : decode_env -> 'case -> 'p) :
      'p case_cons list * (encode_env -> 'case -> param list) =
    let case = Case (cons, box) in
    let encode env p = build_param env p cons in
    case :: cases, encode

  let build_match (cases : 'p case_cons list) (destruct_param : 'p encode_case)
      : 'p param_cons =
    CEither (destruct_param, List.rev cases)

  let return_either (p : 'p encode_case) : 'p build_either =
   fun cases -> cases, p

  let ( let| ) ((cons, box) : 'case param_cons * (decode_env -> 'case -> 'p))
      (f : 'case encode_case -> 'p build_either) : 'p build_either =
   fun cases ->
    let cases, encode = bind_case cases cons box in
    f encode cases

  let ( let|= ) (cb : 'p build_either) (f : 'p param_cons -> 'a) : 'a =
    let cases, destruct_param = cb [] in
    let p = build_match cases destruct_param in
    f p

  let maps ~(to_ : encode_env -> 'b -> 'a) ~(from : decode_env -> 'a -> 'b)
      (pcons : 'a param_cons) : 'b param_cons =
    CMap (pcons, { decode = from; encode = to_ })

  let opt_presence (pcons : unit option param_cons) : bool param_cons =
    maps pcons
      ~to_:(fun _ b -> if b then Some () else None)
      ~from:(fun _ -> Option.is_some)

  let bool_flag f : bool param_cons = opt_presence @@ option @@ flag f

  let param0 = CVoid

  let param2 pc1 pc2 = CParam2 (pc1, pc2)

  let param3 pc1 pc2 pc3 = CParam3 (pc1, pc2, pc3)

  let param4 pc1 pc2 pc3 pc4 = CParam4 (pc1, pc2, pc3, pc4)

  let param5 pc1 pc2 pc3 pc4 pc5 = CParam5 (pc1, pc2, pc3, pc4, pc5)

  let flag_case flg constr =
    (flag flg, fun _ () -> constr)

  let param2_case ~decode p1 p2 =
    param2 p1 p2, fun env (c1, c2) -> decode env c1 c2

  let param3_case ~decode p1 p2 p3 =
    param3 p1 p2 p3, fun env (c1, c2, c3) -> decode env c1 c2 c3

  let param4_case ~decode p1 p2 p3 p4 =
    param4 p1 p2 p3 p4, fun env (c1, c2, c3, c4) -> decode env c1 c2 c3 c4

  let param5_case ~decode p1 p2 p3 p4 p5 =
    ( param5 p1 p2 p3 p4 p5,
      fun env (c1, c2, c3, c4, c5) -> decode env c1 c2 c3 c4 c5 )

  let recursive_pattern fcons =
    let rec pat = lazy (fcons (CLazy pat)) in Lazy.force pat

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
