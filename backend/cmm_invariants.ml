(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open Cmm
module V = Backend_var
module VP = Backend_var.With_provenance

(* Check a number of continuation-related invariants *)

module Env : sig
  type t

  val init : unit -> t

  val handler : t -> cont:Static_label.t -> arg_num:int -> t

  val jump : t -> exit_label:Cmm.exit_label -> arg_num:int -> unit

  val report : Format.formatter -> bool
end = struct
  type t = {
    bound_handlers : int Static_label.Map.t;
  }

  type error =
    | Unbound_handler of { cont: Static_label.t }
    | Multiple_handlers of { cont: Static_label.t; }
    | Wrong_arguments_number of
        { cont: Static_label.t; handler_args: int; jump_args: int; }

  module Error = struct
    type t = error

    let compare = Stdlib.compare
  end

  module ErrorSet = Set.Make(Error)

  type persistent_state = {
    mutable all_handlers : Static_label.Set.t;
    mutable errors : ErrorSet.t;
  }

  let state = {
    all_handlers = Static_label.Set.empty;
    errors = ErrorSet.empty;
  }

  let record_error error =
    state.errors <- ErrorSet.add error state.errors

  let unbound_handler cont =
    record_error (Unbound_handler { cont; })

  let multiple_handler cont =
    record_error (Multiple_handlers { cont; })

  let wrong_arguments cont handler_args jump_args =
    record_error (Wrong_arguments_number { cont; handler_args; jump_args; })

  let init () =
    state.all_handlers <- Static_label.Set.empty;
    state.errors <- ErrorSet.empty;
    {
      bound_handlers = Static_label.Map.empty;
    }

  let handler t ~cont ~arg_num =
    if Static_label.Set.mem cont state.all_handlers then multiple_handler cont;
    state.all_handlers <- Static_label.Set.add cont state.all_handlers;
    let bound_handlers = Static_label.Map.add cont arg_num t.bound_handlers in
    { bound_handlers; }

  let jump t ~exit_label ~arg_num =
    match (exit_label : Cmm.exit_label) with
    | Return_lbl -> ()
    | Lbl cont ->
      match Static_label.Map.find cont t.bound_handlers with
      | handler_args ->
        if arg_num <> handler_args then
          wrong_arguments cont handler_args arg_num
      | exception Not_found -> unbound_handler cont

  let print_error ppf error =
    match error with
    | Unbound_handler { cont } ->
      if Static_label.Set.mem cont state.all_handlers then
        Format.fprintf ppf
          "Continuation %a was used outside the scope of its handler"
          Static_label.format cont
      else
        Format.fprintf ppf
          "Continuation %a was used but never bound"
          Static_label.format cont
    | Multiple_handlers { cont; } ->
      Format.fprintf ppf
        "Continuation %a was declared in more than one handler"
        Static_label.format cont
    | Wrong_arguments_number { cont; handler_args; jump_args } ->
      Format.fprintf ppf
        "Continuation %a was declared with %d arguments but called with %d"
        Static_label.format cont
        handler_args
        jump_args

  let print_error_newline ppf error =
    Format.fprintf ppf "%a@." print_error error

  let report ppf =
    if ErrorSet.is_empty state.errors then false
    else begin
      ErrorSet.iter (fun err -> print_error_newline ppf err) state.errors;
      true
    end
end

let rec check env (expr : Cmm.expression) =
  match expr with
  | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
  | Cconst_symbol _ | Cconst_vec128 _ | Cconst_vec256 _ | Cconst_vec512 _
  | Cvar _ | Cinvalid _ ->
    ()
  | Clet (_, expr, body) ->
    check env expr;
    check env body
  | Cphantom_let (_, _, expr) ->
    check env expr
  | Ctuple exprs ->
    List.iter (check env) exprs
  | Cop (_, args, _) ->
    List.iter (check env) args;
  | Csequence (expr1, expr2) ->
    check env expr1;
    check env expr2
  | Cifthenelse (test, _, ifso, _, ifnot, _) ->
    check env test;
    check env ifso;
    check env ifnot
  | Cswitch (body, _, branches, _) ->
    check env body;
    Array.iter (fun (expr, _) -> check env expr) branches
  | Ccatch (flag, handlers, body) ->
    let env_extended =
      List.fold_left
        (fun env Cmm.{label = cont; params = args; _} ->
           Env.handler env ~cont:cont ~arg_num:(List.length args))
        env
        handlers
    in
    check env_extended body;
    let env_handler =
      match flag with
      | Recursive -> env_extended
      | Normal | Exn_handler -> env
    in
    List.iter (fun Cmm.{body = handler; _} -> check env_handler handler) handlers
  | Cexit (exit_label, args, _trap_actions) ->
    Env.jump env ~exit_label ~arg_num:(List.length args)

let run ppf (fundecl : Cmm.fundecl) =
  let env = Env.init () in
  check env fundecl.fun_body;
  Env.report ppf

(* Machtype checking *)

let machtype_of_float_width : float_width -> machtype = function
  | Float64 -> typ_float
  | Float32 -> typ_float32

let machtype_of_vector_width : vector_width -> machtype = function
  | Vec128 -> typ_vec128
  | Vec256 -> typ_vec256
  | Vec512 -> typ_vec512

let machtype_of_vec128_scalar : vec128_type -> machtype = function
  | Float64x2 -> typ_float
  | Float32x4 -> typ_float32
  | Float16x8 -> Misc.fatal_error "float16x8: scalar type not supported"
  | Int8x16 | Int16x8 | Int32x4 | Int64x2 -> typ_int

let machtype_of_vec256_scalar : vec256_type -> machtype = function
  | Float64x4 -> typ_float
  | Float32x8 -> typ_float32
  | Float16x16 -> Misc.fatal_error "float16x16: scalar type not supported"
  | Int8x32 | Int16x16 | Int32x8 | Int64x4 -> typ_int

let machtype_of_vec512_scalar : vec512_type -> machtype = function
  | Float64x8 -> typ_float
  | Float32x16 -> typ_float32
  | Float16x32 -> Misc.fatal_error "float16x32: scalar type not supported"
  | Int8x64 | Int16x32 | Int32x16 | Int64x8 -> typ_int

let reinterpret_cast_arg_type : reinterpret_cast -> machtype = function
  | Int64_of_value -> typ_val
  | Value_of_int64 -> typ_int
  | Float_of_float32 -> typ_float32
  | Float32_of_float -> typ_float
  | Float_of_int64 -> typ_int
  | Int64_of_float -> typ_float
  | Float32_of_int32 -> typ_int
  | Int32_of_float32 -> typ_float32
  | V128_of_vec width | V256_of_vec width | V512_of_vec width ->
    machtype_of_vector_width width

let static_cast_arg_type : static_cast -> machtype = function
  | Float_of_int64 (_ : float_width) -> typ_int
  | Int64_of_float width -> machtype_of_float_width width
  | Float_of_float32 -> typ_float32
  | Float32_of_float -> typ_float
  | V128_of_scalar ty -> machtype_of_vec128_scalar ty
  | Scalar_of_v128 (_ : vec128_type) -> typ_vec128
  | V256_of_scalar ty -> machtype_of_vec256_scalar ty
  | Scalar_of_v256 (_ : vec256_type) -> typ_vec256
  | V512_of_scalar ty -> machtype_of_vec512_scalar ty
  | Scalar_of_v512 (_ : vec512_type) -> typ_vec512

type expected_arg_type =
  | Exactly of machtype
  | Any_machtype
      (** For argument slots whose machtype is not determined by the operation,
          e.g. addresses, which may legitimately be [Val], [Addr] or naked
          pointers outside the heap. *)

type expected_arg_types =
  | Args of expected_arg_type list
      (** Exactly as many arguments as listed, checked slotwise. *)
  | Any_number_of of expected_arg_type
      (** Any number of arguments, each checked against the given
          constraint. *)
  | Args_then_any_number_of of expected_arg_type list * expected_arg_type
      (** At least the listed arguments, checked slotwise, followed by any
          number of arguments checked against the second constraint. *)

(* The machtypes an operation expects for its arguments, in the style of
   [Select_utils.oper_result_type]. *)
let oper_arg_types : operation -> expected_arg_types = function
  | Capply _ ->
    Args_then_any_number_of
      ([Any_machtype (* closure or code pointer *)], Any_machtype)
  | Cextcall { ty_args = []; _ } ->
    (* An empty [ty_args] means "all arguments are machine words". *)
    Any_number_of Any_machtype
  | Cextcall { ty_args = _ :: _ as ty_args; _ } ->
    Args
      (List.map
         (fun (ty_arg : exttype) ->
           match ty_arg with
           | XInt ->
             (* [XInt] is used for values as well as word-sized integers. *)
             Any_machtype
           | XInt8 | XInt16 | XInt32 | XInt64 | XFloat32 | XFloat | XVec128
           | XVec256 | XVec512 ->
             Exactly (machtype_of_exttype ty_arg))
         ty_args)
  | Cload _ -> Args [Any_machtype]
  | Calloc (_, kind) ->
    let field =
      match kind with
      | Alloc_block_kind_other ->
        (* Mixed blocks are also allocated with this kind: fields after the
           scannable prefix may be unboxed, and the prefix length is only
           encoded in the header argument. *)
        Any_machtype
      | Alloc_block_kind_float | Alloc_block_kind_float_array ->
        Exactly typ_float
      | Alloc_block_kind_int_u_array | Alloc_block_kind_int64_u_array ->
        Exactly typ_int
      | Alloc_block_kind_vec128_u_array -> Exactly typ_vec128
      | Alloc_block_kind_vec256_u_array -> Exactly typ_vec256
      | Alloc_block_kind_vec512_u_array -> Exactly typ_vec512
      | Alloc_block_kind_closure | Alloc_block_kind_boxed_int _
      | Alloc_block_kind_float32 | Alloc_block_kind_vec128
      | Alloc_block_kind_vec256 | Alloc_block_kind_vec512
      | Alloc_block_kind_float32_u_array | Alloc_block_kind_int8_u_array
      | Alloc_block_kind_int16_u_array | Alloc_block_kind_int32_u_array ->
        (* Closures and custom blocks mix code pointers, raw words and
           payloads; packed u-arrays mix packed words with a raw last
           element. *)
        Any_machtype
    in
    Args_then_any_number_of ([Any_machtype (* header *)], field)
  | Cstore (memory_chunk, _) ->
    let value =
      match memory_chunk with
      | Word_int ->
        (* A raw word slot is not scanned, so it accepts any word in a
           general-purpose register, including derived pointers. *)
        Exactly typ_addr
      | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
      | Thirtytwo_unsigned | Thirtytwo_signed | Word_val | Single _ | Double
      | Onetwentyeight_unaligned | Onetwentyeight_aligned
      | Twofiftysix_unaligned | Twofiftysix_aligned | Fivetwelve_unaligned
      | Fivetwelve_aligned ->
        Exactly (machtype_of_memory_chunk memory_chunk)
    in
    Args [Any_machtype; value]
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl
  | Clsr | Casr | Ccmpi _ ->
    (* Under [ge_component], [typ_addr] accepts any word in a general-purpose
       register ([Int], [Val] or [Addr]): classic machtypes cannot distinguish
       tagged values from untagged words, and word operations are applied to
       both (e.g. physical equality on values, untagging a scrutinee). *)
    Args [Exactly typ_addr; Exactly typ_addr]
  | Cclz | Cctz | Cpopcnt | Cbswap _ -> Args [Exactly typ_addr]
  | Caddi128 | Csubi128 ->
    Args [Exactly typ_int; Exactly typ_int; Exactly typ_int; Exactly typ_int]
  | Cmuli64 _ -> Args [Exactly typ_int; Exactly typ_int]
  | Ccsel ty -> Args [Exactly typ_int; Exactly ty; Exactly ty]
  | Cprefetch _ -> Args [Any_machtype]
  | Catomic
      { op = Fetch_and_add | Add | Sub | Land | Lor | Lxor | Exchange; _ } ->
    Args [Exactly typ_int; Any_machtype (* address *)]
  | Catomic { op = Compare_set | Compare_exchange; _ } ->
    Args [Exactly typ_int; Exactly typ_int; Any_machtype (* address *)]
  | Caddv -> Args [Exactly typ_val; Exactly typ_int]
  | Cadda -> Args [Exactly typ_addr; Exactly typ_int]
  | Cnegf width | Cabsf width -> Args [Exactly (machtype_of_float_width width)]
  | Caddf width | Csubf width | Cmulf width | Cdivf width ->
    let ty = machtype_of_float_width width in
    Args [Exactly ty; Exactly ty]
  | Cpackf32 ->
    (* Two float32s held in the low bits of float registers. *)
    Args [Exactly typ_float; Exactly typ_float]
  | Ccmpf (width, _) ->
    let ty = machtype_of_float_width width in
    Args [Exactly ty; Exactly ty]
  | Creinterpret_cast cast -> Args [Exactly (reinterpret_cast_arg_type cast)]
  | Cstatic_cast cast -> Args [Exactly (static_cast_arg_type cast)]
  | Craise _ ->
    (* All arguments are GC-scannable; under [ge_component], [typ_val] accepts
       exactly [Val] and [Int]. *)
    Any_number_of (Exactly typ_val)
  | Cprobe _ -> Any_number_of Any_machtype
  | Copaque -> Any_number_of Any_machtype
  | Cprobe_is_enabled _ | Cbeginregion | Cdls_get | Ctls_get | Cdomain_index
  | Cpoll | Cpause ->
    Args []
  | Cendregion -> Args [Exactly typ_int]
  | Ctuple_field (_, fields_ty) ->
    Args [Exactly (Array.concat (Array.to_list fields_ty))]

(* [Never_returns] is for diverging expressions, which do not constrain their
   context, in the same way as [Select_utils.Or_never_returns] in the
   selectors. *)
type inferred_machtype =
  | Machtype of machtype
  | Never_returns

let dbg_suffix dbg =
  if Debuginfo.is_none dbg
  then ""
  else Format.asprintf " at %a" Debuginfo.print_compact dbg

(* [what] is only evaluated on error. *)
let check_machtype ~dbg ~what ~expected (actual : inferred_machtype) =
  match actual with
  | Never_returns -> ()
  | Machtype actual ->
    let compatible =
      Array.length expected = Array.length actual
      && Array.for_all2
           (fun expected_comp actual_comp ->
             (* The compiler types statically-known tagged immediates as [Int]
                while generic loads and calls produce them as [Val], so [Val]
                flows into [Int] positions routinely. *)
             (equal_machtype_component expected_comp Int
             && equal_machtype_component actual_comp Val)
             ||
             (* [ge_component] is fatal on incomparable components *)
             (try ge_component expected_comp actual_comp
              with Misc.Fatal_error -> false))
           expected actual
    in
    if not compatible
    then
      Misc.fatal_errorf
        "Cmm machtype check failed%s: %s has machtype %a but %a was expected"
        (dbg_suffix dbg) (what ()) Printcmm.machtype actual Printcmm.machtype
        expected

let join_inferred_machtypes ~dbg ty1 ty2 =
  match ty1, ty2 with
  | Never_returns, ty | ty, Never_returns -> ty
  | Machtype ty1, Machtype ty2 ->
    if Array.length ty1 <> Array.length ty2
    then
      Misc.fatal_errorf
        "Cmm machtype check failed%s: join of machtype %a and machtype %a, \
         which do not have the same number of components"
        (dbg_suffix dbg) Printcmm.machtype ty1 Printcmm.machtype ty2
    else Machtype (Array.map2 lub_component ty1 ty2)

let join_all_inferred_machtypes ~dbg tys =
  List.fold_left (join_inferred_machtypes ~dbg) Never_returns tys

type typechecking_env =
  { vars : machtype V.Map.t;
    handlers : machtype list Static_label.Map.t;
    return_type : machtype option
  }

let concat_inferred_machtypes tys =
  List.fold_left
    (fun acc ty ->
      match acc, ty with
      | Never_returns, (Machtype _ | Never_returns) | Machtype _, Never_returns
        ->
        Never_returns
      | Machtype tys, Machtype ty -> Machtype (Array.append tys ty))
    (Machtype typ_void) tys

let rec infer_machtype env (expr : expression) : inferred_machtype =
  match expr with
  | Cconst_int _ | Cconst_natint _ | Cconst_symbol _ ->
    (* Integer literals and statically-allocated symbols are non-heap words;
       [Int] is below [Val], so they are also accepted where values are
       expected. *)
    Machtype typ_int
  | Cconst_float _ -> Machtype typ_float
  | Cconst_float32 _ -> Machtype typ_float32
  | Cconst_vec128 _ -> Machtype typ_vec128
  | Cconst_vec256 _ -> Machtype typ_vec256
  | Cconst_vec512 _ -> Machtype typ_vec512
  | Cvar var -> (
    match V.Map.find_opt var env.vars with
    | Some ty -> Machtype ty
    | None ->
      Misc.fatal_errorf "Cmm machtype check failed: unbound variable %a"
        V.print var)
  | Clet (var, defining_expr, body) -> (
    match infer_machtype env defining_expr with
    | Never_returns -> Never_returns
    | Machtype ty ->
      infer_machtype
        { env with vars = V.Map.add (VP.var var) ty env.vars }
        body)
  | Cphantom_let (_, _, body) -> infer_machtype env body
  | Ctuple exprs ->
    concat_inferred_machtypes (List.map (infer_machtype env) exprs)
  | Cop (op, args, dbg) -> typecheck_cop env op args dbg
  | Csequence (expr1, expr2) -> (
    match infer_machtype env expr1 with
    | Never_returns -> Never_returns
    | Machtype _ -> infer_machtype env expr2)
  | Cifthenelse (cond, _, ifso, _, ifnot, dbg) -> (
    let cond_ty = infer_machtype env cond in
    check_machtype ~dbg
      ~what:(fun () -> "if-then-else condition")
      ~expected:typ_int cond_ty;
    let branches_ty =
      join_inferred_machtypes ~dbg (infer_machtype env ifso)
        (infer_machtype env ifnot)
    in
    match cond_ty with
    | Never_returns -> Never_returns
    | Machtype _ -> branches_ty)
  | Cswitch (scrutinee, _, cases, dbg) -> (
    let scrutinee_ty = infer_machtype env scrutinee in
    check_machtype ~dbg
      ~what:(fun () -> "switch scrutinee")
      ~expected:typ_int scrutinee_ty;
    let cases_ty =
      join_all_inferred_machtypes ~dbg
        (Array.to_list
           (Array.map
              (fun (case, _) -> infer_machtype env case)
              cases))
    in
    match scrutinee_ty with
    | Never_returns -> Never_returns
    | Machtype _ -> cases_ty)
  | Ccatch (_, handlers, body) ->
    let env =
      { env with
        handlers =
          List.fold_left
            (fun handlers handler ->
              Static_label.Map.add handler.label
                (List.map snd handler.params)
                handlers)
            env.handlers handlers
      }
    in
    let body_ty = infer_machtype env body in
    let handler_tys =
      List.map
        (fun (handler : static_handler) ->
          let env =
            { env with
              vars =
                List.fold_left
                  (fun vars (var, ty) -> V.Map.add (VP.var var) ty vars)
                  env.vars handler.params
            }
          in
          infer_machtype env handler.body)
        handlers
    in
    join_all_inferred_machtypes ~dbg:Debuginfo.none (body_ty :: handler_tys)
  | Cexit (label, args, _) ->
    (* Exit arguments are matched to handler parameters by machtype component,
       not one-to-one: a single argument of a product machtype can fill
       several parameters. *)
    let args_ty =
      concat_inferred_machtypes (List.map (infer_machtype env) args)
    in
    (match label with
    | Return_lbl -> (
      match env.return_type with
      | None -> ()
      | Some expected ->
        check_machtype ~dbg:Debuginfo.none
          ~what:(fun () -> "return")
          ~expected args_ty)
    | Lbl label -> (
      match Static_label.Map.find_opt label env.handlers with
      | None ->
        (* Exits to out-of-scope handlers are reported by [run]. *)
        ()
      | Some param_tys ->
        check_machtype ~dbg:Debuginfo.none
          ~expected:(Array.concat param_tys)
          args_ty
          ~what:(fun () ->
            Format.asprintf "arguments of exit to %a" Static_label.format
              label)));
    Never_returns
  | Cinvalid _ -> Never_returns

and typecheck_cop env op args dbg : inferred_machtype =
  let arg_tys = List.map (infer_machtype env) args in
  let check_arg arg_index expected actual =
    match expected with
    | Any_machtype -> ()
    | Exactly expected ->
      check_machtype ~dbg ~expected actual ~what:(fun () ->
          Printf.sprintf "argument %d of operation %s" (arg_index + 1)
            (Printcmm.operation dbg op))
  in
  let arity_error expected_arity =
    Misc.fatal_errorf
      "Cmm machtype check failed at %a: operation %s expects %s arguments but \
       is given %d"
      Debuginfo.print_compact dbg
      (Printcmm.operation dbg op)
      expected_arity (List.length args)
  in
  (match oper_arg_types op with
  | Args expected_tys ->
    if List.compare_lengths expected_tys args <> 0
    then arity_error (string_of_int (List.length expected_tys));
    List.iteri
      (fun arg_index (expected, actual) -> check_arg arg_index expected actual)
      (List.combine expected_tys arg_tys)
  | Any_number_of expected ->
    List.iteri (fun arg_index actual -> check_arg arg_index expected actual)
      arg_tys
  | Args_then_any_number_of (expected_prefix, expected_rest) ->
    if List.compare_lengths expected_prefix args > 0
    then
      arity_error
        (Printf.sprintf "at least %d" (List.length expected_prefix));
    List.iteri
      (fun arg_index actual ->
        let expected =
          match List.nth_opt expected_prefix arg_index with
          | Some expected -> expected
          | None -> expected_rest
        in
        check_arg arg_index expected actual)
      arg_tys);
  if
    List.exists
      (function Never_returns -> true | Machtype _ -> false)
      arg_tys
  then Never_returns
  else
    match op with
    | Craise _
    | Cextcall { returns = false; _ } ->
      Never_returns
    | Copaque -> (
      (* [Copaque] is the identity whatever the machtype of its argument;
         [oper_result_type] approximates its result as [typ_val]. *)
      match arg_tys with
      | [ty] -> ty
      | [] | _ :: _ :: _ -> arity_error "1")
    | Cextcall { returns = true; _ }
    | Capply _
    | Cload _ | Calloc _ | Cstore _ | Caddi | Csubi | Cmuli
    | Cmulhi _ | Cdivi | Cmodi | Caddi128 | Csubi128 | Cmuli64 _ | Cand | Cor
    | Cxor | Clsl | Clsr | Casr | Cbswap _ | Ccsel _ | Cclz | Cctz | Cpopcnt
    | Cprefetch _ | Catomic _ | Ccmpi _ | Caddv | Cadda | Cnegf _ | Cabsf _
    | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32 | Creinterpret_cast _
    | Cstatic_cast _ | Ccmpf _ | Cprobe _ | Cprobe_is_enabled _ | Cbeginregion
    | Cendregion | Ctuple_field _ | Cdls_get | Ctls_get | Cdomain_index | Cpoll
    | Cpause ->
      Machtype (Select_utils.oper_result_type op)

let check_machtypes (fundecl : fundecl) =
  let vars =
    List.fold_left
      (fun vars (var, ty) -> V.Map.add (VP.var var) ty vars)
      V.Map.empty fundecl.fun_args
  in
  let env =
    { vars;
      handlers = Static_label.Map.empty;
      return_type = Some fundecl.fun_ret_type
    }
  in
  check_machtype ~dbg:fundecl.fun_dbg ~expected:fundecl.fun_ret_type
    ~what:(fun () -> Printf.sprintf "the body of %s" fundecl.fun_name.sym_name)
    (infer_machtype env fundecl.fun_body)
