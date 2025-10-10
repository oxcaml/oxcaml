(******************************************************************************
 *                                  OxCaml                                    *
 *                      Andrej Ivaskovic, Jane Street                         *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* CR metaprogramming aivaskovic: This file has not been code reviewed *)

open Asttypes
open Lambda
open Misc
open Typedtree
open Types
open Debuginfo.Scoped_location
open Longident

let camlinternalQuote =
  lazy
    (match
       Env.open_pers_signature "CamlinternalQuote" (Lazy.force Env.initial)
     with
    | exception Not_found -> fatal_error "Module CamlinternalQuote unavailable."
    | path, _, env -> path, env)

let combinator modname field =
  lazy
    (let _, env = Lazy.force camlinternalQuote in
     let lid =
       match unflatten (String.split_on_char '.' modname) with
       | None -> Lident field
       | Some lid -> Ldot (lid, field)
     in
     match Env.find_value_by_name lid env with
     | p, _ -> transl_value_path Loc_unknown env p
     | exception Not_found ->
       fatal_error
         ("Primitive CamlinternalQuote." ^ modname ^ "." ^ field ^ " not found."))

(* Strings and booleans *)

let string loc s = Lconst (Const_base (Const_string (s, loc, None)))

let true_ = Lconst (Const_base (Const_int 1))

let false_ = Lconst (Const_base (Const_int 0))

let quote_bool b = if b then true_ else false_

(* Lambdas that represent the option type *)

let none = Lconst (Const_base (Const_int 0))

let some x =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x], Loc_unknown)

let option opt = match opt with None -> none | Some x -> some x

let string_option loc s = option (Option.map (string loc) s)

(* Lambdas for lists and tuples *)

let nil = Lconst (Const_base (Const_int 0))

let cons hd tl =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [hd; tl], Loc_unknown)

let hd l = Lprim (Pfield (0, Immediate, Reads_vary), [l], Loc_unknown)

let tl l = Lprim (Pfield (1, Immediate, Reads_vary), [l], Loc_unknown)

let rec mk_list list =
  match list with [] -> nil | hd :: tl -> cons hd (mk_list tl)

let pair (x, y) =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x; y], Loc_unknown)

let triple (x, y, z) =
  Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x; y; z], Loc_unknown)

(* Let-expressions *)

let bind id def body =
  Llet
    ( Strict,
      Pvalue { raw_kind = Pgenval; nullable = Non_nullable },
      id,
      debug_uid_none,
      def,
      body )

(* Typed representation of complex lambdas *)

module rec Var : sig
  module Module : sig
    type s

    type t = s Lam.t

    val mk : lambda -> t
  end

  module Value : sig
    type s

    type t = s Lam.t

    val mk : lambda -> t
  end

  module Type_constr : sig
    type s

    type t = s Lam.t

    val mk : lambda -> t
  end

  module Type_var : sig
    type s

    type t = s Lam.t

    val mk : lambda -> t
  end
end = struct
  module Module = struct
    type s = lambda

    type t = s Lam.t

    let mk x = x
  end

  module Value = struct
    type s = lambda

    type t = s Lam.t

    let mk x = x
  end

  module Type_constr = struct
    type s = lambda

    type t = s Lam.t

    let mk x = x
  end

  module Type_var = struct
    type s = lambda

    type t = s Lam.t

    let mk x = x
  end
end

and Lam : sig
  type 'a t

  type 'a param =
    | Var_module : Var.Module.t param
    | Var_value : Var.Value.t param
    | Var_type_constr : Var.Type_constr.t param
    | Var_type_var : Var.Type_var.t param
    | Var_list : 'a param -> 'a list param

  val extract : _ t -> lambda

  val inject : lambda -> 'a t

  val func : 'a param -> ('b -> lambda) -> Ident.t -> 'b -> ('a -> 'b) t

  val list_param_binding :
    'a param -> ('b -> lambda) -> Ident.t list -> 'b -> ('a list -> 'b) t
end = struct
  type 'a t = lambda

  type 'a param =
    | Var_module : Var.Module.t param
    | Var_value : Var.Value.t param
    | Var_type_constr : Var.Type_constr.t param
    | Var_type_var : Var.Type_var.t param
    | Var_list : 'a param -> 'a list param

  let extract x = x

  let inject x = x

  let func_ _ id body =
    let param_from_name name =
      { name;
        layout = Pvalue { raw_kind = Pgenval; nullable = Non_nullable };
        debug_uid = debug_uid_none;
        attributes = { unbox_param = false };
        mode = alloc_heap
      }
    in
    lfunction
      ~kind:(Curried { nlocal = 1 })
      ~params:[param_from_name id]
      ~return:(Pvalue { raw_kind = Pgenval; nullable = Non_nullable })
      ~attr:default_function_attribute ~body ~loc:Loc_unknown ~mode:alloc_heap
      ~ret_mode:alloc_heap

  let func arg_sort body_lam id body = func_ arg_sort id (body_lam body)

  let list_param_binding (type a b) (arg_sort : a param)
      (body_lam : b -> lambda) idents (body : b) =
    let fun_body, t_opt =
      List.fold_right
        (fun id (body, t_opt) ->
          let new_t = Ident.create_local "t" in
          let let_t =
            match t_opt with
            | None -> body
            | Some t -> bind t (tl (Lvar new_t)) body
          in
          bind id (hd (Lvar new_t)) let_t, Some new_t)
        idents
        (body_lam body, None)
    in
    let list_arg =
      match t_opt with None -> Ident.create_local "t" | Some t -> t
    in
    func_ (Var_list arg_sort) list_arg fun_body
end

type 'a lam = 'a Lam.t

let extract = Lam.extract

let inject = Lam.inject

let inject_force x = Lam.inject (Lazy.force x)

let option_extract opt =
  match opt with None -> none | Some x -> some (extract x)

(* Environments in the context of translating quotations refer to the
 * various variables that quotations need to keep track of. *)

type 'a fv_env = (Ident.t, 'a) Hashtbl.t (* maps identifiers to lambda *)

type 'b pv_env =
  (string, Ident.t * 'b) Hashtbl.t (* maps identifiers to lambda *)

type var_env =
  { env_vals : Var.Value.t fv_env;
    env_tys : Var.Type_constr.t fv_env;
    env_mod : Var.Module.t fv_env;
    env_poly : Var.Type_var.t pv_env
  }

let vars_env =
  { env_vals = Hashtbl.create 64;
    env_tys = Hashtbl.create 64;
    env_mod = Hashtbl.create 64;
    env_poly = Hashtbl.create 64
  }

let _refresh_env () =
  Hashtbl.clear vars_env.env_vals;
  Hashtbl.clear vars_env.env_tys;
  Hashtbl.clear vars_env.env_mod;
  Hashtbl.clear vars_env.env_poly

let with_value ident val_ =
  Hashtbl.add vars_env.env_vals ident (Var.Value.mk val_)

let with_type_constr ident ty_ =
  Hashtbl.add vars_env.env_tys ident (Var.Type_constr.mk ty_)

let with_module ident mod_ =
  Hashtbl.add vars_env.env_mod ident (Var.Module.mk mod_)

let with_new_idents_values = List.iter (fun id -> with_value id (Lvar id))

let with_new_idents_types_constr =
  List.iter (fun id -> with_type_constr id (Lvar id))

let with_new_idents_modules = List.iter (fun id -> with_module id (Lvar id))

let without_value ident = Hashtbl.remove vars_env.env_vals ident

let without_type_constr ident = Hashtbl.remove vars_env.env_tys ident

let without_module ident = Hashtbl.remove vars_env.env_mod ident

let without_idents_values = List.iter without_value

let without_idents_types_constr = List.iter without_type_constr

let without_idents_modules = List.iter without_module

let with_poly_type name =
  let id = Ident.create_local name in
  Hashtbl.add vars_env.env_poly name (id, Var.Type_var.mk (Lvar id))

let with_new_idents_poly = List.iter with_poly_type

let without_poly_type name = Hashtbl.remove vars_env.env_poly name

let without_idents_poly = List.iter without_poly_type

let ident_for_poly_name name =
  let ident, _ = Hashtbl.find vars_env.env_poly name in
  ident

(* Compiling quoted expressions to lambda *)

let use modname field = combinator modname field

let apply modname field loc args =
  lazy
    (let comb = use modname field in
     Lambda.Lapply
       { ap_func = Lazy.force comb;
         ap_args = args;
         ap_probe = None;
         ap_loc = of_location ~scopes:empty_scopes loc;
         ap_result_layout =
           Pvalue { raw_kind = Pgenval; nullable = Non_nullable };
         ap_region_close = Rc_normal;
         ap_mode = alloc_heap;
         ap_tailcall = Default_tailcall;
         ap_inlined = Default_inlined;
         ap_specialised = Default_specialise
       })

let apply1 modname field loc arg1 = apply modname field loc [arg1]

let apply2 modname field loc arg1 arg2 = apply modname field loc [arg1; arg2]

let apply3 modname field loc arg1 arg2 arg3 =
  apply modname field loc [arg1; arg2; arg3]

let apply4 modname field loc arg1 arg2 arg3 arg4 =
  apply modname field loc [arg1; arg2; arg3; arg4]

let apply5 modname field loc arg1 arg2 arg3 arg4 arg5 =
  apply modname field loc [arg1; arg2; arg3; arg4; arg5]

let apply6 modname field loc arg1 arg2 arg3 arg4 arg5 arg6 =
  apply modname field loc [arg1; arg2; arg3; arg4; arg5; arg6]

module Loc : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val unknown : t'

  val known : Location.t -> string -> int -> int -> int -> int -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let unknown = use "Loc" "unknown"

  let known loc file start_line start_col end_line end_col =
    apply5 "Loc" "known" loc
      (Lconst (Const_base (Const_string (file, loc, None))))
      (Lconst (Const_base (Const_int start_line)))
      (Lconst (Const_base (Const_int start_col)))
      (Lconst (Const_base (Const_int end_line)))
      (Lconst (Const_base (Const_int end_col)))
end

module Name : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val mk : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let mk loc s = apply1 "Name" "mk" loc (string loc s)
end

module Constant : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val int : Location.t -> int -> t'

  val char : Location.t -> char -> t'

  val string : Location.t -> string -> string option -> t'

  val float : Location.t -> string -> t'

  val float32 : Location.t -> string -> t'

  val int32 : Location.t -> int32 -> t'

  val int64 : Location.t -> int64 -> t'

  val nativeint : Location.t -> nativeint -> t'

  val unboxed_float : Location.t -> string -> t'

  val unboxed_float32 : Location.t -> string -> t'

  val unboxed_int32 : Location.t -> int32 -> t'

  val unboxed_int64 : Location.t -> int64 -> t'

  val unboxed_nativeint : Location.t -> nativeint -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let int loc x =
    apply1 "Constant" "int" loc (Lconst (Const_base (Const_int x)))

  let char loc x =
    apply1 "Constant" "char" loc (Lconst (Const_base (Const_char x)))

  let string loc a1 a2 =
    apply2 "Constant" "string" loc
      (Lconst (Const_base (Const_string (a1, loc, None))))
      (string_option loc a2)

  let float loc x =
    apply1 "Constant" "float" loc
      (Lconst (Const_base (Const_string (x, loc, None))))

  let float32 loc x =
    apply1 "Constant" "float32" loc
      (Lconst (Const_base (Const_string (x, loc, None))))

  let int32 loc x =
    apply1 "Constant" "int32" loc (Lconst (Const_base (Const_int32 x)))

  let int64 loc x =
    apply1 "Constant" "int64" loc (Lconst (Const_base (Const_int64 x)))

  let nativeint loc x =
    apply1 "Constant" "nativeint" loc (Lconst (Const_base (Const_nativeint x)))

  let unboxed_float loc x =
    apply1 "Constant" "unboxed_float" loc
      (Lconst (Const_base (Const_string (x, loc, None))))

  let unboxed_float32 loc x =
    apply1 "Constant" "unboxed_float32" loc
      (Lconst (Const_base (Const_string (x, loc, None))))

  let unboxed_int32 loc x =
    apply1 "Constant" "unboxed_int32" loc
      (Lconst (Const_base (Const_unboxed_int32 x)))

  let unboxed_int64 loc x =
    apply1 "Constant" "unboxed_int64" loc
      (Lconst (Const_base (Const_unboxed_int64 x)))

  let unboxed_nativeint loc x =
    apply1 "Constant" "unboxed_nativeint" loc
      (Lconst (Const_base (Const_unboxed_nativeint x)))
end

module Exp_attribute : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val inline : t'

  val inlined : t'

  val specialise : t'

  val specialised : t'

  val unrolled : t'

  val nontail : t'

  val tail : t'

  val poll : t'

  val loop : t'

  val tail_mod_cons : t'

  val quotation : t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let inline = use "Exp_attribute" "inline"

  let inlined = use "Exp_attribute" "inlined"

  let specialise = use "Exp_attribute" "specialise"

  let specialised = use "Exp_attribute" "specialised"

  let unrolled = use "Exp_attribute" "unrolled"

  let nontail = use "Exp_attribute" "nontail"

  let tail = use "Exp_attribute" "tail"

  let poll = use "Exp_attribute" "poll"

  let loop = use "Exp_attribute" "loop"

  let tail_mod_cons = use "Exp_attribute" "tail_mod_cons"

  let quotation = use "Exp_attribute" "quotation"
end

module Identifier : sig
  module Module : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val compilation_unit : Location.t -> Global_module.Name.t -> t'

    val dot : Location.t -> t -> string -> t'

    val var : Location.t -> Var.Module.t -> Loc.t -> t'
  end

  module Value : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val dot : Location.t -> Module.t -> string -> t'

    val var : Location.t -> Var.Value.t -> Loc.t -> t'
  end

  module Type : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val dot : Location.t -> Module.t -> string -> t'

    val var : Location.t -> Var.Type_constr.t -> Loc.t -> t'

    val int : t'

    val char : t'

    val string : t'

    val bytes : t'

    val float : t'

    val float32 : t'

    val bool : t'

    val unit : t'

    val exn : t'

    val array : t'

    val iarray : t'

    val list : t'

    val option : t'

    val nativeint : t'

    val int32 : t'

    val int64 : t'

    val lazy_t : t'

    val extension_constructor : t'

    val floatarray : t'

    val lexing_position : t'

    val code : t'

    val unboxed_float : t'

    val unboxed_nativeint : t'

    val unboxed_int32 : t'

    val unboxed_int64 : t'

    val int8x16 : t'

    val int16x8 : t'

    val int32x4 : t'

    val int64x2 : t'

    val float32x4 : t'

    val float64x2 : t'
  end

  module Module_type : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val dot : Location.t -> Module.t -> string -> t'
  end

  module Constructor : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val dot : Location.t -> Module.t -> string -> t'

    val false_ : t'

    val true_ : t'

    val void : t'

    val nil : t'

    val cons : t'

    val none : t'

    val some : t'

    val match_failure : t'

    val out_of_memory : t'

    val invalid_argument : t'

    val failure : t'

    val not_found : t'

    val sys_error : t'

    val end_of_file : t'

    val division_by_zero : t'

    val stack_overflow : t'

    val sys_blocked_io : t'

    val assert_failure : t'

    val undefined_recursive_module : t'
  end

  module Field : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val dot : Location.t -> Module.t -> string -> t'

    val wrap : t' -> t
  end
end = struct
  module Module = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let compilation_unit loc a1 =
      (* CR metaprogramming jrickard: I'm pretty confident this is bugged:
         it ignores parameterized libraries, and references the wrong file for
         impls (for example Stdlib.Buffer should reference Stdlib__Buffer but
         this references Stdlib). *)
      Env.require_global_for_quote
        (Compilation_unit.Name.of_head_of_global_name a1);
      let a1 = Global_module.Name.to_string a1 in
      apply1 "Identifier.Module" "compilation_unit" loc (string loc a1)

    let dot loc a1 a2 =
      apply2 "Identifier.Module" "dot" loc (extract a1) (string loc a2)

    let var loc a1 a2 =
      apply2 "Identifier.Module" "var" loc (extract a1) (extract a2)
  end

  module Value = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let dot loc a1 a2 =
      apply2 "Identifier.Value" "dot" loc (extract a1) (string loc a2)

    let var loc a1 a2 =
      apply2 "Identifier.Value" "var" loc (extract a1) (extract a2)
  end

  module Type = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let dot loc a1 a2 =
      apply2 "Identifier.Type" "dot" loc (extract a1) (string loc a2)

    let var loc a1 a2 =
      apply2 "Identifier.Type" "var" loc (extract a1) (extract a2)

    let int = use "Identifier.Type" "int"

    let char = use "Identifier.Type" "char"

    let string = use "Identifier.Type" "string"

    let bytes = use "Identifier.Type" "bytes"

    let float = use "Identifier.Type" "float"

    let float32 = use "Identifier.Type" "float32"

    let bool = use "Identifier.Type" "bool"

    let unit = use "Identifier.Type" "unit"

    let exn = use "Identifier.Type" "exn"

    let array = use "Identifier.Type" "array"

    let iarray = use "Identifier.Type" "iarray"

    let list = use "Identifier.Type" "list"

    let option = use "Identifier.Type" "option"

    let nativeint = use "Identifier.Type" "nativeint"

    let int32 = use "Identifier.Type" "int32"

    let int64 = use "Identifier.Type" "int64"

    let lazy_t = use "Identifier.Type" "lazy_t"

    let extension_constructor = use "Identifier.Type" "extension_constructor"

    let floatarray = use "Identifier.Type" "floatarray"

    let lexing_position = use "Identifier.Type" "lexing_position"

    let code = use "Identifier.Type" "code"

    let unboxed_float = use "Identifier.Type" "unboxed_float"

    let unboxed_nativeint = use "Identifier.Type" "unboxed_nativeint"

    let unboxed_int32 = use "Identifier.Type" "unboxed_int32"

    let unboxed_int64 = use "Identifier.Type" "unboxed_int64"

    let int8x16 = use "Identifier.Type" "int8x16"

    let int16x8 = use "Identifier.Type" "int16x8"

    let int32x4 = use "Identifier.Type" "int32x4"

    let int64x2 = use "Identifier.Type" "int64x2"

    let float32x4 = use "Identifier.Type" "float32x4"

    let float64x2 = use "Identifier.Type" "float64x2"
  end

  module Module_type = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let dot loc a1 a2 =
      apply2 "Identifier.Module_type" "dot" loc (extract a1) (string loc a2)
  end

  module Constructor = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let dot loc a1 a2 =
      apply2 "Identifier.Constructor" "dot" loc (extract a1) (string loc a2)

    let false_ = use "Identifier.Constructor" "false_"

    let true_ = use "Identifier.Constructor" "true_"

    let void = use "Identifier.Constructor" "void"

    let nil = use "Identifier.Constructor" "nil"

    let cons = use "Identifier.Constructor" "cons"

    let none = use "Identifier.Constructor" "none"

    let some = use "Identifier.Constructor" "some"

    let match_failure = use "Identifier.Constructor" "match_failure"

    let out_of_memory = use "Identifier.Constructor" "out_of_memory"

    let invalid_argument = use "Identifier.Constructor" "invalid_argument"

    let failure = use "Identifier.Constructor" "failure"

    let not_found = use "Identifier.Constructor" "not_found"

    let sys_error = use "Identifier.Constructor" "sys_error"

    let end_of_file = use "Identifier.Constructor" "end_of_file"

    let division_by_zero = use "Identifier.Constructor" "division_by_zero"

    let stack_overflow = use "Identifier.Constructor" "stack_overflow"

    let sys_blocked_io = use "Identifier.Constructor" "sys_blocked_io"

    let assert_failure = use "Identifier.Constructor" "assert_failure"

    let undefined_recursive_module =
      use "Identifier.Constructor" "undefined_recursive_module"
  end

  module Field = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let dot loc a1 a2 =
      apply2 "Identifier.Field" "dot" loc (extract a1) (string loc a2)
  end
end

module Label : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  module Nonoptional : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val no_label : t'

    val labelled : Location.t -> string -> t'
  end

  val no_label : t'

  val labelled : Location.t -> string -> t'

  val optional : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  module Nonoptional = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let no_label = use "Label.Nonoptional" "no_label"

    let labelled loc a1 =
      apply1 "Label.Nonoptional" "labelled" loc (string loc a1)
  end

  let no_label = use "Label" "no_label"

  let labelled loc a1 = apply1 "Label" "labelled" loc (string loc a1)

  let optional loc a1 = apply1 "Label" "optional" loc (string loc a1)
end

module Module_type : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val ident : Location.t -> Identifier.Module_type.t -> t'

  val of_string : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let ident loc a1 = apply1 "Module_type" "of_string" loc (extract a1)

  let of_string loc a1 = apply1 "Module_type" "ident" loc (string loc a1)
end

module Fragment : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val name : Location.t -> string -> t'

  val dot : Location.t -> t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let name loc a1 = apply1 "Fragment" "name" loc (string loc a1)

  let dot loc a1 a2 = apply2 "Fragment" "dot" loc (extract a1) (string loc a2)
end

module Variant : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val of_string : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let of_string loc a1 = apply1 "Variant" "of_string" loc (string loc a1)
end

module Constructor : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val ident : Location.t -> Identifier.Constructor.t -> t'

  val of_string : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let ident loc a1 = apply1 "Constructor" "ident" loc (extract a1)

  let of_string loc a1 = apply1 "Constructor" "of_string" loc (string loc a1)
end

module Field : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val ident : Location.t -> Identifier.Field.t -> t'

  val of_string : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let ident loc a1 = apply1 "Field" "ident" loc (extract a1)

  let of_string loc a1 = apply1 "Field" "of_string" loc (string loc a1)
end

module Method : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val of_string : Location.t -> string -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let of_string loc a1 = apply1 "Method" "of_string" loc (string loc a1)
end

module Module : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val ident : Location.t -> Identifier.Module.t -> t'

  val apply : Location.t -> t -> t -> t'

  val apply_unit : Location.t -> t -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let ident loc a1 = apply1 "Module" "ident" loc (extract a1)

  let apply loc a1 a2 = apply2 "Module" "apply" loc (extract a1) (extract a2)

  let apply_unit loc a1 = apply1 "Module" "apply_unit" loc (extract a1)
end

module rec Variant_type : sig
  module Variant_form : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val fixed : t'

    val open_ : t'

    val closed : Location.t -> string list -> t'
  end

  module Row_field : sig
    type s

    type t' = s lazy_t

    type t = s lam

    val wrap : t' -> t

    val inherit_ : Location.t -> Type.t -> t'

    val tag : Location.t -> Variant.t -> bool -> Type.t list -> t'
  end

  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val of_row_fields_list :
    Location.t -> Row_field.t list -> Variant_form.t -> t'
end = struct
  module Variant_form = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let fixed = use "Variant_type.Variant_form" "fixed"

    let open_ = use "Variant_type.Variant_form" "open_"

    let closed loc a1 =
      apply1 "Variant_type.Variant_form" "closed" loc
        (mk_list (List.map (string loc) a1))
  end

  module Row_field = struct
    type s = lambda

    type t' = s lazy_t

    type t = s lam

    let wrap = inject_force

    let inherit_ loc a1 =
      apply1 "Variant_type.Row_field" "inherit_" loc (extract a1)

    let tag loc a1 a2 a3 =
      apply3 "Variant_type.Row_field" "tag" loc (extract a1) (quote_bool a2)
        (mk_list (List.map extract a3))
  end

  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let of_row_fields_list loc a1 a2 =
    apply2 "Variant_type" "of_row_fields_list" loc
      (mk_list (List.map extract a1))
      (extract a2)
end

and Type : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val var : Location.t -> Var.Type_var.t option -> t'

  val arrow : Location.t -> Label.t -> t -> t -> t'

  val tuple : Location.t -> (Label.Nonoptional.t * t) list -> t'

  val unboxed_tuple : Location.t -> (Label.Nonoptional.t * t) list -> t'

  val constr : Location.t -> Identifier.Type.t -> t list -> t'

  val alias : Location.t -> t -> Var.Type_var.t -> t'

  val variant : Location.t -> Variant_type.t -> t'

  val poly :
    Location.t -> Loc.t -> Name.t list -> (Var.Type_var.t list -> t) lam -> t'

  val package : Location.t -> Module_type.t -> (Fragment.t * t) list -> t'

  val quote : Location.t -> t -> t'

  val call_pos : t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let var loc a1 = apply1 "Type" "var" loc (option (Option.map extract a1))

  let arrow loc a1 a2 a3 =
    apply3 "Type" "arrow" loc (extract a1) (extract a2) (extract a3)

  let tuple loc a1 =
    apply1 "Type" "tuple" loc
      (mk_list (List.map (fun (lbl, ty) -> pair (extract lbl, extract ty)) a1))

  let unboxed_tuple loc a1 =
    apply1 "Type" "unboxed_tuple" loc
      (mk_list (List.map (fun (lbl, ty) -> pair (extract lbl, extract ty)) a1))

  let constr loc a1 a2 =
    apply2 "Type" "constr" loc (extract a1) (mk_list (List.map extract a2))

  let alias loc a1 a2 = apply2 "Type" "alias" loc (extract a1) (extract a2)

  let variant loc a1 = apply1 "Type" "variant" loc (extract a1)

  let poly loc a1 a2 a3 =
    apply3 "Type" "poly" loc (extract a1)
      (mk_list (List.map extract a2))
      (extract a3)

  let package loc a1 a2 =
    apply2 "Type" "package" loc (extract a1)
      (mk_list
         (List.map (fun (frag, ty) -> pair (extract frag, extract ty)) a2))

  let quote loc a1 = apply1 "Type" "quote" loc (extract a1)

  let call_pos = use "Type" "call_pos"
end

module Pat : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val any : t'

  val var : Location.t -> Var.Value.t -> t'

  val alias : Location.t -> t -> Var.Value.t -> t'

  val constant : Location.t -> Constant.t -> t'

  val tuple : Location.t -> (Label.Nonoptional.t * t) list -> t'

  val unboxed_tuple : Location.t -> (Label.Nonoptional.t * t) list -> t'

  val construct : Location.t -> Constructor.t -> t option -> t'

  val variant : Location.t -> Variant.t -> t option -> t'

  val record : Location.t -> (Field.t * t) list -> bool -> t'

  val unboxed_record : Location.t -> (Field.t * t) list -> bool -> t'

  val array : Location.t -> t list -> t'

  val or_ : Location.t -> t -> t -> t'

  val lazy_ : Location.t -> t -> t'

  val any_module : t'

  val unpack : Location.t -> Var.Module.t -> t'

  val exception_ : Location.t -> t -> t'

  val constraint_ : Location.t -> t -> Type.t -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let any = use "Pat" "any"

  let var loc a1 = apply1 "Pat" "var" loc (extract a1)

  let alias loc a1 a2 = apply2 "Pat" "alias" loc (extract a1) (extract a2)

  let constant loc a1 = apply1 "Pat" "constant" loc (extract a1)

  let tuple loc a1 =
    apply1 "Pat" "tuple" loc
      (mk_list (List.map (fun (lbl, ty) -> pair (extract lbl, extract ty)) a1))

  let unboxed_tuple loc a1 =
    apply1 "Pat" "unboxed_tuple" loc
      (mk_list (List.map (fun (lbl, ty) -> pair (extract lbl, extract ty)) a1))

  let construct loc a1 a2 =
    apply2 "Pat" "construct" loc (extract a1) (option_extract a2)

  let variant loc a1 a2 =
    apply2 "Pat" "variant" loc (extract a1) (option_extract a2)

  let record loc a1 a2 =
    apply2 "Pat" "record" loc
      (mk_list (List.map (fun (f, ty) -> pair (extract f, extract ty)) a1))
      (quote_bool a2)

  let unboxed_record loc a1 a2 =
    apply2 "Pat" "unboxed_record" loc
      (mk_list (List.map (fun (f, ty) -> pair (extract f, extract ty)) a1))
      (quote_bool a2)

  let array loc a1 = apply1 "Pat" "array" loc (mk_list (List.map extract a1))

  let or_ loc a1 a2 = apply2 "Pat" "or_" loc (extract a1) (extract a2)

  let lazy_ loc a1 = apply1 "Pat" "lazy_" loc (extract a1)

  let any_module = use "Pat" "any_module"

  let unpack loc a1 = apply1 "Pat" "unpack" loc (extract a1)

  let exception_ loc a1 = apply1 "Pat" "exception_" loc (extract a1)

  let constraint_ loc a1 a2 =
    apply2 "Pat" "constraint_" loc (extract a1) (extract a2)
end

module rec Case : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val nonbinding : Location.t -> Loc.t -> Pat.t -> Exp.t -> t'

  val simple : Location.t -> Loc.t -> Name.t -> (Var.Value.t -> Exp.t) lam -> t'

  val pattern :
    Location.t ->
    Loc.t ->
    Name.t list ->
    Name.t list ->
    (Var.Value.t list -> (Var.Module.t list -> Pat.t * Exp.t) lam) lam ->
    t'

  val guarded :
    Location.t ->
    Loc.t ->
    Name.t list ->
    Name.t list ->
    (Var.Value.t list -> (Var.Module.t list -> Pat.t * Exp.t * Exp.t) lam) lam ->
    t'

  val refutation :
    Location.t ->
    Loc.t ->
    Name.t list ->
    Name.t list ->
    (Var.Value.t list -> (Var.Module.t list -> Pat.t) lam) lam ->
    t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let nonbinding loc a1 a2 a3 =
    apply3 "Case" "nonbinding" loc (extract a1) (extract a2) (extract a3)

  let simple loc a1 a2 a3 =
    apply3 "Case" "simple" loc (extract a1) (extract a2) (extract a3)

  let pattern loc a1 a2 a3 a4 =
    apply4 "Case" "pattern" loc (extract a1)
      (mk_list (List.map extract a2))
      (mk_list (List.map extract a3))
      (extract a4)

  let guarded loc a1 a2 a3 a4 =
    apply4 "Case" "guarded" loc (extract a1)
      (mk_list (List.map extract a2))
      (mk_list (List.map extract a3))
      (extract a4)

  let refutation loc a1 a2 a3 a4 =
    apply4 "Case" "refutation" loc (extract a1)
      (mk_list (List.map extract a2))
      (mk_list (List.map extract a3))
      (extract a4)
end

and Type_constraint : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val constraint_ : Location.t -> Type.t -> t'

  val coercion : Location.t -> Type.t option -> Type.t -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let constraint_ loc a1 =
    apply1 "Type_constraint" "constraint_" loc (extract a1)

  let coercion loc a1 a2 =
    apply2 "Type_constraint" "coercion" loc
      (option (Option.map extract a1))
      (extract a2)
end

and Function : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val body : Location.t -> Exp.t -> Type_constraint.t option -> t'

  val cases : Location.t -> Case.t list -> Type_constraint.t option -> t'

  val param :
    Location.t ->
    Label.t ->
    Exp.t option ->
    Loc.t ->
    Name.t list ->
    (Var.Value.t list -> Pat.t * t) lam ->
    t'

  val param_module_nonbinding :
    Location.t -> Label.t -> Loc.t -> Pat.t -> t -> t'

  val param_module :
    Location.t ->
    Label.t ->
    Loc.t ->
    Name.t ->
    (Var.Module.t -> Pat.t * t) lam ->
    t'

  val newtype :
    Location.t -> Loc.t -> Name.t -> (Var.Type_constr.t -> t) lam -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let body loc a1 a2 =
    apply2 "Function" "body" loc (extract a1) (option (Option.map extract a2))

  let cases loc a1 a2 =
    apply2 "Function" "cases" loc
      (mk_list (List.map extract a1))
      (option (Option.map extract a2))

  let param loc a1 a2 a3 a4 a5 =
    apply5 "Function" "param" loc (extract a1)
      (option (Option.map extract a2))
      (extract a3)
      (mk_list (List.map extract a4))
      (extract a5)

  let param_module_nonbinding loc a1 a2 a3 a4 =
    apply4 "Function" "param_module_nonbinding" loc (extract a1) (extract a2)
      (extract a3) (extract a4)

  let param_module loc a1 a2 a3 a4 =
    apply4 "Function" "param_module" loc (extract a1) (extract a2) (extract a3)
      (extract a4)

  let newtype loc a1 a2 a3 =
    apply3 "Function" "newtype" loc (extract a1) (extract a2) (extract a3)
end

and Comprehension : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val body : Location.t -> Exp.t -> t'

  val when_clause : Location.t -> Exp.t -> t -> t'

  val for_range :
    Location.t ->
    Loc.t ->
    Name.t ->
    Exp.t ->
    Exp.t ->
    bool ->
    (Var.Value.t -> t) lam ->
    t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let body loc a1 = apply1 "Comprehension" "body" loc (extract a1)

  let when_clause loc a1 a2 =
    apply2 "Comprehension" "when_clause" loc (extract a1) (extract a2)

  let for_range loc a1 a2 a3 a4 a5 a6 =
    apply6 "Comprehension" "for_range" loc (extract a1) (extract a2)
      (extract a3) (extract a4) (quote_bool a5) (extract a6)
end

and Exp_desc : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val ident : Location.t -> Identifier.Value.t -> t'

  val constant : Location.t -> Constant.t -> t'

  val let_rec_simple :
    Location.t ->
    Loc.t ->
    Name.t list ->
    (Var.Value.t list -> Exp.t list * Exp.t) lam ->
    t'

  val let_ :
    Location.t ->
    Loc.t ->
    Name.t list ->
    Name.t list ->
    Exp.t list ->
    (Var.Value.t list -> (Var.Module.t list -> Pat.t * Exp.t) lam) lam ->
    t'

  val function_ : Location.t -> Function.t -> t'

  val apply : Location.t -> Exp.t -> (Label.t * Exp.t) list -> t'

  val match_ : Location.t -> Exp.t -> Case.t list -> t'

  val try_ : Location.t -> Exp.t -> Case.t list -> t'

  val tuple : Location.t -> (Label.Nonoptional.t * Exp.t) list -> t'

  val construct : Location.t -> Constructor.t -> Exp.t option -> t'

  val variant : Location.t -> Variant.t -> Exp.t option -> t'

  val record : Location.t -> (Field.t * Exp.t) list -> Exp.t option -> t'

  val field : Location.t -> Exp.t -> Field.t -> t'

  val setfield : Location.t -> Exp.t -> Field.t -> Exp.t -> t'

  val array : Location.t -> Exp.t list -> t'

  val ifthenelse : Location.t -> Exp.t -> Exp.t -> Exp.t option -> t'

  val sequence : Location.t -> Exp.t -> Exp.t -> t'

  val while_ : Location.t -> Exp.t -> Exp.t -> t'

  val for_simple :
    Location.t ->
    Loc.t ->
    Name.t ->
    Exp.t ->
    Exp.t ->
    bool ->
    (Var.Value.t -> Exp.t) lam ->
    t'

  val send : Location.t -> Exp.t -> Method.t -> t'

  val assert_ : Location.t -> Exp.t -> t'

  val lazy_ : Location.t -> Exp.t -> t'

  val letmodule_nonbinding : Location.t -> Module.t -> Exp.t -> t'

  val letmodule :
    Location.t ->
    Loc.t ->
    Name.t ->
    Module.t ->
    (Var.Module.t -> Exp.t) lam ->
    t'

  val constraint_ : Location.t -> Exp.t -> Type_constraint.t -> t'

  val new_ : Location.t -> Identifier.Value.t -> t'

  val pack : Location.t -> Module.t -> t'

  val unreachable : t'

  val src_pos : t'

  val stack : Location.t -> Exp.t -> t'

  val extension_constructor : Location.t -> Name.t -> t'

  val let_exception : Location.t -> Name.t -> Exp.t -> t'

  val let_op :
    Location.t -> Identifier.Value.t list -> Exp.t list -> Case.t -> t'

  val exclave : Location.t -> Exp.t -> t'

  val list_comprehension : Location.t -> Comprehension.t -> t'

  val array_comprehension : Location.t -> Comprehension.t -> t'

  val unboxed_tuple : Location.t -> (Label.Nonoptional.t * Exp.t) list -> t'

  val unboxed_record_product :
    Location.t -> (Field.t * Exp.t) list -> Exp.t option -> t'

  val unboxed_field : Location.t -> Exp.t -> Field.t -> t'

  val quote : Location.t -> Exp.t -> t'

  val antiquote : Location.t -> Exp.t -> t'

  val splice : Location.t -> Code.t -> t'

  val eval : Location.t -> Type.t -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let ident loc a1 = apply1 "Exp_desc" "ident" loc (extract a1)

  let constant loc a1 = apply1 "Exp_desc" "constant" loc (extract a1)

  let let_rec_simple loc a1 a2 a3 =
    apply3 "Exp_desc" "let_rec_simple" loc (extract a1)
      (mk_list (List.map extract a2))
      (extract a3)

  let let_ loc a1 a2 a3 a4 a5 =
    apply5 "Exp_desc" "let_" loc (extract a1)
      (mk_list (List.map extract a2))
      (mk_list (List.map extract a3))
      (mk_list (List.map extract a4))
      (extract a5)

  let function_ loc a1 = apply1 "Exp_desc" "function_" loc (extract a1)

  let apply loc a1 a2 =
    apply2 "Exp_desc" "apply" loc (extract a1)
      (mk_list (List.map (fun (lab, e) -> pair (extract lab, extract e)) a2))

  let match_ loc a1 a2 =
    apply2 "Exp_desc" "match_" loc (extract a1) (mk_list (List.map extract a2))

  let try_ loc a1 a2 =
    apply2 "Exp_desc" "try_" loc (extract a1) (mk_list (List.map extract a2))

  let tuple loc a1 =
    apply1 "Exp_desc" "tuple" loc
      (mk_list (List.map (fun (l, e) -> pair (extract l, extract e)) a1))

  let construct loc a1 a2 =
    apply2 "Exp_desc" "construct" loc (extract a1)
      (option (Option.map extract a2))

  let variant loc a1 a2 =
    apply2 "Exp_desc" "variant" loc (extract a1)
      (option (Option.map extract a2))

  let record loc a1 a2 =
    apply2 "Exp_desc" "record" loc
      (mk_list (List.map (fun (f, e) -> pair (extract f, extract e)) a1))
      (option (Option.map extract a2))

  let field loc a1 a2 = apply2 "Exp_desc" "field" loc (extract a1) (extract a2)

  let setfield loc a1 a2 a3 =
    apply3 "Exp_desc" "setfield" loc (extract a1) (extract a2) (extract a3)

  let array loc a1 =
    apply1 "Exp_desc" "array" loc (mk_list (List.map extract a1))

  let ifthenelse loc a1 a2 a3 =
    apply3 "Exp_desc" "ifthenelse" loc (extract a1) (extract a2)
      (option (Option.map extract a3))

  let sequence loc a1 a2 =
    apply2 "Exp_desc" "sequence" loc (extract a1) (extract a2)

  let while_ loc a1 a2 =
    apply2 "Exp_desc" "while_" loc (extract a1) (extract a2)

  let for_simple loc a1 a2 a3 a4 a5 a6 =
    apply6 "Exp_desc" "for_simple" loc (extract a1) (extract a2) (extract a3)
      (extract a4) (quote_bool a5) (extract a6)

  let send loc a1 a2 = apply2 "Exp_desc" "send" loc (extract a1) (extract a2)

  let assert_ loc a1 = apply1 "Exp_desc" "assert_" loc (extract a1)

  let lazy_ loc a1 = apply1 "Exp_desc" "lazy_" loc (extract a1)

  let letmodule_nonbinding loc a1 a2 =
    apply2 "Exp_desc" "letmodule_nonbinding" loc (extract a1) (extract a2)

  let letmodule loc a1 a2 a3 a4 =
    apply4 "Exp_desc" "letmodule" loc (extract a1) (extract a2) (extract a3)
      (extract a4)

  let constraint_ loc a1 a2 =
    apply2 "Exp_desc" "constraint_" loc (extract a1) (extract a2)

  let new_ loc a1 = apply1 "Exp_desc" "new_" loc (extract a1)

  let pack loc a1 = apply1 "Exp_desc" "pack" loc (extract a1)

  let unreachable = use "Exp_desc" "unreachable"

  let src_pos = use "Exp_desc" "src_pos"

  let stack loc a1 = apply1 "Exp_desc" "stack" loc (extract a1)

  let extension_constructor loc a1 =
    apply1 "Exp_desc" "extension_constructor" loc (extract a1)

  let let_exception loc a1 a2 =
    apply2 "Exp_desc" "let_exception" loc (extract a1) (extract a2)

  let let_op loc a1 a2 a3 =
    apply3 "Exp_desc" "let_op" loc
      (mk_list (List.map extract a1))
      (mk_list (List.map extract a2))
      (extract a3)

  let exclave loc a1 = apply1 "Exp_desc" "exclave" loc (extract a1)

  let list_comprehension loc a1 =
    apply1 "Exp_desc" "list_comprehension" loc (extract a1)

  let array_comprehension loc a1 =
    apply1 "Exp_desc" "array_comprehension" loc (extract a1)

  let unboxed_tuple loc a1 =
    apply1 "Exp_desc" "unboxed_tuple" loc
      (mk_list (List.map (fun (l, e) -> pair (extract l, extract e)) a1))

  let unboxed_record_product loc a1 a2 =
    apply2 "Exp_desc" "unboxed_record_product" loc
      (mk_list (List.map (fun (f, e) -> pair (extract f, extract e)) a1))
      (option (Option.map extract a2))

  let unboxed_field loc a1 a2 =
    apply2 "Exp_desc" "unboxed_field" loc (extract a1) (extract a2)

  let quote loc a1 = apply1 "Exp_desc" "quote" loc (extract a1)

  let antiquote loc a1 = apply1 "Exp_desc" "antiquote" loc (extract a1)

  let splice loc a1 = apply1 "Exp_desc" "splice" loc (extract a1)

  let eval loc a1 = apply1 "Exp_desc" "eval" loc (extract a1)
end

and Exp : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val mk : Location.t -> Exp_desc.t -> Exp_attribute.t list -> t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let mk loc a1 a2 =
    apply2 "Exp" "mk" loc (extract a1) (mk_list (List.map extract a2))
end

and Code : sig
  type s

  type t' = s lazy_t

  type t = s lam

  val wrap : t' -> t

  val inject : lambda -> t

  val of_exp : Location.t -> Loc.t -> Exp.t -> t'

  val of_exp_with_type_vars :
    Location.t ->
    Loc.t ->
    Name.t list ->
    (Var.Type_var.t list -> Exp.t) lam ->
    t'
end = struct
  type s = lambda

  type t' = s lazy_t

  type t = s lam

  let wrap = inject_force

  let inject = inject

  let of_exp loc a1 a2 = apply2 "Code" "of_exp" loc (extract a1) (extract a2)

  let of_exp_with_type_vars loc a1 a2 a3 =
    apply3 "Code" "of_exp_with_type_vars" loc (extract a1)
      (mk_list (List.map extract a2))
      (extract a3)
end

let mk_exp_noattr loc desc = Exp.mk loc desc [] |> Exp.wrap

let name_of_ident loc id = Name.mk loc (Ident.name id) |> Name.wrap

let quote_attributes e =
  let quoted_attr (attr : Typedtree.attribute) =
    (match attr.attr_name.txt with
    | "inline" -> Exp_attribute.inline
    | "inlined" -> Exp_attribute.inlined
    | "specialise" -> Exp_attribute.specialise
    | "specialised" -> Exp_attribute.specialised
    | "unrolled" -> Exp_attribute.unrolled
    | "nontail" -> Exp_attribute.nontail
    | "tail" -> Exp_attribute.tail
    | "poll" -> Exp_attribute.poll
    | "loop" -> Exp_attribute.loop
    | "tail_mod_cons" -> Exp_attribute.tail_mod_cons
    | "quotation" -> Exp_attribute.quotation
    | _ -> fatal_error "Unknown attribute")
    |> Exp_attribute.wrap
  in
  List.map quoted_attr e.exp_attributes

let quote_constant loc (const : Typedtree.constant) =
  (match const with
  | Const_int x -> Constant.int loc x
  | Const_char x -> Constant.char loc x
  | Const_string (x, loc, lopt) -> Constant.string loc x lopt
  | Const_float x -> Constant.float loc x
  | Const_float32 x -> Constant.float32 loc x
  | Const_int32 x -> Constant.int32 loc x
  | Const_int64 x -> Constant.int64 loc x
  | Const_nativeint x -> Constant.nativeint loc x
  | Const_unboxed_float x -> Constant.unboxed_float loc x
  | Const_unboxed_float32 x -> Constant.unboxed_float32 loc x
  | Const_unboxed_int32 x -> Constant.unboxed_int32 loc x
  | Const_unboxed_int64 x -> Constant.unboxed_int64 loc x
  | Const_unboxed_nativeint x -> Constant.unboxed_nativeint loc x
  (* TODO: add support for these in CamlinternalQuote *)
  | Const_untagged_char x -> Constant.char loc x
  | Const_int8 x
  | Const_int16 x
  | Const_untagged_int x
  | Const_untagged_int8 x
  | Const_untagged_int16 x ->
    Constant.int loc x)
  |> Constant.wrap

let quote_loc (loc : Location.t) =
  if loc = Location.none
  then Loc.wrap Loc.unknown
  else
    Loc.wrap
      (Loc.known loc loc.loc_start.pos_fname loc.loc_start.pos_lnum
         loc.loc_start.pos_cnum loc.loc_end.pos_lnum loc.loc_end.pos_cnum)

let quote_name (str : string loc) = Name.mk str.loc str.txt |> Name.wrap

let quote_method loc (meth : Typedtree.meth) =
  let name =
    match meth with
    | Tmeth_name name -> name
    | Tmeth_val id -> Ident.name id
    | Tmeth_ancestor (id, path) -> Path.name path ^ "#" ^ Ident.name id
  in
  Method.of_string loc name |> Method.wrap

let quote_arg_label loc = function
  | Labelled s -> Label.wrap (Label.labelled loc s)
  | Optional s -> Label.wrap (Label.optional loc s)
  | Nolabel -> Label.wrap Label.no_label
  | _ ->
    fatal_error
      "No support for any types of labels other than Labelled, Nolabel and \
       Optional"

let rec module_for_path loc = function
  | Path.Pident id ->
    (match Hashtbl.find_opt vars_env.env_mod id with
    | Some m -> Identifier.Module.var loc m (quote_loc loc)
    | None -> (
      match Ident.to_global id with
      | Some global -> Identifier.Module.compilation_unit loc global
      | None -> raise Exit))
    |> Identifier.Module.wrap
  | Path.Pdot (p, s) ->
    Identifier.Module.dot loc (module_for_path loc p) s
    |> Identifier.Module.wrap
  | _ -> raise Exit

let module_type_for_path loc = function
  | Path.Pident id ->
    Module_type.of_string loc (Ident.name id) |> Module_type.wrap
  | Path.Pdot (p, s) ->
    Module_type.ident loc
      (Identifier.Module_type.dot loc (module_for_path loc p) s
      |> Identifier.Module_type.wrap)
    |> Module_type.wrap
  | _ -> raise Exit

let type_for_path loc = function
  | Path.Pident id ->
    (match Hashtbl.find_opt vars_env.env_tys id with
    | Some t -> Identifier.Type.var loc t (quote_loc loc)
    | None -> (
      match Ident.name id with
      | "int" -> Identifier.Type.int
      | "char" -> Identifier.Type.char
      | "string" -> Identifier.Type.string
      | "bytes" -> Identifier.Type.bytes
      | "float" -> Identifier.Type.float
      | "float32" -> Identifier.Type.float32
      | "bool" -> Identifier.Type.bool
      | "unit" -> Identifier.Type.unit
      | "exn" -> Identifier.Type.exn
      | "array" -> Identifier.Type.array
      | "iarray" -> Identifier.Type.iarray
      | "list" -> Identifier.Type.list
      | "option" -> Identifier.Type.option
      | "nativeint" -> Identifier.Type.nativeint
      | "int32" -> Identifier.Type.int32
      | "int64" -> Identifier.Type.int64
      | "lazy_t" -> Identifier.Type.lazy_t
      | "extension_constructor" -> Identifier.Type.extension_constructor
      | "floatarray" -> Identifier.Type.floatarray
      | "lexing_position" -> Identifier.Type.lexing_position
      | "expr" -> Identifier.Type.code
      | "unboxed_float" -> Identifier.Type.unboxed_float
      | "unboxed_nativeint" -> Identifier.Type.unboxed_nativeint
      | "unboxed_int32" -> Identifier.Type.unboxed_int32
      | "unboxed_int64" -> Identifier.Type.unboxed_int64
      | "int8x16" -> Identifier.Type.int8x16
      | "int16x8" -> Identifier.Type.int16x8
      | "int32x4" -> Identifier.Type.int32x4
      | "int64x2" -> Identifier.Type.int64x2
      | "float32x4" -> Identifier.Type.float32x4
      | "float62x2" -> Identifier.Type.float64x2
      | _ -> raise Exit))
    |> Identifier.Type.wrap
  | Path.Pdot (p, s) ->
    Identifier.Type.dot loc (module_for_path loc p) s |> Identifier.Type.wrap
  | _ -> raise Exit

let value_for_path loc = function
  | Path.Pdot (p, s) ->
    Identifier.Value.dot loc (module_for_path loc p) s |> Identifier.Value.wrap
  | _ -> raise Exit

let value_for_path_opt loc p =
  match value_for_path loc p with res -> Some res | exception Exit -> None

let rec print_path = function
  | Path.Pident id -> Ident.name id
  | Path.Pdot (p, s) -> print_path p ^ "." ^ s
  | Path.Papply (p1, p2) -> print_path p1 ^ "(" ^ print_path p2 ^ ")"
  | Path.Pextra_ty (p, _) -> print_path p ^ "[extra]"

let quote_value_ident_path loc path =
  match value_for_path_opt loc path with
  | Some ident_val -> ident_val
  | None -> (
    match path with
    | Path.Pident id ->
      if Hashtbl.mem vars_env.env_vals id
      then
        Identifier.Value.var loc (Var.Value.mk (Lvar id)) (quote_loc loc)
        |> Identifier.Value.wrap
      else fatal_error ("Cannot quote free variable " ^ Ident.name id)
    | Path.Pdot _ | Path.Papply _ | Path.Pextra_ty _ ->
      fatal_error ("No global path for identifier " ^ print_path path))

let quote_value_ident_path_as_exp loc path =
  Exp_desc.ident loc (quote_value_ident_path loc path)

let type_path env ty =
  let desc =
    Types.get_desc (Ctype.expand_head_opt env (Ctype.correct_levels ty))
  in
  match desc with Tconstr (p, _, _) -> Some p | _ -> None

let quote_record_field env loc lbl_desc =
  match type_path env lbl_desc.lbl_res with
  | None -> fatal_error "No global path for record field"
  | Some (Path.Pident _) -> Field.of_string loc lbl_desc.lbl_name |> Field.wrap
  | Some (Path.Pdot (p, _)) ->
    Field.ident loc
      (Identifier.Field.dot loc (module_for_path loc p) lbl_desc.lbl_name
      |> Identifier.Field.wrap)
    |> Field.wrap
  | _ -> fatal_error "Unsupported constructor type detected."

let quote_constructor env loc constr =
  let exception Non_builtin of string in
  (try
     Identifier.Constructor.wrap
       (match type_path env constr.cstr_res with
       | None -> fatal_error "No global path for constructor"
       | Some (Path.Pident _) -> (
         match constr.cstr_name with
         | "false" -> Identifier.Constructor.false_
         | "true" -> Identifier.Constructor.true_
         | "()" -> Identifier.Constructor.void
         | "[]" -> Identifier.Constructor.nil
         | "::" -> Identifier.Constructor.cons
         | "None" -> Identifier.Constructor.none
         | "Some" -> Identifier.Constructor.some
         | "Match_failure" -> Identifier.Constructor.match_failure
         | "Out_of_memory" -> Identifier.Constructor.out_of_memory
         | "Invalid_argument" -> Identifier.Constructor.invalid_argument
         | "Failure" -> Identifier.Constructor.failure
         | "Not_found" -> Identifier.Constructor.not_found
         | "Sys_error" -> Identifier.Constructor.sys_error
         | "End_of_file" -> Identifier.Constructor.end_of_file
         | "Division_by_zero" -> Identifier.Constructor.division_by_zero
         | "Stack_overflow" -> Identifier.Constructor.stack_overflow
         | "Sys_blocked_io" -> Identifier.Constructor.sys_blocked_io
         | "Assert_failure" -> Identifier.Constructor.assert_failure
         | "Undefined_recursive_module" ->
           Identifier.Constructor.undefined_recursive_module
         | name -> raise (Non_builtin name))
       | Some (Path.Pdot (p, _)) ->
         Identifier.Constructor.dot loc (module_for_path loc p) constr.cstr_name
       | _ -> fatal_error "Unsupported constructor type detected.")
     |> Constructor.ident loc
   with Non_builtin name -> Constructor.of_string loc name)
  |> Constructor.wrap

let rec quote_fragment_of_lid loc = function
  | Lident id -> Fragment.name loc id |> Fragment.wrap
  | Ldot (p, s) ->
    Fragment.dot loc (quote_fragment_of_lid loc p) s |> Fragment.wrap
  | _ -> fatal_error "Unsupported fragment type detected."

let quote_variant loc name = Variant.of_string loc name |> Variant.wrap

let quote_nonopt loc (lbl : string option) =
  match lbl with
  | None -> Label.Nonoptional.no_label |> Label.Nonoptional.wrap
  | Some s -> Label.Nonoptional.labelled loc s |> Label.Nonoptional.wrap

let is_module pat =
  List.mem Tpat_unpack (List.map (fun (extra, _, _) -> extra) pat.pat_extra)

let rec with_new_idents_pat pat =
  match pat.pat_desc with
  | Tpat_any -> ()
  | Tpat_var (id, _, _, _, _) ->
    if is_module pat
    then with_new_idents_modules [id]
    else with_new_idents_values [id]
  | Tpat_alias (pat, id, _, _, _, _, _) ->
    with_new_idents_values [id];
    with_new_idents_pat pat
  | Tpat_constant _ -> ()
  | Tpat_tuple args -> List.iter (fun (_, pat) -> with_new_idents_pat pat) args
  | Tpat_construct (_, _, args, _) ->
    List.iter (fun pat -> with_new_idents_pat pat) args
  | Tpat_variant (_, argo, _) -> (
    match argo with None -> () | Some pat -> with_new_idents_pat pat)
  | Tpat_record (lbl_pats, _) ->
    List.iter (fun (_, _, pat) -> with_new_idents_pat pat) lbl_pats
  | Tpat_array (_, _, pats) ->
    List.iter (fun pat -> with_new_idents_pat pat) pats
  | Tpat_or (pat1, pat2, _) ->
    with_new_idents_pat pat1;
    with_new_idents_pat pat2
  | Tpat_unboxed_tuple args ->
    List.iter (fun (_, pat, _) -> with_new_idents_pat pat) args
  | Tpat_record_unboxed_product (lbl_pats, _) ->
    List.iter (fun (_, _, pat) -> with_new_idents_pat pat) lbl_pats
  | Tpat_lazy pat -> with_new_idents_pat pat

let rec without_idents_pat pat =
  match pat.pat_desc with
  | Tpat_any -> ()
  | Tpat_var (id, _, _, _, _) ->
    if is_module pat
    then without_idents_modules [id]
    else without_idents_values [id]
  | Tpat_alias (pat, id, _, _, _, _, _) ->
    without_idents_values [id];
    without_idents_pat pat
  | Tpat_constant _ -> ()
  | Tpat_tuple args -> List.iter (fun (_, pat) -> without_idents_pat pat) args
  | Tpat_construct (_, _, args, _) ->
    List.iter (fun pat -> without_idents_pat pat) args
  | Tpat_variant (_, argo, _) -> (
    match argo with None -> () | Some pat -> without_idents_pat pat)
  | Tpat_record (lbl_pats, _) ->
    List.iter (fun (_, _, pat) -> without_idents_pat pat) lbl_pats
  | Tpat_array (_, _, pats) ->
    List.iter (fun pat -> without_idents_pat pat) pats
  | Tpat_or (pat1, pat2, _) ->
    without_idents_pat pat1;
    without_idents_pat pat2
  | Tpat_unboxed_tuple args ->
    List.iter (fun (_, pat, _) -> without_idents_pat pat) args
  | Tpat_record_unboxed_product (lbl_pats, _) ->
    List.iter (fun (_, _, pat) -> without_idents_pat pat) lbl_pats
  | Tpat_lazy pat -> without_idents_pat pat

let with_new_param fp =
  let pat_of_param =
    match fp.fp_kind with
    | Tparam_pat pat -> pat
    | Tparam_optional_default (pat, _, _) -> pat
  in
  with_new_idents_pat pat_of_param;
  List.iter
    (fun (id, _, _, _) -> with_new_idents_types_constr [id])
    fp.fp_newtypes

let without_param fp =
  let pat_of_param =
    match fp.fp_kind with
    | Tparam_pat pat -> pat
    | Tparam_optional_default (pat, _, _) -> pat
  in
  without_idents_pat pat_of_param;
  List.iter
    (fun (id, _, _, _) -> without_idents_types_constr [id])
    fp.fp_newtypes

type case_binding =
  | Non_binding of Pat.t * Exp.t
  | Simple of Name.t * (Var.Value.t -> Exp.t) lam
  | Pattern of
      Name.t list
      * Name.t list
      * (Var.Value.t list -> (Var.Module.t list -> Pat.t * Exp.t) lam) lam
  | Guarded of
      Name.t list
      * Name.t list
      * (Var.Value.t list -> (Var.Module.t list -> Pat.t * Exp.t * Exp.t) lam)
        lam
  | Refutation of
      Name.t list
      * Name.t list
      * (Var.Value.t list -> (Var.Module.t list -> Pat.t) lam) lam

let rec quote_module_path loc = function
  (* CR metaprogramming jrickard: I think this should probably use
     [Env.find_module_address] at least it should do to register the globals
     that will be needed. *)
  | Path.Pident s -> (
    match Ident.to_global s with
    | Some global ->
      Identifier.Module.compilation_unit loc global |> Identifier.Module.wrap
    | None -> failwith "TODO")
  | Path.Pdot (p, s) ->
    Identifier.Module.dot loc (quote_module_path loc p) s
    |> Identifier.Module.wrap
  | _ -> fatal_error "No support for Papply in quoting modules"

let rec quote_computation_pattern p =
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_value pat -> quote_value_pattern (pat :> value general_pattern)
  | Tpat_exception pat ->
    Pat.exception_ loc (quote_value_pattern pat) |> Pat.wrap
  | Tpat_or (pat1, pat2, _) ->
    let pat1 = quote_computation_pattern pat1 in
    let pat2 = quote_computation_pattern pat2 in
    Pat.or_ loc pat1 pat2 |> Pat.wrap

and quote_pat_extra loc pat_lam extra =
  let extra, _, _ = extra in
  match extra with
  | Tpat_constraint ty ->
    Pat.constraint_ loc pat_lam (quote_core_type ty) |> Pat.wrap
  | Tpat_unpack -> pat_lam (* handled elsewhere *)
  | Tpat_type _ -> pat_lam (* TODO: consider adding support for #tconst *)
  | Tpat_open _ -> fatal_error "No support for open patterns."

and quote_value_pattern p =
  let env = p.pat_env and loc = p.pat_loc in
  let pat_quoted =
    match p.pat_desc with
    | Tpat_any -> if is_module p then Pat.any_module else Pat.any
    | Tpat_var (id, _, _, _, _) ->
      if is_module p
      then Pat.unpack loc (Var.Module.mk (Lvar id))
      else Pat.var loc (Var.Value.mk (Lvar id))
    | Tpat_alias (pat, id, _, _, _, _, _) ->
      let pat = quote_value_pattern pat in
      Pat.alias loc pat (Var.Value.mk (Lvar id))
    | Tpat_constant const ->
      let const = quote_constant loc const in
      Pat.constant loc const
    | Tpat_tuple pats ->
      let pats =
        List.map
          (fun (lbl, p) -> quote_nonopt loc lbl, quote_value_pattern p)
          pats
      in
      Pat.tuple loc pats
    | Tpat_construct (lid, constr, args, _) ->
      let constr = quote_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | _ :: _ ->
          let args = List.map quote_value_pattern args in
          let with_labels =
            List.map
              (fun a -> Label.Nonoptional.no_label |> Label.Nonoptional.wrap, a)
              args
          in
          Some (Pat.tuple loc with_labels |> Pat.wrap)
      in
      Pat.construct loc constr args
    | Tpat_variant (variant, argo, _) ->
      let argo = Option.map quote_value_pattern argo in
      Pat.variant loc (Variant.of_string loc variant |> Variant.wrap) argo
    | Tpat_record (lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl_desc, pat) ->
            let lbl = quote_record_field env Asttypes.(lid.loc) lbl_desc in
            let pat = quote_value_pattern pat in
            lbl, pat)
          lbl_pats
      in
      let closed =
        match closed with Asttypes.Closed -> true | Asttypes.Open -> false
      in
      Pat.record loc lbl_pats closed
    | Tpat_array (_, _, pats) ->
      let pats = List.map quote_value_pattern pats in
      Pat.array loc pats
    | Tpat_or (pat1, pat2, _) ->
      let pat1 = quote_value_pattern pat1 in
      let pat2 = quote_value_pattern pat2 in
      Pat.or_ loc pat1 pat2
    | Tpat_unboxed_tuple pats ->
      let pats =
        List.map
          (fun (lbl, p, _) -> quote_nonopt loc lbl, quote_value_pattern p)
          pats
      in
      Pat.unboxed_tuple loc pats
    | Tpat_record_unboxed_product (lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl_desc, pat) ->
            let lbl = quote_record_field env Asttypes.(lid.loc) lbl_desc in
            let pat = quote_value_pattern pat in
            lbl, pat)
          lbl_pats
      in
      let closed =
        match closed with Asttypes.Closed -> true | Asttypes.Open -> false
      in
      Pat.unboxed_record loc lbl_pats closed
    | Tpat_lazy pat ->
      let pat = quote_value_pattern pat in
      Pat.lazy_ loc pat
  in
  List.fold_right
    (fun extra p -> quote_pat_extra loc p extra)
    p.pat_extra (Pat.wrap pat_quoted)

and quote_core_type ty =
  let loc = ty.ctyp_loc in
  match ty.ctyp_desc with
  | Ttyp_var (None, _) -> Type.var loc None |> Type.wrap
  | Ttyp_var (Some name, _) ->
    let var =
      match Hashtbl.find_opt vars_env.env_poly name with
      | Some (_, var) -> var
      | None ->
        with_poly_type name;
        let _, var = Hashtbl.find vars_env.env_poly name in
        var
    in
    Type.var loc (Some var) |> Type.wrap
  | Ttyp_arrow (arg_lab, ty1, ty2) ->
    let lab = quote_arg_label loc arg_lab
    and ty1 = quote_core_type ty1
    and ty2 = quote_core_type ty2 in
    Type.arrow loc lab ty1 ty2 |> Type.wrap
  | Ttyp_tuple ts ->
    let tups =
      List.map
        (fun (s_opt, ty) -> quote_nonopt loc s_opt, quote_core_type ty)
        ts
    in
    Type.tuple loc tups |> Type.wrap
  | Ttyp_unboxed_tuple ts ->
    let tups =
      List.map
        (fun (s_opt, ty) -> quote_nonopt loc s_opt, quote_core_type ty)
        ts
    in
    Type.unboxed_tuple loc tups |> Type.wrap
  | Ttyp_constr (path, _, tys) ->
    let ident = type_for_path loc path and tys = List.map quote_core_type tys in
    Type.constr loc ident tys |> Type.wrap
  | Ttyp_object (_, _) -> fatal_error "Still not implemented."
  | Ttyp_class (_, _, _) -> fatal_error "Still not implemented."
  | Ttyp_alias (ty, alias_opt, _) -> (
    let ty = quote_core_type ty in
    match alias_opt with
    | None -> ty
    | Some { txt; _ } ->
      with_poly_type txt;
      Type.alias loc ty (snd (Hashtbl.find vars_env.env_poly txt)) |> Type.wrap)
  | Ttyp_variant (row_fields, closed_flag, labels) ->
    let row_fields =
      List.map
        (fun rf ->
          match rf.rf_desc with
          | Tinherit ty ->
            Variant_type.Row_field.inherit_ rf.rf_loc (quote_core_type ty)
            |> Variant_type.Row_field.wrap
          | Ttag (tag, b, tys) ->
            let variant = Variant.of_string tag.loc tag.txt |> Variant.wrap in
            Variant_type.Row_field.tag rf.rf_loc variant b
              (List.map quote_core_type tys)
            |> Variant_type.Row_field.wrap)
        row_fields
    and variant_form =
      match closed_flag, labels with
      | Open, None -> Variant_type.Variant_form.open_
      | Closed, None -> Variant_type.Variant_form.fixed
      | _, Some labs -> Variant_type.Variant_form.closed loc labs
    in
    Type.variant loc
      (Variant_type.of_row_fields_list loc row_fields
         (Variant_type.Variant_form.wrap variant_form)
      |> Variant_type.wrap)
    |> Type.wrap
  | Ttyp_poly (tvs, ty) ->
    let names = List.map fst tvs in
    let names_lam = List.map (fun name -> Name.wrap (Name.mk loc name)) names in
    with_new_idents_poly names;
    let body =
      Lam.list_param_binding Var_type_var extract
        (List.map ident_for_poly_name names)
        (quote_core_type ty)
    in
    without_idents_poly names;
    Type.poly loc (quote_loc loc) names_lam body |> Type.wrap
  | Ttyp_package package ->
    let { pack_path; pack_fields; pack_type = _; pack_txt = _ } = package in
    let mod_type = module_type_for_path loc pack_path
    and with_types =
      List.map
        (fun (lid, ty) ->
          quote_fragment_of_lid Asttypes.(lid.loc) lid.txt, quote_core_type ty)
        pack_fields
    in
    Type.package loc mod_type with_types |> Type.wrap
  | Ttyp_quote ty -> Type.quote loc (quote_core_type ty) |> Type.wrap
  | Ttyp_splice _ -> Type.var loc None |> Type.wrap
  | Ttyp_open _ -> fatal_error "Still not implemented."
  | Ttyp_of_kind _ -> fatal_error "Still not implemented."
  | Ttyp_call_pos -> Type.wrap Type.call_pos

let rec case_binding transl stage case =
  let pat = case.c_lhs in
  match case.c_guard with
  | None -> (
    let binding_with_computation_pat () =
      match pat_bound_idents pat with
      | [] ->
        let pat = quote_computation_pattern pat in
        let exp = quote_expression transl stage case.c_rhs in
        Non_binding (pat, exp)
      | ids ->
        let loc = pat.pat_loc in
        let names = List.map (name_of_ident loc) ids in
        let pat = quote_computation_pattern pat in
        with_new_idents_values ids;
        let exp = quote_expression transl stage case.c_rhs in
        let res =
          match case.c_rhs.exp_desc with
          | Texp_unreachable ->
            let body =
              Lam.list_param_binding Var_value extract ids
                (Lam.list_param_binding Var_module extract [] pat)
            in
            Refutation (names, [], body)
          | _ ->
            let body =
              Lam.list_param_binding Var_value extract ids
                (Lam.list_param_binding Var_module
                   (fun (p, e) -> pair (extract p, extract e))
                   [] (pat, exp))
            in
            Pattern (names, [], body)
        in
        without_idents_values ids;
        res
    in
    match pat.pat_desc with
    | Tpat_value pat -> (
      match (pat :> value general_pattern).pat_desc with
      | Tpat_var (id, name, _, _, _) ->
        with_new_idents_values [id];
        let exp = quote_expression transl stage case.c_rhs in
        let res = Simple (quote_name name, Lam.func Var_value extract id exp) in
        without_idents_values [id];
        res
      | _ -> binding_with_computation_pat ())
    | _ -> binding_with_computation_pat ())
  | Some guard ->
    let ids = pat_bound_idents case.c_lhs in
    let names = List.map (name_of_ident guard.exp_loc) ids in
    let pat = quote_computation_pattern case.c_lhs in
    with_new_idents_values ids;
    let exp = quote_expression transl stage case.c_rhs in
    let guard = quote_expression transl stage guard in
    let body =
      Lam.list_param_binding Var_value extract ids
        (Lam.list_param_binding Var_module
           (fun (p, g, e) -> triple (extract p, extract g, extract e))
           [] (pat, guard, exp))
    in
    let res = Guarded (names, [], body) in
    without_idents_values ids;
    res

and case_value_pattern_binding transl stage case =
  case_binding transl stage
    { case with c_lhs = as_computation_pattern case.c_lhs }

and quote_case_binding loc cb =
  (match cb with
  | Non_binding (pat, exp) -> Case.nonbinding loc (quote_loc loc) pat exp
  | Simple (name, body) -> Case.simple loc (quote_loc loc) name body
  | Pattern (names_vals, names_mods, body) ->
    Case.pattern loc (quote_loc loc) names_vals names_mods body
  | Guarded (names_vals, names_mods, body) ->
    Case.guarded loc (quote_loc loc) names_vals names_mods body
  | Refutation (names_vals, names_mods, body) ->
    Case.refutation loc (quote_loc loc) names_vals names_mods body)
  |> Case.wrap

and quote_case transl stage loc case =
  quote_case_binding loc (case_binding transl stage case)

and quote_value_pattern_case transl stage loc case =
  quote_case_binding loc (case_value_pattern_binding transl stage case)

and quote_newtype loc ident sloc rest =
  Function.newtype loc (quote_loc loc) (quote_name sloc)
    (Lam.func Var_type_constr extract ident rest)
  |> Function.wrap

and fun_param_binding transl stage loc param frest =
  let with_newtypes =
    List.fold_right
      (fun (ident, sloc, _, _) rest -> quote_newtype loc ident sloc rest)
      param.fp_newtypes frest
  in
  let pat, opt_exp =
    match param.fp_kind with
    | Tparam_pat pat -> pat, None
    | Tparam_optional_default (pat, exp, _) ->
      pat, Some (quote_expression transl stage exp)
  in
  let idents = pat_bound_idents pat in
  let fun_ =
    if is_module pat
    then
      match idents with
      | [] ->
        Function.param_module_nonbinding loc
          (quote_arg_label loc param.fp_arg_label)
          (quote_loc loc) (quote_value_pattern pat) with_newtypes
      | [id] ->
        let fun_rem =
          Lam.func Var_module
            (fun (p, e) -> pair (extract p, extract e))
            id
            (quote_value_pattern pat, with_newtypes)
        and name = name_of_ident loc id in
        Function.param_module loc
          (quote_arg_label loc param.fp_arg_label)
          (quote_loc loc) name fun_rem
      | _ ->
        fatal_error "Expected only one module variable in parameter binding."
    else
      let fun_rem =
        Lam.list_param_binding Var_value
          (fun (p, f) -> pair (extract p, extract f))
          idents
          (quote_value_pattern pat, with_newtypes)
      and names = List.map (name_of_ident loc) idents in
      Function.param loc
        (quote_arg_label loc param.fp_arg_label)
        opt_exp (quote_loc loc) names fun_rem
  in
  Function.wrap fun_

and quote_function transl stage loc fn extras =
  match fn with
  | Texp_function fn ->
    List.iter with_new_param fn.params;
    let fn_body =
      match fn.body with
      | Tfunction_body exp ->
        Function.body loc (quote_expression transl stage exp) None
      | Tfunction_cases cases ->
        Function.cases loc
          (List.map
             (fun fc ->
               quote_case_binding fc.c_lhs.pat_loc
                 (case_value_pattern_binding transl stage fc))
             cases.fc_cases)
          None
    in
    let fn_def =
      List.fold_right
        (fun_param_binding transl stage loc)
        fn.params (Function.wrap fn_body)
    in
    List.iter without_param fn.params;
    List.fold_right
      (fun (extra, loc, _) fn ->
        match extra with
        | Texp_newtype (id, sloc, _, _) ->
          Function.newtype loc (quote_loc sloc.loc) (quote_name sloc)
            (Lam.func Var_type_constr extract id fn)
          |> Function.wrap
        | _ -> fn)
      extras fn_def
  | _ -> fatal_error "Unexpected usage of quote_function."

and quote_module_exp transl stage loc mod_exp =
  match mod_exp.mod_desc with
  | Tmod_ident (path, _) ->
    let m = quote_module_path loc path in
    Module.ident loc m |> Module.wrap
  | Tmod_apply (funct, arg, _) ->
    let transl_funct = quote_module_exp transl stage loc funct in
    let transl_arg = quote_module_exp transl stage loc arg in
    Module.apply loc transl_funct transl_arg |> Module.wrap
  | Tmod_apply_unit funct ->
    let transl_funct = quote_module_exp transl stage loc funct in
    Module.apply_unit loc transl_funct |> Module.wrap
  | Tmod_constraint (mod_exp, _, _, _) ->
    quote_module_exp transl stage loc mod_exp
  | Tmod_structure _ | Tmod_functor _ ->
    fatal_error "Cannot quote struct..end blocks"
  | Tmod_unpack _ -> fatal_error "No support for unpacking first-class modules"

and quote_comprehension transl stage loc { comp_body; comp_clauses } =
  let add_clause body = function
    | Texp_comp_when exp ->
      let exp = quote_expression transl stage exp in
      Comprehension.when_clause loc exp body |> Comprehension.wrap
    | Texp_comp_for clause_bindings ->
      List.fold_left
        (fun body clb ->
          match clb.comp_cb_iterator with
          | Texp_comp_range rcd ->
            let start = quote_expression transl stage rcd.start
            and stop = quote_expression transl stage rcd.stop
            and direction =
              match rcd.direction with Upto -> true | Downto -> false
            in
            let body_fn = Lam.func Var_value extract rcd.ident body in
            Comprehension.for_range loc (quote_loc loc)
              (quote_name (mkloc (Ident.name rcd.ident) loc))
              start stop direction body_fn
            |> Comprehension.wrap
          | Texp_comp_in _ -> fatal_error "foo")
        body clause_bindings
  in
  let body = Comprehension.body loc (quote_expression transl stage comp_body) in
  List.fold_left
    (fun body clause -> add_clause body clause)
    (Comprehension.wrap body) comp_clauses

and quote_expression_extra _ _ extra lambda =
  let extra, loc, _ = extra in
  match extra with
  | Texp_newtype _ -> lambda
  (* Texp_newtype only relevant for functions, handled elsewhere *)
  | Texp_constraint ty ->
    let constr_ =
      Type_constraint.constraint_ loc (quote_core_type ty)
      |> Type_constraint.wrap
    in
    Exp_desc.constraint_ loc (mk_exp_noattr loc lambda) constr_ |> Exp_desc.wrap
  | Texp_coerce (ty_opt, ty) ->
    let coerce =
      Type_constraint.coercion loc
        (Option.map quote_core_type ty_opt)
        (quote_core_type ty)
      |> Type_constraint.wrap
    in
    Exp_desc.constraint_ loc (mk_exp_noattr loc lambda) coerce |> Exp_desc.wrap
  | Texp_stack -> Exp_desc.stack loc (mk_exp_noattr loc lambda) |> Exp_desc.wrap
  | Texp_poly _ -> fatal_error "No support for Texp_poly yet"
  | Texp_mode _ -> lambda (* FIXME: add modes to quotation representation *)

and update_env_with_extra extra =
  let extra, _, _ = extra in
  match extra with
  | Texp_newtype (id, _, _, _) -> with_new_idents_types_constr [id]
  | Texp_constraint _ | Texp_coerce _ | Texp_stack -> ()
  | Texp_poly _ -> fatal_error "No support for Texp_poly yet"
  | Texp_mode _ -> ()

and update_env_without_extra extra =
  let extra, _, _ = extra in
  match extra with
  | Texp_newtype (id, _, _, _) -> without_idents_types_constr [id]
  | Texp_constraint _ | Texp_coerce _ | Texp_stack -> ()
  | Texp_poly _ -> fatal_error "No support for Texp_poly yet"
  | Texp_mode _ -> ()

and quote_expression_desc transl stage e =
  let env = e.exp_env in
  let loc = e.exp_loc in
  List.iter update_env_with_extra e.exp_extra;
  let body =
    match e.exp_desc with
    | Texp_ident (path, _, _, _, _) -> quote_value_ident_path_as_exp loc path
    | Texp_constant const ->
      let const = quote_constant loc const in
      Exp_desc.constant loc const
    | Texp_let (rec_flag, vbs, exp) -> (
      match rec_flag with
      | Recursive ->
        let names_defs =
          List.map
            (fun vb ->
              match vb.vb_pat.pat_desc with
              | Tpat_var (ident, _, _, _, _) -> ident, vb.vb_expr
              | _ -> assert false)
            vbs
        in
        let idents, defs = List.split names_defs in
        with_new_idents_values idents;
        let names_lam = List.map (name_of_ident loc) idents in
        let defs_lam = List.map (quote_expression transl stage) defs in
        let frest =
          Lam.list_param_binding Var_value
            (fun (defs, body) ->
              pair (mk_list (List.map extract defs), extract body))
            idents
            (defs_lam, quote_expression transl stage exp)
        in
        without_idents_values idents;
        Exp_desc.let_rec_simple loc (quote_loc loc) names_lam frest
      | Nonrecursive ->
        let val_l, _, pats, defs =
          List.fold_left
            (fun (val_l, _, pats, defs) vb ->
              let pat = vb.vb_pat in
              let idents = pat_bound_idents pat in
              let def = quote_expression transl stage vb.vb_expr in
              with_new_idents_values idents;
              idents @ val_l, [], pat :: pats, def :: defs)
            ([], [], [], []) (List.rev vbs)
        in
        let def_pat =
          Pat.tuple loc
            (List.map
               (fun pat ->
                 ( Label.Nonoptional.wrap Label.Nonoptional.no_label,
                   quote_value_pattern pat ))
               pats)
          |> Pat.wrap
        in
        let names_lam = List.map (name_of_ident loc) val_l
        and frest =
          Lam.list_param_binding Var_value extract val_l
            (Lam.list_param_binding Var_module
               (fun (p, e) -> pair (extract p, extract e))
               []
               (def_pat, quote_expression transl stage exp))
        in
        List.iter
          (fun vb -> without_idents_values (pat_bound_idents vb.vb_pat))
          vbs;
        Exp_desc.let_ loc (quote_loc loc) names_lam [] defs frest)
    | Texp_function fun_spec ->
      let fn =
        quote_function transl stage loc (Texp_function fun_spec) e.exp_extra
      in
      Exp_desc.function_ loc fn
    | Texp_apply (fn, args, _, _, _) ->
      let fn = quote_expression transl stage fn in
      let args =
        List.filter
          (fun (_, exp) -> match exp with Omitted _ -> false | _ -> true)
          args
      in
      let args =
        List.map
          (fun (lbl, exp) ->
            match exp with
            | Omitted _ -> assert false
            | Arg (exp, _) ->
              let lbl = quote_arg_label loc lbl in
              let exp = quote_expression transl stage exp in
              lbl, exp)
          args
      in
      Exp_desc.apply loc fn args
    | Texp_match (exp, _, cases, _) ->
      let exp = quote_expression transl stage exp in
      let cases = List.map (quote_case transl stage loc) cases in
      Exp_desc.match_ loc exp cases
    | Texp_try (exp, cases) ->
      let exp = quote_expression transl stage exp
      and cases = List.map (quote_value_pattern_case transl stage loc) cases in
      Exp_desc.try_ loc exp cases
    | Texp_tuple (exps, _) ->
      let exps =
        List.map
          (fun (lab, exp) ->
            quote_nonopt loc lab, quote_expression transl stage exp)
          exps
      in
      Exp_desc.tuple loc exps
    | Texp_construct (lid, constr, args, _) ->
      let constr = quote_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | [arg] -> Some (quote_expression transl stage arg)
        | _ :: _ ->
          let args = List.map (quote_expression transl stage) args in
          let with_labels =
            List.map
              (fun a -> Label.Nonoptional.wrap Label.Nonoptional.no_label, a)
              args
          in
          let as_tuple = Exp_desc.tuple loc with_labels |> Exp_desc.wrap in
          Some (mk_exp_noattr loc as_tuple)
      in
      Exp_desc.construct loc constr args
    | Texp_variant (variant, argo) ->
      let variant = quote_variant loc variant
      and argo =
        Option.map (fun (arg, _) -> quote_expression transl stage arg) argo
      in
      Exp_desc.variant loc variant argo
    | Texp_record record ->
      let lbl_exps =
        Array.map
          (fun (lbl, def) ->
            let lbl = quote_record_field env loc lbl in
            let exp =
              match def with
              | Overridden (_, exp) -> quote_expression transl stage exp
              | Kept _ ->
                fatal_error "No support for record update syntax in quotations"
            in
            lbl, exp)
          record.fields
      in
      let base =
        Option.map
          (fun (e, _, _) -> quote_expression transl stage e)
          record.extended_expression
      in
      Exp_desc.record loc (Array.to_list lbl_exps) base
    | Texp_field (rcd, _, lid, lbl, _, _) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_record_field env lid.loc lbl in
      Exp_desc.field loc rcd lbl
    | Texp_setfield (rcd, _, lid, lbl, exp) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_record_field env lid.loc lbl in
      let exp = quote_expression transl stage exp in
      Exp_desc.setfield loc rcd lbl exp
    | Texp_array (_, _, exps, _) ->
      let exps = List.map (quote_expression transl stage) exps in
      Exp_desc.array loc exps
    | Texp_ifthenelse (cond, then_, else_) ->
      let cond = quote_expression transl stage cond in
      let then_ = quote_expression transl stage then_ in
      let else_ = Option.map (quote_expression transl stage) else_ in
      Exp_desc.ifthenelse loc cond then_ else_
    | Texp_sequence (exp1, _, exp2) ->
      let exp1 = quote_expression transl stage exp1 in
      let exp2 = quote_expression transl stage exp2 in
      Exp_desc.sequence loc exp1 exp2
    | Texp_while wh ->
      let cond = quote_expression transl stage wh.wh_cond in
      let body = quote_expression transl stage wh.wh_body in
      Exp_desc.while_ loc cond body
    | Texp_for floop ->
      let low = quote_expression transl stage floop.for_from
      and high = quote_expression transl stage floop.for_to
      and dir =
        match floop.for_dir with
        | Asttypes.Upto -> true
        | Asttypes.Downto -> false
      and name = quote_name (mkloc (Ident.name floop.for_id) loc) in
      with_new_idents_values [floop.for_id];
      let body = quote_expression transl stage floop.for_body in
      without_idents_values [floop.for_id];
      Exp_desc.for_simple loc (quote_loc loc) name low high dir
        (Lam.func Var_value extract floop.for_id body)
    | Texp_send (obj, meth, _) ->
      let obj = quote_expression transl stage obj in
      let meth = quote_method loc meth in
      Exp_desc.send loc obj meth
    | Texp_open _ -> fatal_error "No support for opening modules yet."
    | Texp_letmodule (ident, _, _, mod_exp, body) -> (
      let mod_exp = quote_module_exp transl stage loc mod_exp in
      match ident with
      | None ->
        Exp_desc.letmodule_nonbinding loc mod_exp
          (quote_expression transl stage body)
      | Some ident ->
        let name = quote_name (mkloc (Ident.name ident) loc) in
        with_new_idents_modules [ident];
        let body = quote_expression transl stage body in
        without_idents_modules [ident];
        Exp_desc.letmodule loc (quote_loc loc) name mod_exp
          (Lam.func Var_module extract ident body))
    | Texp_assert (exp, _) ->
      let exp = quote_expression transl stage exp in
      Exp_desc.assert_ loc exp
    | Texp_lazy exp ->
      let exp = quote_expression transl stage exp in
      Exp_desc.lazy_ loc exp
    | Texp_quotation exp ->
      let exp = quote_expression transl (stage + 1) exp in
      Exp_desc.quote loc exp
    | Texp_antiquotation exp ->
      if stage > 0
      then
        let exp = quote_expression transl (stage - 1) exp in
        Exp_desc.antiquote loc exp
      else Exp_desc.splice loc (Code.inject (transl exp))
    | Texp_new (path, _, _, _) ->
      Exp_desc.new_ loc (quote_value_ident_path loc path)
    | Texp_pack m -> Exp_desc.pack loc (quote_module_exp transl stage loc m)
    | Texp_unreachable -> Exp_desc.unreachable
    | Texp_src_pos -> Exp_desc.src_pos
    | Texp_exclave e -> Exp_desc.exclave loc (quote_expression transl stage e)
    | Texp_extension_constructor (_, path) ->
      let name = Name.wrap (Name.mk loc (Path.name path)) in
      Exp_desc.extension_constructor loc name
    | Texp_unboxed_tuple ts ->
      let tups =
        List.map
          (fun (lab_opt, exp, _) ->
            quote_nonopt loc lab_opt, quote_expression transl stage exp)
          ts
      in
      Exp_desc.unboxed_tuple loc tups
    | Texp_record_unboxed_product record ->
      let lbl_exps =
        Array.map
          (fun (lbl, def) ->
            let lbl = quote_record_field env loc lbl in
            let exp =
              match def with
              | Overridden (_, exp) -> quote_expression transl stage exp
              | Kept _ ->
                fatal_error "No support for record update syntax in quotations."
            in
            lbl, exp)
          record.fields
      in
      let base =
        Option.map
          (fun (e, _) -> quote_expression transl stage e)
          record.extended_expression
      in
      Exp_desc.unboxed_record_product loc (Array.to_list lbl_exps) base
    | Texp_unboxed_field (rcd, _, lid, lbl, _) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_record_field env lid.loc lbl in
      Exp_desc.unboxed_field loc rcd lbl
    | Texp_letexception (ext_const, exp) ->
      let exp = quote_expression transl stage exp in
      Exp_desc.let_exception loc (quote_name ext_const.ext_name) exp
    | Texp_letop rcd ->
      let let_l = quote_value_ident_path rcd.let_.bop_loc rcd.let_.bop_op_path
      and ands_l =
        List.map
          (fun bop -> quote_value_ident_path bop.bop_loc bop.bop_op_path)
          rcd.ands
      and defs =
        quote_expression transl stage rcd.let_.bop_exp
        :: List.map (fun d -> quote_expression transl stage d.bop_exp) rcd.ands
      and body = quote_value_pattern_case transl stage loc rcd.body in
      Exp_desc.let_op loc (let_l :: ands_l) defs body
    | Texp_list_comprehension compr ->
      Exp_desc.list_comprehension loc
        (quote_comprehension transl stage loc compr)
    | Texp_array_comprehension (_, _, compr) ->
      Exp_desc.array_comprehension loc
        (quote_comprehension transl stage loc compr)
    | Texp_overwrite _ -> fatal_error "Not implemented yet"
    | Texp_hole _ -> fatal_error "No support for typed holes inside quotations."
    | Texp_instvar _ | Texp_setinstvar _ | Texp_override _ ->
      fatal_error "Should not encounter OOP syntax in quotes."
    | Texp_object _ -> fatal_error "Cannot quote object construction."
    | Texp_probe _ | Texp_probe_is_enabled _ ->
      fatal_error "Cannot quote probing constructs."
    | Texp_mutvar _ | Texp_letmutable _ | Texp_setmutvar _ ->
      fatal_error "Cannot quote constructs related to mutable variables."
    | Texp_atomic_loc _ ->
      fatal_error "Cannot quote Texp_atomic_loc constructs yet."
    | Texp_idx _ -> fatal_error "Cannot quote Texp_idx constructs yet."
    | Texp_eval (typ, _) -> Exp_desc.eval loc (quote_core_type typ)
  in
  List.iter update_env_without_extra e.exp_extra;
  List.fold_right
    (quote_expression_extra transl stage)
    e.exp_extra (Exp_desc.wrap body)

and quote_expression transl stage e =
  let desc = quote_expression_desc transl stage e
  and attributes = quote_attributes e
  and loc = e.exp_loc in
  Exp.mk loc desc attributes |> Exp.wrap

let transl_quote transl exp loc =
  let exp_quoted = quote_expression transl 0 exp in
  let code =
    if Hashtbl.length vars_env.env_poly = 0
    then Code.of_exp loc (quote_loc loc) exp_quoted
    else
      let free_type_vars = Hashtbl.to_seq vars_env.env_poly |> List.of_seq in
      let type_names =
        List.map (fun p -> Name.mk loc (fst p) |> Name.wrap) free_type_vars
      in
      let type_idents = List.map (fun p -> fst (snd p)) free_type_vars in
      let quote_fun =
        Lam.list_param_binding Var_type_var extract type_idents exp_quoted
      in
      Code.of_exp_with_type_vars loc (quote_loc loc) type_names quote_fun
  in
  extract (Code.wrap code)
