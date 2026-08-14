(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Sort = struct
  type t =
    | Bool
    | Int
    | Bitvec of int
    | Uninterpreted of string
    | Datatype of string

  let equal (a : t) (b : t) = a = b

  let key = function
    | Bool -> "Bool"
    | Int -> "Int"
    | Bitvec width -> "Bv" ^ Int.to_string width
    | Uninterpreted name -> name
    | Datatype name -> name
end

module Op = struct
  type t =
    | Not
    | And
    | Or
    | Implies
    | Eq
    | Distinct
    | Neg
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Le
    | Gt
    | Ge
    | Bv_neg
    | Bv_add
    | Bv_sub
    | Bv_mul
    | Bv_sdiv
    | Bv_srem
    | Bv_not
    | Bv_and
    | Bv_or
    | Bv_xor
    | Bv_shl
    | Bv_lshr
    | Bv_ashr
    | Bv_slt
    | Bv_sle
    | Bv_sgt
    | Bv_sge
end

module Literal = struct
  type t =
    | Bool of bool
    | Int of string
    | Bitvec of { width : int; value : int64 }

  let ocaml_int n = Bitvec { width = 63; value = Int64.of_int n }
end

module Term = struct
  type t =
    | Var of string
    | Const of Literal.t
    | App of Op.t * t list
    | Call of string * t list
    | Ite of t * t * t
    | Construct of string * t list
    | Select of string * int * t
    | Test of string * t
end

module Origin = struct
  type t =
    { label : string
    ; location : Location.t
    }
end

module Datatype = struct
  type ty =
    | Bool
    | Int
    | Bitvec of int
    | Uninterpreted of string
    | Param of string
    | Apply of string * ty list
    | Arrow of ty * ty

  type constructor =
    { constructor_name : string
    ; fields : (string * ty) list
    }

  type decl =
    { decl_name : string
    ; params : string list
    ; constructors : constructor list
    }
end

module Signature = struct
  type constructor =
    { constructor_name : string
    ; fields : (string * Sort.t) list
    }

  type datatype =
    { datatype_name : string
    ; constructors : constructor list
    }

  type t =
    { sorts : string list
    ; datatypes : datatype list
    ; variables : (string * Sort.t) list
    ; functions : (string * Sort.t list * Sort.t) list
    }

  let empty = { sorts = []; datatypes = []; variables = []; functions = [] }

  exception Instantiate_error of string

  let error fmt =
    Format.kasprintf (fun message -> raise (Instantiate_error message)) fmt

  let mangle name = function
    | [] -> name
    | arguments ->
      name ^ "<" ^ String.concat "," (List.map Sort.key arguments) ^ ">"

  let instantiate (decls : Datatype.decl list) roots =
    let find_decl name : Datatype.decl =
      match
        List.filter
          (fun (decl : Datatype.decl) -> String.equal decl.decl_name name)
          decls
      with
      | [decl] -> decl
      | [] -> error "unknown datatype %s" name
      | _ :: _ :: _ -> error "duplicate datatype declaration %s" name
    in
    (* Ground instances already produced, keyed by mangled name, in
       discovery order (the renderer reorders anyway). *)
    let completed : datatype list ref = ref [] in
    (* Instance names must be injective: [Sort.key] gives [Int] and an
       uninterpreted sort named "Int" the same key, so two different
       instantiations could otherwise silently alias one instance. *)
    let started : (string, string * Sort.t list) Hashtbl.t =
      Hashtbl.create 16
    in
    (* Instances whose fields are being expanded.  A recursive use at
       different arguments while expanding would demand infinitely many
       instances: that is non-regular recursion. *)
    let in_progress = ref [] in
    let rec instance name (arguments : Sort.t list) =
      let decl = find_decl name in
      if List.length decl.params <> List.length arguments
      then
        error "datatype %s expects %d argument(s) but was given %d" name
          (List.length decl.params)
          (List.length arguments);
      (match List.assoc_opt name !in_progress with
       | Some expanding_arguments
         when not (List.equal Sort.equal expanding_arguments arguments) ->
         error "non-regular recursive datatype %s is not supported" name
       | Some _ | None -> ());
      let instance_name = mangle name arguments in
      (match Hashtbl.find_opt started instance_name with
       | Some (started_name, started_arguments)
         when not
                (String.equal started_name name
                 && List.equal Sort.equal started_arguments arguments) ->
         error
           "two distinct instantiations produce the same instance name %s"
           instance_name
       | Some _ | None -> ());
      if not (Hashtbl.mem started instance_name)
      then begin
        Hashtbl.add started instance_name (name, arguments);
        in_progress := (name, arguments) :: !in_progress;
        let subst = List.combine decl.params arguments in
        let constructors =
          List.map
            (fun (constructor : Datatype.constructor) : constructor ->
               { constructor_name =
                   mangle constructor.constructor_name arguments
               ; fields =
                   List.map
                     (fun (field_name, field_ty) ->
                        mangle field_name arguments, ground subst field_ty)
                     constructor.fields
               })
            decl.constructors
        in
        in_progress := List.tl !in_progress;
        completed :=
          { datatype_name = instance_name; constructors } :: !completed
      end;
      Sort.Datatype instance_name
    and ground subst : Datatype.ty -> Sort.t = function
      | Bool -> Bool
      | Int -> Int
      | Bitvec width -> Bitvec width
      | Uninterpreted name -> Uninterpreted name
      | Param param ->
        (match List.assoc_opt param subst with
         | Some sort -> sort
         | None -> error "unbound datatype parameter '%s" param)
      | Apply (name, arguments) ->
        instance name (List.map (ground subst) arguments)
      | Arrow _ -> error "function-valued datatype fields are not supported"
    in
    match
      List.map
        (fun (name, arguments) ->
           instance name (List.map (ground []) arguments))
        roots
    with
    | root_sorts -> Ok (List.rev !completed, root_sorts)
    | exception Instantiate_error message -> Error message
end

module Obligation = struct
  type hypothesis =
    { id : int
    ; term : Term.t
    ; origin : Origin.t
    }

  type t =
    { signature : Signature.t
    ; hypotheses : hypothesis list
    ; goal : Term.t
    ; location : Location.t
    }
end
