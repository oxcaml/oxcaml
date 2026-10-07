(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

module Hash = struct
  type t = int32

  let of_int32 t = t

  let to_int32 t = t

  let equal = Int32.equal

  let compare = Int32.unsigned_compare

  let is_function_entry t = Int32.equal (Int32.logand t 1l) 1l

  (* Position hashes record whether the position is a function entry. This is
     because oxcaml-fdo-decode computes a "call target index" from the profile
     (which call sites reached which functions), which cannot be known
     statically (e.g. for indirect calls). This index is then used for function
     reordering. *)
  let of_prehash ~is_function_entry prehash =
    Int32.logor
      (Int32.logand (Fdo_prehash.to_int32 prehash) (-2l))
      (if is_function_entry then 1l else 0l)

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    (* Already mixed. *)
    let hash t = Int32.to_int t land max_int
  end)
end

module Function_body_hash = struct
  type t = int32

  let of_int32 t = t

  let to_int32 t = t

  let equal = Int32.equal
end

(* The kinds of node hashed into an identity. Their numbers are part of the
   on-disk format: changing one changes every hash. *)
module Tag = struct
  type t =
    | Then
    | Else
    | Callsite
    | Switch_case
    | Original
    | Specialized
    | Interior
    | Instantiation_site

  let to_int = function
    | Then -> 0
    | Else -> 1
    | Callsite -> 2
    | Switch_case -> 3
    | Original -> 4
    | Specialized -> 5
    | Interior -> 6
    | Instantiation_site -> 7
end

let prehash_node tag components =
  Fdo_prehash.node ~tag:(Tag.to_int tag) components

module Edge = struct
  type t =
    | Then
    | Else
    | Switch_case of int
    | Callsite

  let prehash = function
    | Then -> prehash_node Tag.Then []
    | Else -> prehash_node Tag.Else []
    | Callsite -> prehash_node Tag.Callsite []
    | Switch_case n -> prehash_node Tag.Switch_case [Fdo_prehash.of_int n]

  let equal a b =
    match a, b with
    | Then, Then | Else, Else | Callsite, Callsite -> true
    | Switch_case a, Switch_case b -> Int.equal a b
    | (Then | Else | Callsite | Switch_case _), _ -> false

  let to_string = function
    | Then -> "then"
    | Else -> "else"
    | Switch_case n -> Printf.sprintf "case(%d)" n
    | Callsite -> "call"
end

(* Specializations refer to positions, whose containing functions may themselves
   be specialized, so the two types are mutually recursive. They are defined
   here and given their own modules below. *)

(* Definitions can be functions or modules. *)
type definition_id =
  | Original of
      { demangled_name : string;
        discriminator : int;
        prehash : Fdo_prehash.t
      }
  | Specialized of
      { unspecialized : definition_id;
        specialization_site : position;
        prehash : Fdo_prehash.t
      }

and position =
  | Function_entry of definition_id
  | Interior of
      { function_id : definition_id;
        function_body_hash : Function_body_hash.t;
        ast_pos : int;
        edge : Edge.t;
        prehash : Fdo_prehash.t
      }
  (* Like [module M = F (...)] *)
  | Instantiation_site of
      { module_binding : definition_id;
        prehash : Fdo_prehash.t
      }

let prehash_of_definition_id = function
  | Original { prehash; _ } | Specialized { prehash; _ } -> prehash

let prehash_of_position = function
  | Function_entry fn -> prehash_of_definition_id fn
  | Interior { prehash; _ } | Instantiation_site { prehash; _ } -> prehash

let rec equal_definition_id a b =
  match a, b with
  | Original a, Original b ->
    String.equal a.demangled_name b.demangled_name
    && Int.equal a.discriminator b.discriminator
  | Specialized a, Specialized b ->
    equal_definition_id a.unspecialized b.unspecialized
    && equal_position a.specialization_site b.specialization_site
  | (Original _ | Specialized _), _ -> false

and equal_position a b =
  match a, b with
  | Function_entry a, Function_entry b -> equal_definition_id a b
  | Instantiation_site a, Instantiation_site b ->
    equal_definition_id a.module_binding b.module_binding
  | Interior a, Interior b ->
    equal_definition_id a.function_id b.function_id
    && Function_body_hash.equal a.function_body_hash b.function_body_hash
    && Int.equal a.ast_pos b.ast_pos
    && Edge.equal a.edge b.edge
  | (Function_entry _ | Interior _ | Instantiation_site _), _ -> false

let rec definition_id_to_string = function
  | Original { demangled_name; discriminator; prehash = _ } ->
    Printf.sprintf "%S:%d" demangled_name discriminator
  | Specialized { unspecialized; specialization_site; prehash = _ } ->
    Printf.sprintf "%s@(%s)"
      (definition_id_to_string unspecialized)
      (position_to_string specialization_site)

and position_to_string = function
  | Function_entry fn -> definition_id_to_string fn
  | Interior { function_id; function_body_hash; ast_pos; edge; prehash = _ } ->
    Printf.sprintf "%s:%08lx:%d:%s"
      (definition_id_to_string function_id)
      function_body_hash ast_pos (Edge.to_string edge)
  | Instantiation_site { module_binding; prehash = _ } ->
    Printf.sprintf "module(%s)" (definition_id_to_string module_binding)

module Definition_id = struct
  type t = definition_id

  let create_original ~demangled_name ~discriminator =
    let prehash =
      prehash_node Tag.Original
        [Fdo_prehash.of_string demangled_name; Fdo_prehash.of_int discriminator]
    in
    Original { demangled_name; discriminator; prehash }

  let create_specialized ~unspecialized ~specialization_site =
    let prehash =
      prehash_node Tag.Specialized
        [ prehash_of_definition_id unspecialized;
          prehash_of_position specialization_site ]
    in
    Specialized { unspecialized; specialization_site; prehash }

  let demangled_name = function
    | Original { demangled_name; _ } -> Some demangled_name
    | Specialized _ -> None

  let hash t =
    Hash.of_prehash ~is_function_entry:true (prehash_of_definition_id t)

  let equal = equal_definition_id

  let to_string = definition_id_to_string
end

module Position = struct
  type t = position

  let create_function_entry fn = Function_entry fn

  let create_interior ~function_id ~function_body_hash ~ast_pos ~edge =
    let prehash =
      prehash_node Tag.Interior
        [ prehash_of_definition_id function_id;
          Fdo_prehash.of_int (Int32.to_int function_body_hash);
          Fdo_prehash.of_int ast_pos;
          Edge.prehash edge ]
    in
    Interior { function_id; function_body_hash; ast_pos; edge; prehash }

  let create_instantiation_site module_binding =
    let prehash =
      prehash_node Tag.Instantiation_site
        [prehash_of_definition_id module_binding]
    in
    Instantiation_site { module_binding; prehash }

  let definition_id = function
    | Function_entry fn
    | Interior { function_id = fn; _ }
    | Instantiation_site { module_binding = fn; _ } ->
      fn

  let function_body_hash = function
    | Interior { function_body_hash; _ } -> Some function_body_hash
    | Function_entry _ | Instantiation_site _ -> None

  let is_function_entry = function
    | Function_entry _ -> true
    | Interior _ | Instantiation_site _ -> false

  let hash t =
    Hash.of_prehash ~is_function_entry:(is_function_entry t)
      (prehash_of_position t)

  let equal = equal_position

  let to_string = position_to_string

  let specialize t ~at =
    let definition_id =
      List.fold_left
        (fun unspecialized specialization_site ->
          (match specialization_site with
          | Interior { edge = Callsite; _ } | Instantiation_site _ -> ()
          | Function_entry _
          | Interior { edge = Then | Else | Switch_case _; _ } ->
            Misc.fatal_errorf "Fdo_counter.specialize: %s is not a call site"
              (position_to_string specialization_site));
          Definition_id.create_specialized ~unspecialized ~specialization_site)
        (definition_id t) at
    in
    match t with
    | Function_entry _ -> create_function_entry definition_id
    | Interior { function_body_hash; ast_pos; edge; _ } ->
      create_interior ~function_id:definition_id ~function_body_hash ~ast_pos
        ~edge
    | Instantiation_site _ -> create_instantiation_site definition_id
end
