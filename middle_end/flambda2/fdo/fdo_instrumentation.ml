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

module FC = Fdo_counter
module String = Misc.Stdlib.String

type t =
  { function_ : FC.Function.t;
    body_hash : FC.Body_hash.t;
    mutable next_index : int;
        (** For a position in the lambda body of the current function *)
    module_call_indices : int String.Tbl.t
        (** The next index of the calls that bind a module-level result, by the
            path of the binding. *)
  }

let create ~function_ ~body_hash =
  { function_;
    body_hash;
    next_index = 0;
    module_call_indices = String.Tbl.create 3
  }

let entry_annotation t =
  Fdo_annotation.With_body_hash.create
    ~annotation:
      (Fdo_annotation.singleton
         (FC.of_position (FC.Position.create_function_entry t.function_)))
    ~body_hash:(Some t.body_hash)

let fresh_index t =
  let index = t.next_index in
  t.next_index <- index + 1;
  index

(* [Some path] if [loc] is that of a call that binds a module-level result with
   scope path [path] (see [Fdo_counter_identity] for why we care). *)
let module_level_call_binding (loc : Lambda.scoped_location) =
  match loc with
  | Loc_unknown -> None
  | Loc_known { scopes; loc = _ } -> (
    match scopes with
    | Cons { item = Sc_module_definition; name; prev; _ } ->
      let is_compilation_unit =
        match prev with Empty -> true | Cons _ -> false
      in
      (* Treat calls without a binding of its own (e.g. in a toplevel [let () =
         ...] [include F (X)] or [module _ = F (X)]) as interior positions.
         These have no stable binders to anchor to. *)
      if String.equal name "" || is_compilation_unit
      then None
      else
        Some
          (Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
             scopes)
    | Empty
    | Cons
        { item =
            ( Sc_anonymous_function | Sc_value_definition | Sc_class_definition
            | Sc_method_definition | Sc_partial_or_eta_wrapper | Sc_lazy );
          _
        } ->
      None)

let call t loc =
  let position =
    match module_level_call_binding loc with
    | None ->
      FC.Position.create_call ~function_:t.function_ ~body_hash:t.body_hash
        ~index:(fresh_index t)
    | Some path ->
      (* Multiple calls can have the same binder, e.g. [module M = F (G (X))] *)
      let index =
        Option.value (String.Tbl.find_opt t.module_call_indices path) ~default:0
      in
      String.Tbl.replace t.module_call_indices path (index + 1);
      let binder = FC.Name.create ~name_str:path ~discriminator:0 in
      FC.Position.create_module_level_call ~function_:t.function_ ~binder ~index
  in
  FC.of_position position

module Switch_counters = struct
  type t =
    { case : int -> Fdo_annotation.t;
      failaction : Fdo_annotation.t
    }

  let empty =
    { case = (fun _ -> Fdo_annotation.empty);
      failaction = Fdo_annotation.empty
    }

  let case t n = t.case n

  let failaction t = t.failaction
end

module Lambda_switch_counters = struct
  type t =
    { consts : Switch_counters.t;
      blocks : Switch_counters.t;
      is_int : Switch_counters.t
    }

  let consts t = t.consts

  let blocks t = t.blocks

  let is_int t = t.is_int
end

(* The constant or the block side of a [Lambda] switch. A switch with both
   translates to a switch per side behind the dispatch on whether the scrutinee
   is an immediate. A side whose discriminants all go to one arm becomes a jump
   (in [Closure_conversion.close_switch] or later), which has no counters, so
   that arm's counters go on the dispatch instead. *)
module Side = struct
  type t =
    { counters : Switch_counters.t;
      single_arm : Fdo_annotation.t option
          (** The counters of the only arm the side reaches, if there is one *)
    }

  let create ~cases ~num ~has_default ~counter ~failaction =
    let counters : Switch_counters.t =
      { case =
          (fun n -> if List.mem_assoc n cases then counter n else failaction);
        failaction
      }
    in
    (* Whether a discriminant has no case of its own. The keys of [cases] are
       distinct and below [num]. *)
    let reaches_default =
      has_default && List.compare_length_with cases num < 0
    in
    let single_arm =
      match cases, reaches_default with
      | [], true -> Some failaction
      | [(key, _)], false -> Some (counter key)
      | [], false | _ :: _, true | _ :: _ :: _, false -> None
    in
    { counters; single_arm }

  let switch_counters t =
    match t.single_arm with
    | None -> t.counters
    | Some _ -> Switch_counters.empty

  let dispatch_counters t =
    Option.value t.single_arm ~default:Fdo_annotation.empty
end

(* Arm 0 of a [Lambda] switch is its default arm, followed by its constants and
   then its tags. *)
let switch t
    ({ sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction } :
      Lambda.lambda_switch) : Lambda_switch_counters.t =
  let index = fresh_index t in
  let counter arm =
    FC.Position.create_branch ~function_:t.function_ ~body_hash:t.body_hash
      ~index ~arm
    |> FC.of_position |> Fdo_annotation.singleton
  in
  let has_default = Option.is_some sw_failaction in
  let failaction = if has_default then counter 0 else Fdo_annotation.empty in
  let side cases ~num ~first_arm =
    Side.create ~cases ~num ~has_default
      ~counter:(fun n -> counter (first_arm + n))
      ~failaction
  in
  let consts = side sw_consts ~num:sw_numconsts ~first_arm:1 in
  let blocks = side sw_blocks ~num:sw_numblocks ~first_arm:(1 + sw_numconsts) in
  { consts = Side.switch_counters consts;
    blocks = Side.switch_counters blocks;
    is_int =
      { case =
          (function
          | 0 -> Side.dispatch_counters blocks
          | 1 -> Side.dispatch_counters consts
          | _ -> Fdo_annotation.empty);
        failaction = Fdo_annotation.empty
      }
  }
