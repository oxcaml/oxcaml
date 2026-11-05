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
      let path =
        Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
          scopes
      in
      let is_compilation_unit =
        match prev with Empty -> true | Cons _ -> false
      in
      let anonymous = String.equal name "" || is_compilation_unit in
      (* If a call has no binding of its own, append ".(module)" to its path.
         This can happen, for instance, in a toplevel [include F (X)] or [module
         _ = F (X)]. *)
      Some (if anonymous then path ^ ".(module)" else path)
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

  let case t n = t.case n

  let failaction t = t.failaction
end

(* The arms of a [Lambda] switch are numbered by constant, then by tag, then the
   default arm. *)
let switch t
    ({ sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction } :
      Lambda.lambda_switch) =
  let index = fresh_index t in
  let counter arm =
    FC.Position.create_branch ~function_:t.function_ ~body_hash:t.body_hash
      ~index ~arm
    |> FC.of_position |> Fdo_annotation.singleton
  in
  let failaction =
    match sw_failaction with
    | None -> Fdo_annotation.empty
    | Some _ -> counter (sw_numconsts + sw_numblocks)
  in
  let switch_counters cases ~first_arm : Switch_counters.t =
    { case =
        (fun n ->
          if List.mem_assoc n cases then counter (first_arm + n) else failaction);
      failaction
    }
  in
  let consts = switch_counters sw_consts ~first_arm:0 in
  let blocks = switch_counters sw_blocks ~first_arm:sw_numconsts in
  ~consts, ~blocks
