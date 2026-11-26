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

(* CR-someday yusumez: consider emitting this metadata as an FDO-only DWARF
   compilation unit in .debug_info instead, with vendor tags and attributes, the
   innermost position of each stack inline and the rest as references to a trie
   of inlining contexts. Standard DWARF tools could then read it, and new
   attributes would not break older decoders. *)

let section_name = ".debug_fdo_metadata"

let magic = "FDOM"

let version = 20

(* CR-someday yusumez: Under recursion, the activation that catches an exception
   can be an outer one rather than the one of the nearest frame that
   [Unwind_to_handler] unwinds to. The decoder then keeps the context of the
   inner activation, which is wrong rather than lost. LBR records no stack
   pointer that would tell them apart. *)

module Operand = struct
  type 'address t =
    | Return_address of 'address
    | Target of 'address
end

module Action = struct
  type 'address t =
    | Count_taken of Fdo_counter.Hashed.t
    | Count_fallthrough of Fdo_counter.Hashed.t
    | Count_executed of Fdo_counter.Hashed.t
    | Count_call of
        { return_address : 'address;
          counter : Fdo_counter.Hashed.t
        }
    | Count_tail_call of Fdo_counter.Hashed.t
    | Count_indirect_jump of
        { target : 'address;
          counter : Fdo_counter.Hashed.t
        }
    | Unwind_to_handler

  let counter = function
    | Count_taken counter
    | Count_fallthrough counter
    | Count_executed counter
    | Count_tail_call counter
    | Count_call { counter; _ }
    | Count_indirect_jump { counter; _ } ->
      Some counter
    | Unwind_to_handler -> None

  let operand : _ t -> _ Operand.t option = function
    | Count_call { return_address; _ } -> Some (Return_address return_address)
    | Count_indirect_jump { target; _ } -> Some (Target target)
    | Count_taken _ | Count_fallthrough _ | Count_executed _ | Count_tail_call _
    | Unwind_to_handler ->
      None
end

module Kind = struct
  type t =
    | Count_taken
    | Count_fallthrough
    | Count_executed
    | Count_call
    | Count_tail_call
    | Count_indirect_jump
    | Unwind_to_handler

  let all =
    [ Count_taken;
      Count_fallthrough;
      Count_executed;
      Count_call;
      Count_tail_call;
      Count_indirect_jump;
      Unwind_to_handler ]

  let of_action : _ Action.t -> t = function
    | Count_taken _ -> Count_taken
    | Count_fallthrough _ -> Count_fallthrough
    | Count_executed _ -> Count_executed
    | Count_call _ -> Count_call
    | Count_tail_call _ -> Count_tail_call
    | Count_indirect_jump _ -> Count_indirect_jump
    | Unwind_to_handler -> Unwind_to_handler

  let on_taken = function
    | Count_taken | Count_executed | Count_call | Count_tail_call
    | Count_indirect_jump | Unwind_to_handler ->
      true
    | Count_fallthrough -> false

  let on_fallthrough = function
    | Count_fallthrough | Count_executed | Unwind_to_handler -> true
    | Count_taken | Count_call | Count_tail_call | Count_indirect_jump -> false

  let has_counter = function
    | Count_taken | Count_fallthrough | Count_executed | Count_call
    | Count_tail_call | Count_indirect_jump ->
      true
    | Unwind_to_handler -> false

  let operand : t -> unit Operand.t option = function
    | Count_call -> Some (Return_address ())
    | Count_indirect_jump -> Some (Target ())
    | Count_taken | Count_fallthrough | Count_executed | Count_tail_call
    | Unwind_to_handler ->
      None

  let to_action t ~(operand : _ Operand.t option) ~counter : _ Action.t option =
    match t, operand, counter with
    | Count_taken, None, Some counter -> Some (Count_taken counter)
    | Count_fallthrough, None, Some counter -> Some (Count_fallthrough counter)
    | Count_executed, None, Some counter -> Some (Count_executed counter)
    | Count_tail_call, None, Some counter -> Some (Count_tail_call counter)
    | Count_call, Some (Return_address return_address), Some counter ->
      Some (Count_call { return_address; counter })
    | Count_indirect_jump, Some (Target target), Some counter ->
      Some (Count_indirect_jump { target; counter })
    | Unwind_to_handler, None, None -> Some Unwind_to_handler
    | ( ( Count_taken | Count_fallthrough | Count_executed | Count_tail_call
        | Count_call | Count_indirect_jump | Unwind_to_handler ),
        _,
        _ ) ->
      None

  let code = function
    | Count_call -> 0
    | Count_tail_call -> 1
    | Count_taken | Count_fallthrough | Count_executed -> 2
    | Unwind_to_handler -> 3
    | Count_indirect_jump -> 4

  let on_fallthrough_bit = 1

  let on_taken_bit = 2

  let to_flags t =
    let bit on b = if on t then b else 0 in
    (code t lsl 2)
    lor bit on_taken on_taken_bit
    lor bit on_fallthrough on_fallthrough_bit

  let of_flags n = List.find_opt (fun t -> to_flags t = n) all
end

module Counter_table = struct
  module Node = struct
    type t =
      { hash : Fdo_counter.Position_hash.t;
        context : int option
      }
  end

  module Builder = struct
    type t =
      { indexes : (int32 * int option, int) Hashtbl.t;
        mutable nodes : Node.t list  (** In reverse index order. *)
      }

    let create () = { indexes = Hashtbl.create 64; nodes = [] }

    let intern t ({ hash; context } : Node.t) =
      let key = Fdo_counter.Position_hash.to_int32 hash, context in
      match Hashtbl.find_opt t.indexes key with
      | Some index -> index
      | None ->
        let index = Hashtbl.length t.indexes in
        Hashtbl.add t.indexes key index;
        t.nodes <- { hash; context } :: t.nodes;
        index

    let add t counter =
      match
        List.fold_right
          (fun hash context -> Some (intern t { hash; context }))
          counter None
      with
      | Some index -> index
      | None -> invalid_arg "Fdo_metadata_format.Counter_table: empty counter"

    let nodes t = List.rev t.nodes
  end

  type t = Fdo_counter.Hashed.t array

  let of_nodes nodes =
    let nodes = Array.of_list nodes in
    let counters = Array.make (Array.length nodes) [] in
    let resolve (index, ({ hash; context } : Node.t)) =
      match context with
      | None ->
        counters.(index) <- [hash];
        true
      | Some context when 0 <= context && context < index ->
        counters.(index) <- hash :: counters.(context);
        true
      | Some _ -> false
    in
    if Seq.for_all resolve (Array.to_seqi nodes) then Some counters else None

  let find t index =
    if 0 <= index && index < Array.length t then Some t.(index) else None
end
