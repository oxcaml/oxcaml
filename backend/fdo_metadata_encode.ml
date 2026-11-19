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

module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label
module Hash = Fdo_counter.Position_hash

module Event = struct
  type t =
    | Branch of
        { taken : Fdo_annotation.t;
          fallthrough : Fdo_annotation.t
        }
    | Call of
        { return_address : L.t;
          counter : Fdo_counter.t
        }
    | Tailcall of Fdo_counter.t
    | Jump of
        { target : L.t;
          taken : Fdo_annotation.t
        }
    | Reset
end

type kind =
  | Taken  (** a branch from here was taken *)
  | Fallthrough  (** instruction was executed without taking a branch *)
  | Exec  (** instruction was executed *)

type action =
  | Call of
      { return_address : L.t;
        stack : Fdo_counter.Hashed.t
      }
  | Tailcall of Fdo_counter.Hashed.t
  | Normal of Fdo_counter.Hashed.t
  | Jump of
      { target : L.t;
        stack : Fdo_counter.Hashed.t
      }
  | Reset

type piece =
  { start : L.t;
    finish : L.t;
    operations : (L.t * kind * action) list
  }

type t =
  { record_names : bool;
    mutable pieces : piece list;  (** the functions so far, most recent first *)
    names : string Hash.Tbl.t;
    body_hashes : Fdo_counter.Body_hash.t Hash.Tbl.t;
        (** of every compiled function, whether or not it executes *)
    mutable dropped :
      (Fdo_dropped_counters.Reason.t * Fdo_counter.Hashed.t) list
  }

type function_metadata =
  { unit_metadata : t;
    start : L.t;
    mutable operations : (L.t * kind * action) list  (** most recent first *)
  }

let create ~record_names =
  { record_names;
    pieces = [];
    names = Hash.Tbl.create 64;
    body_hashes = Hash.Tbl.create 64;
    dropped = []
  }

let remember t hash name =
  (match Hash.Tbl.find_opt t.names hash with
  | Some old when not (String.equal old name) ->
    Misc.fatal_errorf "FDO hash collision %08lx: %s and %s" (Hash.to_int32 hash)
      old name
  | Some _ | None -> ());
  Hash.Tbl.replace t.names hash name

let hash_counter t counter =
  if t.record_names
  then
    List.iter
      (fun position ->
        remember t
          (Fdo_counter.Position.hash position)
          (Fdo_counter.Position.to_string position))
      (Fdo_counter.position counter :: Fdo_counter.inlining_stack counter);
  Fdo_counter.hash counter

let hash_annotation t annotation =
  List.map (hash_counter t) (Fdo_annotation.to_list annotation)

let emit fn kind label action =
  fn.operations <- (label, kind, action) :: fn.operations

let record fn kind label ?(action = fun stack -> Normal stack) stacks =
  (* Group common outer contexts for metadata-order compression. Every action
     still specifies its complete within-function stack, innermost first. *)
  List.map List.rev stacks
  |> List.sort_uniq (List.compare Hash.compare)
  |> List.iter (function
    | [] -> ()
    | path -> emit fn kind label (action (List.rev path)))

let record_body t ~function_ ~body_hash =
  let hash = Fdo_counter.Function.entry_position_hash function_ in
  (* There could be hash collisions here, but this is fine as it will be very
     rare. *)
  Hash.Tbl.replace t.body_hashes hash body_hash

let begin_function t ~start ~entry_annotation =
  let fn = { unit_metadata = t; start; operations = [] } in
  record fn Exec start (hash_annotation t entry_annotation);
  fn

let record_event fn label (event : Event.t) =
  let t = fn.unit_metadata in
  match event with
  | Branch { taken; fallthrough } ->
    record fn Taken label (hash_annotation t taken);
    record fn Fallthrough label (hash_annotation t fallthrough)
  | Call { return_address; counter } ->
    let stack = hash_counter t counter in
    emit fn Taken label (Call { return_address; stack })
  | Tailcall counter -> emit fn Taken label (Tailcall (hash_counter t counter))
  | Jump { target; taken } ->
    record fn Taken label
      ~action:(fun stack -> Jump { target; stack })
      (hash_annotation t taken)
  | Reset -> emit fn Exec label Reset

let end_function fn ~finish =
  let t = fn.unit_metadata in
  if not (List.is_empty fn.operations)
  then
    t.pieces
      <- { start = fn.start; finish; operations = List.rev fn.operations }
         :: t.pieces

let record_dropped t dropped =
  t.dropped
    <- List.map
         (fun (counter, reason) -> reason, hash_counter t counter)
         (Fdo_dropped_counters.to_list dropped)

(* A compilation-unit fragment: "FDOM", version (ULEB), number of functions
   (ULEB); per function: absolute start (u64), byte length (ULEB), entry count
   (ULEB); per entry: offset from function start (ULEB), opcode byte, operands;
   body count (ULEB), then function hash (u32) and body hash (u32); name count
   (ULEB), then hash (u32) and length-prefixed canonical name (ULEB + bytes);
   dropped count (ULEB), then reason (byte, see
   [Fdo_dropped_counters.Reason.to_int]), stack length (ULEB) and u32 hashes,
   innermost first. Bodies cover every compiled instrumented function, so a
   profile can tell an unexecuted function from one whose body changed. The
   dropped counters are those the compiler has no instruction left for: they are
   only recorded with -fdo-dropped-counters, to assess the loss.

   Bits 0/1 select on_fallthrough/on_taken (neither is invalid). Bits 2..4
   encode Call=0, Tailcall=1, Normal=2, Reset=3, Jump=4. Bit 5 selects stack
   sharing; bits 6..7 are reserved. Reset has no operands or sharing bit. Call
   starts with its instruction length (ULEB), before the stack operands. The
   loader matches observed return instructions against call address + length;
   returns need no records. Jump (taken-only) counts only when the branch lands
   at its target, whose offset from the function start (ULEB) comes before the
   stack operands: the edges of an indirect jump through a jump table.

   Call, Tailcall, Normal and Jump specify the full within-function stack,
   innermost first. Uninstrumented calls have no annotation. Without sharing:
   length (ULEB), then u32 hashes. With sharing: drop (ULEB), length (ULEB),
   then u32 hashes prepended to the previous stack after dropping its innermost
   [drop] entries. The previous stack starts empty per function and changes only
   on counting actions, in METADATA order, not execution order. The loader
   expands this encoding before interpreting any samples.

   Labels in a piece are in the same text section, so all deltas are assembler
   constants even though the metadata is emitted in another section. *)
let version = 18

let section =
  Asm_targets.Asm_section.Custom
    { names = [".debug_fdo_metadata"];
      flags = Some "";
      args = ["@progbits"];
      is_delayed = false
    }

let emit_section t =
  match List.rev t.pieces with
  | [] when Hash.Tbl.length t.body_hashes = 0 && List.is_empty t.dropped -> ()
  | functions ->
    let buf = Buffer.create 4096 in
    let flush () =
      D.string (Buffer.contents buf);
      Buffer.clear buf
    in
    let rec uleb n =
      if n < 0x80
      then Buffer.add_char buf (Char.chr n)
      else (
        Buffer.add_char buf (Char.chr (n land 0x7f lor 0x80));
        uleb (n lsr 7))
    in
    let rec uleb_size n = if n < 0x80 then 1 else 1 + uleb_size (n lsr 7) in
    let hash n = Buffer.add_int32_le buf (Hash.to_int32 n) in
    let rec unshared old stack =
      match old, stack with
      | x :: xs, y :: ys when Hash.equal x y -> unshared xs ys
      | _ -> List.length old, List.rev stack
    in
    D.switch_to_section section;
    Buffer.add_string buf "FDOM";
    uleb version;
    uleb (List.length functions);
    List.iter
      (fun { start; finish; operations } ->
        flush ();
        D.label start;
        D.delta_uleb128 ~upper:finish ~lower:start;
        uleb (List.length operations);
        let previous = ref [] in
        List.iter
          (fun (label, kind, action) ->
            flush ();
            D.delta_uleb128 ~upper:label ~lower:start;
            let code, stack =
              match action with
              | Call { stack; _ } -> 0, Some stack
              | Tailcall stack -> 1, Some stack
              | Normal stack -> 2, Some stack
              | Reset -> 3, None
              | Jump { stack; _ } -> 4, Some stack
            in
            let flags =
              (match kind with Fallthrough -> 1 | Taken -> 2 | Exec -> 3)
              lor (code lsl 2)
            in
            match stack with
            | None -> Buffer.add_char buf (Char.chr flags)
            | Some stack ->
              let drop, push = unshared (List.rev !previous) (List.rev stack) in
              let n = List.length stack and m = List.length push in
              let shared =
                uleb_size drop + uleb_size m + (4 * m) < uleb_size n + (4 * n)
              in
              Buffer.add_char buf
                (Char.chr (flags lor if shared then 32 else 0));
              (match action with
              | Call { return_address; _ } ->
                flush ();
                D.delta_uleb128 ~upper:return_address ~lower:label
              | Jump { target; _ } ->
                flush ();
                D.delta_uleb128 ~upper:target ~lower:start
              | Tailcall _ | Normal _ | Reset -> ());
              if shared
              then (
                uleb drop;
                uleb m;
                List.iter hash push)
              else (
                uleb n;
                List.iter hash stack);
              previous := stack)
          operations)
      functions;
    let sorted table =
      Hash.Tbl.fold (fun key value acc -> (key, value) :: acc) table []
      |> List.sort (fun (a, _) (b, _) -> Hash.compare a b)
    in
    uleb (Hash.Tbl.length t.body_hashes);
    List.iter
      (fun (key, body_hash) ->
        hash key;
        Buffer.add_int32_le buf (Fdo_counter.Body_hash.to_int32 body_hash))
      (sorted t.body_hashes);
    uleb (Hash.Tbl.length t.names);
    List.iter
      (fun (key, name) ->
        hash key;
        uleb (String.length name);
        Buffer.add_string buf name)
      (sorted t.names);
    uleb (List.length t.dropped);
    List.iter
      (fun (reason, stack) ->
        Buffer.add_char buf
          (Char.chr (Fdo_dropped_counters.Reason.to_int reason));
        uleb (List.length stack);
        List.iter hash stack)
      t.dropped;
    flush ()
