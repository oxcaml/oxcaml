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
    | Conditional_branch of
        { taken : Fdo_annotation.t;
          fallthrough : Fdo_annotation.t
        }
    | Call of
        { return_address : L.t;
          counter : Fdo_counter.t
        }
    | Tail_call of Fdo_counter.t
    | Indirect_jump of
        { target : L.t;
          taken : Fdo_annotation.t
        }
    | Exception_handler
end

(* What a decoder does when the instruction at an entry's address executes. *)
module Action = struct
  type t =
    | Count_taken of Fdo_counter.Hashed.t
    | Count_fallthrough of Fdo_counter.Hashed.t
    | Count_executed of Fdo_counter.Hashed.t
    | Count_call of
        { return_address : L.t;
          counter : Fdo_counter.Hashed.t
        }
    | Count_tail_call of Fdo_counter.Hashed.t
    | Count_indirect_jump of
        { target : L.t;
          counter : Fdo_counter.Hashed.t
        }
    | Unwind_to_handler

  (* The flags byte of the format, without the sharing bit. *)
  let flags t =
    let on_taken = 2 and on_fallthrough = 1 in
    let code, on =
      match t with
      | Count_call _ -> 0, on_taken
      | Count_tail_call _ -> 1, on_taken
      | Count_taken _ -> 2, on_taken
      | Count_fallthrough _ -> 2, on_fallthrough
      | Count_executed _ -> 2, on_taken lor on_fallthrough
      | Unwind_to_handler -> 3, on_taken lor on_fallthrough
      | Count_indirect_jump _ -> 4, on_taken
    in
    (code lsl 2) lor on

  let counter = function
    | Count_taken counter
    | Count_fallthrough counter
    | Count_executed counter
    | Count_tail_call counter
    | Count_call { counter; _ }
    | Count_indirect_jump { counter; _ } ->
      Some counter
    | Unwind_to_handler -> None
end

module Entry = struct
  type t =
    { address : L.t;
      action : Action.t
    }
end

module Function = struct
  type t =
    { start : L.t;
      finish : L.t;
      entries : Entry.t list
    }
end

type t =
  { record_names : bool;
    mutable functions : Function.t list;  (** most recent first *)
    names : string Hash.Tbl.t;  (** empty without -fdo-names *)
    body_hashes : Fdo_counter.Body_hash.t Hash.Tbl.t;
        (** of every compiled instrumented function, executed or not, so a
            profile can tell an unexecuted function from one whose body changed
        *)
    mutable dropped :
      (Fdo_dropped_counters.Reason.t * Fdo_counter.Hashed.t) list
        (** empty without -fdo-dropped-counters *)
  }

type function_metadata =
  { unit_metadata : t;
    start : L.t;
    mutable rev_entries : Entry.t list
  }

let create ~record_names =
  { record_names;
    functions = [];
    names = Hash.Tbl.create 64;
    body_hashes = Hash.Tbl.create 64;
    dropped = []
  }

(* CR-someday yusumez: hashes are 32 bits, so collisions are possible. They are
   rare enough that both tables just keep the first binding. We should check
   whether we care. *)
let add_first table hash value =
  if not (Hash.Tbl.mem table hash) then Hash.Tbl.add table hash value

let hash_counter t counter =
  if t.record_names
  then
    List.iter
      (fun position ->
        add_first t.names
          (Fdo_counter.Position.hash position)
          (Fdo_counter.Position.to_string position))
      (Fdo_counter.position counter :: Fdo_counter.inlining_stack counter);
  Fdo_counter.hash counter

let hash_annotation t annotation =
  List.map (hash_counter t) (Fdo_annotation.to_list annotation)

let add_entry fn address action =
  fn.rev_entries <- { Entry.address; action } :: fn.rev_entries

let add_counters fn address (action : Fdo_counter.Hashed.t -> Action.t) counters
    =
  (* Sort outermost position first to group counters with shared inlining
     contexts. *)
  List.map List.rev counters
  |> List.sort_uniq (List.compare Hash.compare)
  |> List.iter (function
    | [] -> ()
    | rev_counter -> add_entry fn address (action (List.rev rev_counter)))

let record_body t ~function_ ~body_hash =
  add_first t.body_hashes
    (Fdo_counter.Function.entry_position_hash function_)
    body_hash

let begin_function t ~start ~entry_annotation =
  let fn = { unit_metadata = t; start; rev_entries = [] } in
  add_counters fn start
    (fun counter -> Count_executed counter)
    (hash_annotation t entry_annotation);
  fn

let record_event fn address (event : Event.t) =
  let t = fn.unit_metadata in
  match event with
  | Conditional_branch { taken; fallthrough } ->
    add_counters fn address
      (fun counter -> Count_taken counter)
      (hash_annotation t taken);
    add_counters fn address
      (fun counter -> Count_fallthrough counter)
      (hash_annotation t fallthrough)
  | Call { return_address; counter } ->
    add_entry fn address
      (Count_call { return_address; counter = hash_counter t counter })
  | Tail_call counter ->
    add_entry fn address (Count_tail_call (hash_counter t counter))
  | Indirect_jump { target; taken } ->
    add_counters fn address
      (fun counter -> Count_indirect_jump { target; counter })
      (hash_annotation t taken)
  | Exception_handler -> add_entry fn address Unwind_to_handler

let end_function fn ~finish =
  let t = fn.unit_metadata in
  if not (List.is_empty fn.rev_entries)
  then
    t.functions
      <- { Function.start = fn.start;
           finish;
           entries = List.rev fn.rev_entries
         }
         :: t.functions

let set_dropped t dropped =
  t.dropped
    <- List.map
         (fun (counter, reason) -> reason, hash_counter t counter)
         (Fdo_dropped_counters.to_list dropped)

(* CR-someday yusumez: consider emitting this metadata as an FDO-only DWARF
   compilation unit in .debug_info instead, with vendor tags and attributes, the
   innermost position of each stack inline and the rest as references to a trie
   of inlining contexts. Standard DWARF tools could then read it, and new
   attributes would not break older decoders. *)

(*= A fragment of the .debug_fdo_metadata section, for one compilation unit
    (the linker concatenates them):

    fragment:
      "FDOM"            4 bytes
      length            u64    bytes after this field, to skip the fragment
      version           uleb
      functions         uleb n, then n x function
      bodies            uleb n, then n x (function hash u32, body hash u32)
      names             uleb n, then n x (hash u32, .debug_str offset u32)
      dropped           uleb n, then n x (reason u8, counter)

    function:
      start             u64    absolute address
      length            uleb   bytes of code
      entries           uleb n, then n x entry   (not sorted by offset)

    entry:
      offset            uleb   from the function's start
      flags             u8
      operands                 by action, see below

    flags:
       7  6  5  4  3  2  1  0
      +-----+--+--------+-----+
      |0  0 |s | action |t  f |   t, f: on taken, on fallthrough
      +-----+--+--------+-----+   s: the counter is shared (see below)

      action                code  t f   operands
      Count_taken             2   1 0   counter
      Count_fallthrough       2   0 1   counter
      Count_executed          2   1 1   counter
      Count_call              0   1 0   call length (uleb), counter
      Count_tail_call         1   1 0   counter
      Count_indirect_jump     4   1 0   target offset (uleb), counter
      Unwind_to_handler       3   1 1   none (s = 0)

    counter, innermost position first:
      s = 0:            uleb n, then n x hash u32
      s = 1:            uleb drop, uleb n, then n x hash u32, put in front of
                        the previous counter without its [drop] innermost
                        hashes
*)

(* The previous counter starts empty in each function and tracks the last entry
   that had one (in metadata order, not execution order). A call's return
   address is its address plus its length, so returns need no entries. An
   indirect jump counts only when it lands at its target. An exception handler
   unwinds the decoder's call stack to the nearest frame of its function (or
   clears it if none); under recursion, that may not be the frame that caught
   the exception. All labels of a function live in one text section, so offsets
   are assembler constants. *)
let version = 19

let section =
  Asm_targets.Asm_section.Custom
    { names = [".debug_fdo_metadata"];
      flags = Some "";
      args = ["@progbits"];
      is_delayed = false
    }

(* Bytes are buffered, and flushed as a string directive before any other
   directive. *)
module Writer = struct
  type t = Buffer.t

  let create () = Buffer.create 4096

  let flush t =
    if Buffer.length t > 0
    then (
      D.string (Buffer.contents t);
      Buffer.clear t)

  let byte t n = Buffer.add_char t (Char.chr n)

  (* Unsigned Little Endian Base 128. Split into 7-bit chunks and fill the 8th
     bit with whether there are more bytes ahead. *)
  let rec uleb t n =
    if n < 0x80
    then byte t n
    else (
      byte t (n land 0x7f lor 0x80);
      uleb t (n lsr 7))

  let int32 t n = Buffer.add_int32_le t n

  let hash t hash = int32 t (Hash.to_int32 hash)

  let hashes t hashes =
    uleb t (List.length hashes);
    List.iter (hash t) hashes

  let raw_string t s = Buffer.add_string t s

  let directive t emit =
    flush t;
    emit ()

  let label t label = directive t (fun () -> D.label label)

  let delta t ~upper ~lower =
    directive t (fun () -> D.delta_uleb128 ~upper ~lower)
end

let rec uleb_size n = if n < 0x80 then 1 else 1 + uleb_size (n lsr 7)

(* Returns [(drop, push)]: how many innermost hashes of [previous] to drop and
   the hashes of [counter] (innermost first) to prepend in their place. *)
let unshared ~previous counter =
  let rec drop_common previous counter =
    match previous, counter with
    | x :: previous, y :: counter when Hash.equal x y ->
      drop_common previous counter
    | _ -> List.length previous, List.rev counter
  in
  drop_common (List.rev previous) (List.rev counter)

(* Returns the counter the next entry is encoded relative to. *)
let encode_entry w ~start ~previous ({ address; action } : Entry.t) =
  Writer.delta w ~upper:address ~lower:start;
  let flags = Action.flags action in
  match Action.counter action with
  | None ->
    Writer.byte w flags;
    previous
  | Some counter ->
    let drop, push = unshared ~previous counter in
    let n = List.length counter and m = List.length push in
    let shared =
      uleb_size drop + uleb_size m + (4 * m) < uleb_size n + (4 * n)
    in
    Writer.byte w (flags lor if shared then 32 else 0);
    (match action with
    | Count_call { return_address; _ } ->
      Writer.delta w ~upper:return_address ~lower:address
    | Count_indirect_jump { target; _ } ->
      Writer.delta w ~upper:target ~lower:start
    | Count_taken _ | Count_fallthrough _ | Count_executed _ | Count_tail_call _
    | Unwind_to_handler ->
      ());
    if shared
    then (
      Writer.uleb w drop;
      Writer.hashes w push)
    else Writer.hashes w counter;
    counter

let encode_function w ({ start; finish; entries } : Function.t) =
  Writer.label w start;
  Writer.delta w ~upper:finish ~lower:start;
  Writer.uleb w (List.length entries);
  let (_ : Fdo_counter.Hashed.t) =
    List.fold_left
      (fun previous entry -> encode_entry w ~start ~previous entry)
      [] entries
  in
  ()

let sorted_bindings table =
  Hash.Tbl.fold (fun key value acc -> (key, value) :: acc) table []
  |> List.sort (fun (a, _) (b, _) -> Hash.compare a b)

let encode_tables w t =
  Writer.uleb w (Hash.Tbl.length t.body_hashes);
  List.iter
    (fun (hash, body_hash) ->
      Writer.hash w hash;
      Writer.int32 w (Fdo_counter.Body_hash.to_int32 body_hash))
    (sorted_bindings t.body_hashes);
  Writer.uleb w (Hash.Tbl.length t.names);
  List.iter
    (fun (hash, name) ->
      Writer.hash w hash;
      Writer.directive w (fun () ->
          D.offset_into_dwarf_section_label ~width:Thirty_two Debug_str
            (D.cache_string (DWARF Debug_str) name)))
    (sorted_bindings t.names);
  Writer.uleb w (List.length t.dropped);
  List.iter
    (fun (reason, counter) ->
      Writer.byte w (Fdo_dropped_counters.Reason.to_int reason);
      Writer.hashes w counter)
    t.dropped

let emit_section t =
  if
    not
      (List.is_empty t.functions
      && Hash.Tbl.length t.body_hashes = 0
      && List.is_empty t.dropped)
  then (
    let w = Writer.create () in
    let functions = List.rev t.functions in
    let after_length = L.create section in
    let fragment_end = L.create section in
    D.switch_to_section section;
    Writer.raw_string w "FDOM";
    Writer.directive w (fun () ->
        D.between_labels_64_bit ~upper:fragment_end ~lower:after_length ();
        D.define_label after_length);
    Writer.uleb w version;
    Writer.uleb w (List.length functions);
    List.iter (encode_function w) functions;
    encode_tables w t;
    Writer.directive w (fun () -> D.define_label fragment_end);
    (* Emits the names, and forgets them: with -g, DWARF emission emits its own
       strings later. *)
    D.emit_cached_strings ())
