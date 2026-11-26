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
module Metadata_format = Fdo_metadata_format
module Counter_table = Metadata_format.Counter_table

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

module Entry = struct
  type t =
    { address : L.t;
      action : L.t Metadata_format.Action.t
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

let add_counters fn address
    (action : Fdo_counter.Hashed.t -> L.t Metadata_format.Action.t) counters =
  List.sort_uniq (List.compare Hash.compare) counters
  |> List.iter (fun counter -> add_entry fn address (action counter))

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

(* CR-someday yusumez: This section directive is ELF syntax, so the metadata
   does not assemble on macOS, which would need a Mach-O section. *)
let section =
  Asm_targets.Asm_section.Custom
    { names = [Metadata_format.section_name];
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

  let raw_string t s = Buffer.add_string t s

  let directive t emit =
    flush t;
    emit ()

  let label t label = directive t (fun () -> D.label label)

  let delta t ~upper ~lower =
    directive t (fun () -> D.delta_uleb128 ~upper ~lower)
end

(* All labels of a function live in one text section, so offsets are assembler
   constants. *)
let encode_entry w table ~start ({ address; action } : Entry.t) =
  Writer.delta w ~upper:address ~lower:start;
  Writer.byte w
    (Metadata_format.Kind.to_flags (Metadata_format.Kind.of_action action));
  Option.iter
    (fun (operand : L.t Metadata_format.Operand.t) ->
      match operand with
      | Return_address return_address ->
        Writer.delta w ~upper:return_address ~lower:address
      | Target target -> Writer.delta w ~upper:target ~lower:start)
    (Metadata_format.Action.operand action);
  Option.iter
    (fun counter -> Writer.uleb w (Counter_table.Builder.add table counter))
    (Metadata_format.Action.counter action)

let encode_function w table ({ start; finish; entries } : Function.t) =
  Writer.label w start;
  Writer.delta w ~upper:finish ~lower:start;
  Writer.uleb w (List.length entries);
  List.iter (encode_entry w table ~start) entries

(* The table of every counter of the fragment, which precedes the functions and
   the dropped counters that refer to it: encoding them only finds their
   indexes. *)
let counter_table t functions =
  let table = Counter_table.Builder.create () in
  let add counter =
    let (_ : int) = Counter_table.Builder.add table counter in
    ()
  in
  List.iter
    (fun ({ entries; _ } : Function.t) ->
      List.iter
        (fun ({ action; _ } : Entry.t) ->
          Option.iter add (Metadata_format.Action.counter action))
        entries)
    functions;
  List.iter (fun (_, counter) -> add counter) t.dropped;
  table

let encode_counter_table w table =
  let nodes = Counter_table.Builder.nodes table in
  Writer.uleb w (List.length nodes);
  List.iter
    (fun ({ hash; context } : Counter_table.Node.t) ->
      Writer.hash w hash;
      Writer.uleb w (match context with None -> 0 | Some index -> index + 1))
    nodes

let sorted_bindings table =
  Hash.Tbl.fold (fun key value acc -> (key, value) :: acc) table []
  |> List.sort (fun (a, _) (b, _) -> Hash.compare a b)

let encode_tables w table t =
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
      Writer.uleb w (Counter_table.Builder.add table counter))
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
    let table = counter_table t functions in
    let after_length = L.create section in
    let fragment_end = L.create section in
    D.switch_to_section section;
    Writer.raw_string w Metadata_format.magic;
    Writer.directive w (fun () ->
        D.between_labels_64_bit ~upper:fragment_end ~lower:after_length ();
        D.define_label after_length);
    Writer.uleb w Metadata_format.version;
    encode_counter_table w table;
    Writer.uleb w (List.length functions);
    List.iter (encode_function w table) functions;
    encode_tables w table t;
    Writer.directive w (fun () -> D.define_label fragment_end);
    (* Emits the names, and forgets them: with -g, DWARF emission emits its own
       strings later. *)
    D.emit_cached_strings ())
