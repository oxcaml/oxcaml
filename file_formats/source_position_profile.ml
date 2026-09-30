(******************************************************************************
 *                                  OxCaml                                    *
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

exception Error of string

module Hash = Fdo_counter.Hash

let magic_number = "oxcaml-source-position-profile\013"

(* -------------------------------------------------------------------------- *)
(* Counters. *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* The on-disk format (version 13: 32-bit hashes, bit 0 marks function entries). *)
(* *)
(* All integers are little-endian; "u32" is an unsigned 32-bit count, "i64" a *)
(* signed 64-bit count and "u64" an unsigned 64-bit absolute file offset. *)
(* *)
(*   header: *)
(*     magic number (its last byte is the format version) *)
(*     root index offset: u64 *)
(*     call-target index offset: u64 *)
(*     body index offset: u64 *)
(*   nodes, each child before its parent: *)
(*     count of the paths ending at the node (recorded with no further *)
(*       context): i64 *)
(*     count of the paths continuing into its children: i64 *)
(*     number of children: u32 *)
(*     child entries, sorted by unsigned hash: u32 hash, u64 node offset *)
(*   root index: u32 entry count, then root entries like child entries, *)
(*     sorted by unsigned hash *)
(*   call-target index: like the root index; each root is a call site level *)
(*     whose children are the entry levels of the functions calls from it *)
(*     reached. It is an index only: the node counts are 0, the counts are *)
(*     in the trie *)
(*   body index, last so its end doubles as a trailing-bytes check: *)
(*     u32 entry count, then sorted entries of u32 function hash and u32 *)
(*     body hash from the profiled build *)
(* *)
(* The point of this layout is that a query can descend the trie by reading *)
(* only the entry arrays it searches: nothing needs parsing up front, so a *)
(* profile can be memory-mapped and most of it never touched. Reads are *)
(* bounds-checked and validated lazily, as they happen; [load] eagerly *)
(* validates only the header and the root index placement (which *)
(* includes rejecting trailing bytes). *)
(* -------------------------------------------------------------------------- *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* The raw bytes, memory-mapped or read into memory. [filename] is carried for
   error messages. *)
type raw =
  { filename : string;
    data : bigstring
  }

type t =
  { raw : raw;
    (* The offset of the first root-index entry and the number of entries, for
       the trie and for the call-target index. *)
    roots_pos : int;
    num_roots : int;
    call_roots_pos : int;
    num_call_roots : int;
    bodies_pos : int;
    num_bodies : int
  }

let body_entry_size = 8

let corrupted raw fmt =
  Printf.ksprintf
    (fun msg ->
      raise
        (Error
           (Printf.sprintf "Corrupted source-position profile %s: %s"
              raw.filename msg)))
    fmt

let data_length data = Bigarray.Array1.dim data

(* Every primitive read is bounds-checked, so no possible file contents can make
   a read escape the data (offsets read from the file are additionally
   range-checked in [get_offset], keeping the arithmetic here overflow-free). *)
let check_bounds raw pos len =
  if pos < 0 || len < 0 || pos > data_length raw.data - len
  then corrupted raw "unexpected end of profile data"

let get_u32 raw pos =
  check_bounds raw pos 4;
  let byte i = Char.code (Bigarray.Array1.unsafe_get raw.data (pos + i)) in
  byte 0 lor (byte 1 lsl 8) lor (byte 2 lsl 16) lor (byte 3 lsl 24)

let get_i64 raw pos =
  check_bounds raw pos 8;
  let byte i =
    Int64.of_int (Char.code (Bigarray.Array1.unsafe_get raw.data (pos + i)))
  in
  let word = ref 0L in
  for i = 7 downto 0 do
    word := Int64.logor (Int64.shift_left !word 8) (byte i)
  done;
  !word

let get_string raw pos len =
  check_bounds raw pos len;
  String.init len (fun i -> Bigarray.Array1.unsafe_get raw.data (pos + i))

let read_count raw pos =
  let count = get_i64 raw pos in
  if Int64.compare count 0L < 0 then corrupted raw "negative count";
  count

(* A file offset: an unsigned 64-bit word that must fall within the data. *)
let get_offset raw pos =
  let offset = get_i64 raw pos in
  if Int64.unsigned_compare offset (Int64.of_int (data_length raw.data)) > 0
  then corrupted raw "file offset %Lu out of bounds" offset;
  Int64.to_int offset

(* -------------------------------------------------------------------------- *)
(* Entry arrays and integer interpolation search. *)
(* -------------------------------------------------------------------------- *)

(* Root-index and child entries are 12 bytes: the unsigned 32-bit position hash,
   then the node's file offset. *)
let entry_hash raw ~base i =
  Fdo_counter.Hash.of_int32 (Int32.of_int (get_u32 raw (base + (12 * i))))

let entry_offset raw ~base i = get_offset raw (base + (12 * i) + 4)

(* Hash keys and entry counts are unsigned 32-bit values. The interpolation
   product therefore fits in an unsigned 64-bit word, but not necessarily a
   signed one. Clamp probes strictly inside the interval to ensure progress. *)
let find_entry raw ~base ~n (hash : Fdo_counter.Hash.t) =
  let value (hash : Fdo_counter.Hash.t) =
    Int64.logand (Int64.of_int32 (hash :> int32)) 0xffffffffL
  in
  let needle = value hash in
  let key i = entry_hash raw ~base i in
  let rec linear lo hi =
    if lo > hi
    then None
    else if Hash.equal hash (key lo)
    then Some lo
    else linear (lo + 1) hi
  in
  let rec search lo hi =
    if hi - lo < 8
    then linear lo hi
    else
      let lower = key lo in
      if Hash.compare hash lower <= 0
      then if Hash.equal hash lower then Some lo else None
      else
        let upper = key hi in
        if Hash.compare hash upper >= 0
        then if Hash.equal hash upper then Some hi else None
        else
          let offset =
            Int64.unsigned_div
              (Int64.mul
                 (Int64.of_int (hi - lo))
                 (Int64.sub needle (value lower)))
              (Int64.sub (value upper) (value lower))
            |> Int64.to_int
          in
          let mid = max (lo + 1) (min (hi - 1) (lo + offset)) in
          match Hash.compare hash (key mid) with
          | 0 -> Some mid
          | c when c < 0 -> search lo (mid - 1)
          | _ -> search (mid + 1) hi
  in
  search 0 (n - 1)

(* -------------------------------------------------------------------------- *)
(* Node handles. *)
(* -------------------------------------------------------------------------- *)

(* A node handle is the byte offset of the node in the raw data. *)
type node = int

let node_ending t (node : node) = read_count t.raw node

let node_continuing t (node : node) = read_count t.raw (node + 8)

let node_count t node = Int64.add (node_ending t node) (node_continuing t node)

(* The number of children and the offset of the first child entry. *)
let node_children t (node : node) = get_u32 t.raw (node + 16), node + 20

let find_root t hash =
  match find_entry t.raw ~base:t.roots_pos ~n:t.num_roots hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:t.roots_pos i)

let find_call_root t hash =
  match find_entry t.raw ~base:t.call_roots_pos ~n:t.num_call_roots hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:t.call_roots_pos i)

let find_child t node hash =
  let num_children, entries = node_children t node in
  match find_entry t.raw ~base:entries ~n:num_children hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:entries i)

(* -------------------------------------------------------------------------- *)
(* Loading. *)
(* -------------------------------------------------------------------------- *)

let check_magic raw =
  let mlen = String.length magic_number in
  let wrong_format () =
    raise (Error ("Not a source-position profile: " ^ raw.filename))
  in
  if data_length raw.data < mlen then wrong_format ();
  let s = get_string raw 0 mlen in
  if not (String.equal s magic_number)
  then
    if
      String.equal
        (String.sub s 0 (mlen - 1))
        (String.sub magic_number 0 (mlen - 1))
    then
      (* Same producer, different version byte. *)
      raise
        (Error
           (raw.filename ^ " is an incompatible source-position profile version"))
    else wrong_format ()

let of_data ~filename data =
  let raw = { filename; data } in
  check_magic raw;
  let pos = String.length magic_number in
  let root_index_offset = get_offset raw pos in
  let call_index_offset = get_offset raw (pos + 8) in
  let body_index_offset = get_offset raw (pos + 16) in
  let header_end = pos + 24 in
  if root_index_offset < header_end
  then corrupted raw "root index offset inside the header";
  let num_roots = get_u32 raw root_index_offset in
  let index_end = root_index_offset + 4 + (12 * num_roots) in
  if call_index_offset <> index_end
  then corrupted raw "call-target index does not follow the root index";
  let num_call_roots = get_u32 raw call_index_offset in
  let call_index_end = call_index_offset + 4 + (12 * num_call_roots) in
  if body_index_offset <> call_index_end
  then corrupted raw "body index does not follow the call-target index";
  let num_bodies = get_u32 raw body_index_offset in
  let body_index_end = body_index_offset + 4 + (body_entry_size * num_bodies) in
  if body_index_end < data_length data
  then corrupted raw "unexpected trailing bytes"
  else if body_index_end > data_length data
  then corrupted raw "unexpected end of profile data";
  { raw;
    roots_pos = root_index_offset + 4;
    num_roots;
    call_roots_pos = call_index_offset + 4;
    num_call_roots;
    bodies_pos = body_index_offset + 4;
    num_bodies
  }

(* Memory-mapping needs [Unix], which this library cannot link; the native
   driver registers a mapper built on it. *)
let mmap : (string -> bigstring) option ref = ref None

let register_mmap f = mmap := Some f

let read_file filename =
  In_channel.with_open_bin filename (fun ic ->
      let length = Int64.to_int (In_channel.length ic) in
      let data =
        Bigarray.Array1.create Bigarray.char Bigarray.c_layout length
      in
      match In_channel.really_input_bigarray ic data 0 length with
      | Some () -> data
      | None ->
        raise
          (Error
             ("Corrupted source-position profile " ^ filename
            ^ ": unexpected end of profile data")))

let map_or_read filename =
  match !mmap with
  | None -> read_file filename
  | Some map_file -> (
    match map_file filename with
    | data -> data
    | exception _ ->
      (* Mapping can legitimately fail (empty file, exotic filesystem); reading
         then either succeeds or reports a proper error. *)
      read_file filename)

let load ~filename =
  match map_or_read filename with
  | data -> of_data ~filename data
  | exception Sys_error msg ->
    raise (Error ("Cannot open source-position profile: " ^ msg))

(* -------------------------------------------------------------------------- *)
(* Queries. *)
(* -------------------------------------------------------------------------- *)

(* A counter the profile did not record counts as 0, whether the profiled
   program never executed it or the profile could not have recorded it (e.g.
   depth truncation): the two are not distinguished for now. *)
type bound =
  { lower : float;
    upper : float;
    estimate : float option
  }

(* Body index lookups, needed by [count] to tell whether a position the profile
   lacks existed in the profiled build. *)
let body_hash t i =
  Fdo_counter.Hash.of_int32
    (Int32.of_int (get_u32 t.raw (t.bodies_pos + (body_entry_size * i))))

let body_hash_of_body t i =
  Fdo_counter.Function_body_hash.of_int32
    (Int32.of_int (get_u32 t.raw (t.bodies_pos + (body_entry_size * i) + 4)))

let find_body t function_id =
  let hash = Fdo_counter.hash_function_id function_id in
  let rec search lo hi =
    if lo > hi
    then None
    else
      let mid = lo + ((hi - lo) / 2) in
      let c = Hash.compare hash (body_hash t mid) in
      if c = 0
      then Some (body_hash_of_body t mid)
      else if c < 0
      then search lo (mid - 1)
      else search (mid + 1) hi
  in
  search 0 (t.num_bodies - 1)

(* Whether the profiled build had the position: its function was compiled, with
   the body it was numbered in for interior positions (entries do not depend on
   the body). Module initializers have no body record. *)
let position_existed t (position : Fdo_counter.position) =
  match position with
  | Position { function_id; function_body_hash; _ } -> (
    match find_body t function_id with
    | Some body_hash ->
      Fdo_counter.Function_body_hash.equal body_hash function_body_hash
    | None -> false)
  | Function_entry function_id -> Option.is_some (find_body t function_id)
  | Instantiation_site _ -> false

let count t (counter : Fdo_counter.t) =
  let float = Int64.to_float in
  (* [lost]: the paths that ended at the nodes passed so far, recorded with less
     context than the counter has; they may or may not be its executions.
     [scale]: by how much they would multiply the counter's count if, at each of
     those nodes, they split among the contexts like the continuing paths. *)
  let rec descend node ~lost ~scale = function
    | [] ->
      let count = float (node_count t node) in
      let upper = count +. float lost in
      { lower = count;
        upper;
        estimate = Some (Float.min upper (count *. scale))
      }
    | position :: rest -> (
      let ending = node_ending t node and continuing = node_continuing t node in
      let lost = Int64.add lost ending in
      match find_child t node (Fdo_counter.hash_position position) with
      | Some child ->
        let scale =
          if Int64.equal continuing 0L
          then scale
          else scale *. float (Int64.add ending continuing) /. float continuing
        in
        descend child ~lost ~scale rest
      | None ->
        (* Paths continuing into other contexts are not the counter's if the
           profiled build had this level; otherwise they might all be. *)
        if position_existed t position
        then { lower = 0.; upper = float lost; estimate = Some 0. }
        else
          { lower = 0.;
            upper = float (Int64.add lost continuing);
            estimate = None
          })
  in
  match find_root t (Fdo_counter.hash_position counter.position) with
  | Some node -> descend node ~lost:0L ~scale:1. counter.inlining_stack
  | None ->
    if position_existed t counter.position
    then { lower = 0.; upper = 0.; estimate = Some 0. }
    else { lower = 0.; upper = infinity; estimate = None }

let recorded_count t (counter : Fdo_counter.t) =
  let rec descend node = function
    | [] -> node_count t node
    | position :: rest -> (
      match find_child t node (Fdo_counter.hash_position position) with
      | Some child -> descend child rest
      | None -> 0L)
  in
  match find_root t (Fdo_counter.hash_position counter.position) with
  | None -> 0L
  | Some node -> descend node counter.inlining_stack

let count_for_deepest_context t ~root ~context =
  let rec descend node = function
    | [] -> node_count t node
    | hash :: rest -> (
      match find_child t node hash with
      | Some child -> descend child rest
      | None -> node_count t node)
  in
  match find_root t root with None -> 0L | Some node -> descend node context

type body_status =
  | Unknown_function
  | Same_body
  | Changed_body

let body_status t ~function_id ~function_body_hash =
  match find_body t function_id with
  | None -> Unknown_function
  | Some body_hash ->
    if Fdo_counter.Function_body_hash.equal body_hash function_body_hash
    then Same_body
    else Changed_body

let call_targets t callsite =
  match find_call_root t (Fdo_counter.hash_position callsite) with
  | None -> []
  | Some node ->
    let num_children, entries = node_children t node in
    List.init num_children (fun i -> entry_hash t.raw ~base:entries i)

(* Full traversals double as the deep validation that loading no longer
   performs: they check that entries are strictly sorted (which also rules out
   duplicate siblings) and bound the depth (a corrupt offset graph could
   otherwise recurse forever; real profiles are depth-truncated by the
   producer). *)
let max_reasonable_depth = 1000

let check_sorted t prev (hash : Fdo_counter.Hash.t) =
  match prev with
  | Some prev when Hash.compare prev hash >= 0 ->
    corrupted t.raw "unsorted or duplicate sibling hash %08lx" (hash :> int32)
  | Some _ | None -> ()

let iter_forest t ~roots_pos ~num_roots ~f =
  let rec walk depth hash node =
    if depth > max_reasonable_depth then corrupted t.raw "unreasonable depth";
    f ~hash ~depth ~count:(node_count t node) ~ending:(node_ending t node);
    let num_children, entries = node_children t node in
    let prev = ref None in
    for i = 0 to num_children - 1 do
      let child_hash = entry_hash t.raw ~base:entries i in
      check_sorted t !prev child_hash;
      prev := Some child_hash;
      walk (depth + 1) child_hash (entry_offset t.raw ~base:entries i)
    done
  in
  let prev = ref None in
  for i = 0 to num_roots - 1 do
    let hash = entry_hash t.raw ~base:roots_pos i in
    check_sorted t !prev hash;
    prev := Some hash;
    walk 1 hash (entry_offset t.raw ~base:roots_pos i)
  done

let iter t ~f = iter_forest t ~roots_pos:t.roots_pos ~num_roots:t.num_roots ~f

let iter_bodies t ~f =
  let prev = ref None in
  for i = 0 to t.num_bodies - 1 do
    let hash = body_hash t i in
    check_sorted t !prev hash;
    prev := Some hash;
    f ~hash ~function_body_hash:(body_hash_of_body t i)
  done

let iter_call_targets t ~f =
  let callsite = ref (Fdo_counter.Hash.of_int32 0l) in
  iter_forest t ~roots_pos:t.call_roots_pos ~num_roots:t.num_call_roots
    ~f:(fun ~hash ~depth ~count:_ ~ending:_ ->
      match depth with
      | 1 -> callsite := hash
      | 2 -> f ~callsite:!callsite ~callee:hash
      | _ -> corrupted t.raw "call-target index deeper than two levels")

(* -------------------------------------------------------------------------- *)
(* Writer. *)
(* -------------------------------------------------------------------------- *)

module Writer = struct
  type wnode =
    { mutable ending : int64;
      mutable continuing : int64;
      next : wnode Hash.Tbl.t
    }

  type t =
    { forest : wnode Hash.Tbl.t;
      bodies : Fdo_counter.Function_body_hash.t Hash.Tbl.t
    }

  let create () =
    { forest = Hash.Tbl.create 1024; bodies = Hash.Tbl.create 256 }

  let add_body t ~hash ~function_body_hash =
    match Hash.Tbl.find_opt t.bodies hash with
    | Some old
      when not (Fdo_counter.Function_body_hash.equal old function_body_hash) ->
      invalid_arg "Source_position_profile.Writer.add_body: conflicting bodies"
    | Some _ | None -> Hash.Tbl.replace t.bodies hash function_body_hash

  let find_or_add_node table hash =
    match Hash.Tbl.find_opt table hash with
    | Some node -> node
    | None ->
      let node = { ending = 0L; continuing = 0L; next = Hash.Tbl.create 4 } in
      Hash.Tbl.add table hash node;
      node

  (* Add [count] to the node of every prefix of the leaf-first stack [hashes],
     creating the nodes as needed. *)
  let add_path table ~hashes ~count =
    let rec go table = function
      | [hash] ->
        let node = find_or_add_node table hash in
        node.ending <- Int64.add node.ending count
      | hash :: rest ->
        let node = find_or_add_node table hash in
        node.continuing <- Int64.add node.continuing count;
        go node.next rest
      | [] -> ()
    in
    go table hashes

  let add_hashed_stack t ~hashes ~count = add_path t.forest ~hashes ~count

  let add_counter t ~counter ~count =
    add_hashed_stack t ~hashes:(Fdo_counter.hash counter) ~count

  (* Invert the first level of function-entry tries. This includes inlined calls
     as well as explicit calls; branch-counter roots are not callees. *)
  let call_target_index t =
    let index = Hash.Tbl.create 256 in
    Hash.Tbl.iter
      (fun callee node ->
        if Fdo_counter.Hash.is_function_entry callee
        then
          Hash.Tbl.iter
            (fun callsite _ ->
              add_path index ~hashes:[callsite; callee] ~count:0L)
            node.next)
      t.forest;
    index

  (* Sorted by unsigned hash, as the entry arrays of the format require. *)
  let sorted_entries table =
    let entries = Hash.Tbl.fold (fun k v acc -> (k, v) :: acc) table [] in
    List.sort (fun (k1, _) (k2, _) -> Hash.compare k1 k2) entries

  let add_u32 buf n = Buffer.add_int32_le buf (Int32.of_int n)

  let add_i64 buf n = Buffer.add_int64_le buf n

  let add_offset buf n = Buffer.add_int64_le buf (Int64.of_int n)

  (* Emit a node's subtree into [body], children first so their offsets are
     known when the node's child entries are written, and return the node's
     absolute file offset ([body] starts at file offset [base]). *)
  let rec emit_node body ~base (node : wnode) =
    let children =
      List.map
        (fun (hash, child) -> hash, emit_node body ~base child)
        (sorted_entries node.next)
    in
    let offset = base + Buffer.length body in
    add_i64 body node.ending;
    add_i64 body node.continuing;
    add_u32 body (List.length children);
    List.iter
      (fun ((hash : Fdo_counter.Hash.t), child_offset) ->
        Buffer.add_int32_le body (hash :> int32);
        add_offset body child_offset)
      children;
    offset

  let serialize t =
    let header_size =
      String.length magic_number + 8 (* root index offset *) + 8
      (* call-target index offset *) + 8 (* body index offset *)
    in
    let body = Buffer.create 65536 in
    let emit_forest forest =
      List.map
        (fun (hash, node) -> hash, emit_node body ~base:header_size node)
        (sorted_entries forest)
    in
    let emit_index roots =
      let offset = header_size + Buffer.length body in
      add_u32 body (List.length roots);
      List.iter
        (fun ((hash : Fdo_counter.Hash.t), offset) ->
          Buffer.add_int32_le body (hash :> int32);
          add_offset body offset)
        roots;
      offset
    in
    let roots = emit_forest t.forest in
    let call_roots = emit_forest (call_target_index t) in
    let root_index_offset = emit_index roots in
    let call_index_offset = emit_index call_roots in
    let body_index_offset = header_size + Buffer.length body in
    let bodies = sorted_entries t.bodies in
    add_u32 body (List.length bodies);
    List.iter
      (fun ( (hash : Fdo_counter.Hash.t),
             (body_hash : Fdo_counter.Function_body_hash.t) ) ->
        Buffer.add_int32_le body (hash :> int32);
        Buffer.add_int32_le body (body_hash :> int32))
      bodies;
    let header = Buffer.create header_size in
    Buffer.add_string header magic_number;
    add_offset header root_index_offset;
    add_offset header call_index_offset;
    add_offset header body_index_offset;
    assert (Buffer.length header = header_size);
    Buffer.contents header ^ Buffer.contents body

  let write t ~filename =
    let contents = serialize t in
    Out_channel.with_open_bin filename (fun oc ->
        Out_channel.output_string oc contents)

  let to_profile t =
    let contents = serialize t in
    of_data ~filename:"<in-memory profile>"
      (Bigarray.Array1.init Bigarray.char Bigarray.c_layout
         (String.length contents) (String.get contents))
end

(* -------------------------------------------------------------------------- *)
(* Error reporting. *)
(* -------------------------------------------------------------------------- *)

let () =
  Location.register_error_of_exn (function
    | Error msg ->
      Some (Location.error_of_printer_file Format_doc.pp_print_text msg)
    | _ -> None)
