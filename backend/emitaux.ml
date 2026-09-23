(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(* Common functions for emitting assembly code *)

open! Int_replace_polymorphic_compare

type error =
  | Stack_frame_too_large of int
  | Stack_frame_way_too_large of int
  | Inconsistent_probe_init of string * Debuginfo.t

exception Error of error

let output_channel = ref stdout

let output_prefix = ref ""

let emit_string s = output_string !output_channel s

let emit_buffer b = Buffer.output_buffer !output_channel b

(* Record live pointers at call points *)

type frame_debuginfo =
  | Dbg_alloc of Cmm.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

type frame_descr =
  { fd_lbl : Label.t; (* Return address *)
    fd_frame_size : int; (* Size of stack frame *)
    fd_live_offset : int list; (* Offsets/regs of live addresses *)
    fd_debuginfo : frame_debuginfo; (* Location, if any *)
    fd_long : bool; (* Use 32 instead of 16 bit format. *)
    fd_section : int
        (* Identifies the text section the return address lives in. A "short"
           descriptor's delta is the difference between two return-address
           labels, which is only an assembly-time constant when both labels are
           in the same section; descriptors whose section differs from the
           previous one escape. *)
  }

let frame_descriptors = ref ([] : frame_descr list)

(* The epoch bumps at each text-section change. In the compact frame-descriptor
   format a return address is a delta from the previous descriptor's -- an
   assembly-time constant only when both lie in the same section -- so
   descriptors record the epoch to decide where delta chains must break. *)
let frame_section_epoch = ref 0

let current_code_section = ref ""

(* Encoded name of the first symbol of the current text section, to which the
   link-order frametable and trap-note pieces of that section are linked (see
   [Asm_section.Frametable_piece]). *)
let current_link_symbol_ref = ref ""

let enter_code_section name ~link_symbol =
  current_link_symbol_ref := link_symbol;
  if not (String.equal name !current_code_section)
  then (
    current_code_section := name;
    incr frame_section_epoch)

let current_link_symbol () = !current_link_symbol_ref

(* Set by a backend before [emit_frames] when the short descriptor format cannot
   be emitted in the current context (currently only MASM, which has no .uleb128
   directive). Every descriptor then escapes to the normal format. *)
let disable_short_descriptors = ref false

let is_none_dbg d = Debuginfo.Dbg.is_none (Debuginfo.get_dbg d)

let get_flags debuginfo =
  match debuginfo with
  | Dbg_other d | Dbg_raise d -> if is_none_dbg d then 0 else 1
  | Dbg_alloc dbgs ->
    if
      !Clflags.debug
      && List.exists (fun d -> not (is_none_dbg d.Cmm.alloc_dbg)) dbgs
    then 3
    else 2

let is_long n =
  assert (n >= 0);
  (* Long frames must fit in 32-bit integer and not truncated upon conversion
     from int on any target. *)
  if n > 0x3FFF_FFFF then raise (Error (Stack_frame_way_too_large n));
  n >= !Oxcaml_flags.long_frames_threshold

let is_long_stack_index n = is_long n

let record_frame_descr ~label ~frame_size ~live_offset debuginfo =
  assert (frame_size land 3 = 0);
  let fd_long =
    is_long (frame_size + get_flags debuginfo)
    (* The checks below are redundant (if they fail, then frame size check above
       should have failed), but they make the safety of [emit_frame] clear. *)
    || is_long (List.length live_offset)
    || List.exists is_long_stack_index live_offset
  in
  if fd_long && not !Oxcaml_flags.allow_long_frames
  then raise (Error (Stack_frame_too_large frame_size));
  frame_descriptors
    := { fd_lbl = label;
         fd_frame_size = frame_size;
         fd_live_offset = List.sort_uniq ( - ) live_offset;
         fd_debuginfo = debuginfo;
         fd_long;
         fd_section = !frame_section_epoch
       }
       :: !frame_descriptors

type emit_frame_actions =
  { efa_code_label : Label.t -> unit;
    efa_data_label : Label.t -> unit;
    efa_i8 : Numbers.Int8.t -> unit;
    efa_i16 : Numbers.Int16.t -> unit;
    efa_i32 : Int32.t -> unit;
    efa_u8 : Numbers.Uint8.t -> unit;
    efa_u16 : Numbers.Uint16.t -> unit;
    efa_u32 : Numbers.Uint32.t -> unit;
    efa_word : int -> unit;
    efa_align : int -> unit;
    efa_label_rel : Label.t -> int32 -> unit;
    efa_label_delta : Label.t -> Label.t -> unit;
    (* [efa_label_delta upper lower] emits the variable-width return-address
       delta of a "short" frame descriptor, as a ULEB128 constant. *)
    efa_def_label : Label.t -> unit
  }

(* Per-unit tables shared by the frame descriptors and the tail that follows
   them: a descriptor's debuginfo words reference the debuginfo, name and string
   records that [emit_frames_tail] emits once, at the end of the unit. *)

let filenames : (string, Label.t) Hashtbl.t = Hashtbl.create 7

let label_filename name =
  try Hashtbl.find filenames name
  with Not_found ->
    let lbl = Cmm.new_label () in
    Hashtbl.add filenames name lbl;
    lbl

let defnames :
    (string * string * (int * int * int) option, Label.t * Label.t) Hashtbl.t =
  Hashtbl.create 7

let label_defname filename defname loc =
  try snd (Hashtbl.find defnames (filename, defname, loc))
  with Not_found ->
    let file_lbl = label_filename filename in
    let def_lbl = Cmm.new_label () in
    Hashtbl.add defnames (filename, defname, loc) (file_lbl, def_lbl);
    def_lbl

(* defname strings, each emitted once into the mergeable string section and
   referenced from the name_info / name_and_loc_info struct. *)
let defstrings : (string, Label.t) Hashtbl.t = Hashtbl.create 7

let label_defstring defname =
  try Hashtbl.find defstrings defname
  with Not_found ->
    let lbl = Cmm.new_label () in
    Hashtbl.add defstrings defname lbl;
    lbl

module Label_table = Hashtbl.Make (struct
  type t = bool * Debuginfo.Dbg.t

  let equal ((rs1 : bool), dbg1) (rs2, dbg2) =
    Bool.equal rs1 rs2 && Debuginfo.Dbg.compare dbg1 dbg2 = 0

  let hash (rs, dbg) = Hashtbl.hash (rs, Debuginfo.Dbg.hash dbg)
end)

let debuginfos : Label.t Label_table.t = Label_table.create 7

let label_debuginfos rs dbg =
  let dbg = Debuginfo.get_dbg dbg in
  let key = rs, dbg in
  try Label_table.find debuginfos key
  with Not_found ->
    let lbl = Cmm.new_label () in
    Label_table.add debuginfos key lbl;
    lbl

(* Chain suffixes already emitted in this unit, keyed by their emitted content
   (per element: name-struct label and packed info word); the value is a label
   on the suffix's first element. A chain whose tail matches an emitted suffix
   ends with a 4-byte jump word to it instead of repeating the elements (see the
   jump-word comment in runtime/backtrace_nat.c). *)
let emitted_suffixes : ((Label.t * int64) list, Label.t) Hashtbl.t =
  Hashtbl.create 7

let reset_frame_tables () =
  Hashtbl.reset filenames;
  Hashtbl.reset defnames;
  Hashtbl.reset defstrings;
  Label_table.reset debuginfos;
  Hashtbl.reset emitted_suffixes

(* The emit functions below perform bounds checks for the corresponding ranges
   via the conversion functions that raise exceptions. [int32] does not have
   such a function so we perform the check manually here. *)
let emit_u8 a n = a.efa_u8 (Numbers.Uint8.of_nonnegative_int_exn n)

let emit_u16 a n = a.efa_u16 (Numbers.Uint16.of_nonnegative_int_exn n)

let emit_i32 a n =
  let min_i32 = Int64.neg (Int64.shift_left 1L 31) (* -0x8000_0000 *)
  and max_i32 = Int64.sub (Int64.shift_left 1L 31) 1L (* 0x7fff_ffff *)
  and n_64 = Int64.of_int n in
  if Int64.compare n_64 min_i32 < 0 || Int64.compare n_64 max_i32 > 0
  then
    Misc.fatal_errorf "attempting to emit signed 32-bit integer %d out of range"
      n
  else a.efa_i32 (Int32.of_int n)

let emit_u32 a n = a.efa_u32 (Numbers.Uint32.of_nonnegative_int_exn n)

(* Emit the debuginfo words. These are identical in the normal and short
   descriptor layouts: present iff the debug flag is set, one word per
   allocation for an alloc descriptor, otherwise one word. *)
let emit_debug_words a fd =
  let flags = get_flags fd.fd_debuginfo in
  match fd.fd_debuginfo with
  | _ when flags = 0 -> ()
  | Dbg_other dbg -> a.efa_label_rel (label_debuginfos false dbg) Int32.zero
  | Dbg_raise dbg -> a.efa_label_rel (label_debuginfos true dbg) Int32.zero
  | Dbg_alloc dbg ->
    if flags = 3
    then
      List.iter
        (fun Cmm.{ alloc_dbg; _ } ->
          if is_none_dbg alloc_dbg
          then emit_i32 a 0
          else a.efa_label_rel (label_debuginfos false alloc_dbg) Int32.zero)
        dbg

(* Emit one descriptor in the existing normal/long format, with no leading delta
   byte. Used as the body following a 0 (escape) delta byte. *)
let emit_escaped_frame a fd =
  let flags = get_flags fd.fd_debuginfo in
  a.efa_label_rel fd.fd_lbl 0l;
  (* For short format, the size is guaranteed to be less than the constant
     below. *)
  if fd.fd_long
  then (
    emit_u16 a Oxcaml_flags.max_long_frames_threshold;
    (* Keep frame_data at offset 8 *)
    emit_u16 a 0);
  let emit_unsigned_16_or_32 = if fd.fd_long then emit_u32 a else emit_u16 a in
  (* The live offsets are always unsigned. *)
  let emit_live_offset n = emit_unsigned_16_or_32 n in
  emit_unsigned_16_or_32 (fd.fd_frame_size + flags);
  emit_unsigned_16_or_32 (List.length fd.fd_live_offset);
  List.iter emit_live_offset fd.fd_live_offset;
  begin match fd.fd_debuginfo with
  | Dbg_alloc dbg ->
    assert (List.length dbg < 256);
    emit_u8 a (List.length dbg);
    List.iter
      (fun Cmm.{ alloc_words; _ } ->
        (* Possible allocations range between 2 and 257 *)
        assert (
          2 <= alloc_words
          && alloc_words - 1 <= Config.max_young_wosize
          && Config.max_young_wosize <= 256);
        emit_u8 a (alloc_words - 2))
      dbg
  | Dbg_other _ | Dbg_raise _ -> () (* no alloc lengths *)
  end;
  emit_debug_words a fd

(* Partition live offsets into live registers (low bit 1, value >>1 is the
   register number) and live stack-slot byte offsets (low bit 0). See
   [compute_live_offset] in the backends. *)
let partition_live_offset live =
  List.partition_map
    (fun n -> if n land 1 = 1 then Either.Left (n lsr 1) else Either.Right n)
    live

(* Register-number -> hot-bitmap-bit-index. This is the inverse of
   [caml_frame_hot_regs] in runtime/caml/frame_descriptors.h; the compiler and
   the runtime MUST agree exactly or the GC scans the wrong registers (silent
   heap corruption). *)
let hot_reg_bit reg = Array.find_index (fun r -> r = reg) Arch.frame_hot_regs

(* Compute the short-format encoding of a descriptor, or [None] if it must
   escape. Result: [(size_units, num_allocs, alloc_nibbles, reg_bitmap,
   slot_bitmap)], where [slot_bitmap] is the word-granular live-slot bitmap over
   the frame, [ceil (frame_words / 8)] bytes. *)
let short_encoding fd =
  let flags = get_flags fd.fd_debuginfo in
  let has_alloc = flags land 2 <> 0 in
  let size = fd.fd_frame_size in
  if fd.fd_long || size <= 0 || size > 63 * 16 || size land 15 <> 0
  then None
  else
    let frame_words = size / Arch.size_addr in
    let regs, slots = partition_live_offset fd.fd_live_offset in
    let word_slots =
      List.map
        (fun byte_ofs ->
          assert (byte_ofs land (Arch.size_addr - 1) = 0);
          let w = byte_ofs / Arch.size_addr in
          (* A live slot outside the frame (an incoming stack parameter) cannot
             be represented in the frame-sized bitmap. *)
          if w >= frame_words then None else Some w)
        slots
    in
    let bad_slot = List.exists Option.is_none word_slots in
    let word_slots = List.filter_map Fun.id word_slots in
    if bad_slot
    then None
    else if (not has_alloc) && not (Misc.Stdlib.List.is_empty regs)
    then None
    else
      let reg_bits =
        List.fold_left
          (fun acc reg ->
            match acc with
            | None -> None
            | Some bits -> (
              match hot_reg_bit reg with
              | None -> None
              | Some b -> Some (bits lor (1 lsl b))))
          (Some 0) regs
      in
      match reg_bits with
      | None -> None
      | Some reg_bitmap ->
        let num_allocs, alloc_nibbles =
          match fd.fd_debuginfo with
          | Dbg_alloc dbg ->
            let sizes =
              List.map (fun Cmm.{ alloc_words; _ } -> alloc_words - 2) dbg
            in
            List.length sizes, sizes
          | Dbg_other _ | Dbg_raise _ -> 0, []
        in
        if
          num_allocs > 255
          || List.exists (fun s -> s < 0 || s > 15) alloc_nibbles
        then None
        else
          let slot_bitmap = Array.make ((frame_words + 7) / 8) 0 in
          List.iter
            (fun w ->
              slot_bitmap.(w / 8) <- slot_bitmap.(w / 8) lor (1 lsl (w land 7)))
            word_slots;
          Some (size / 16, num_allocs, alloc_nibbles, reg_bitmap, slot_bitmap)

(* Emit a short descriptor body (after its leading delta byte/bytes). *)
let emit_short_body a fd
    (size_units, num_allocs, alloc_nibbles, reg_bitmap, slot_bitmap) =
  let flags = get_flags fd.fd_debuginfo in
  let has_alloc = flags land 2 <> 0 in
  (* [size_units >= 1]: the byte is never zero, distinguishing a short body from
     an escape byte. *)
  emit_u8 a ((size_units lsl 2) lor flags);
  if has_alloc
  then (
    (* reg_bitmap, num_allocs, alloc sizes *)
    emit_u8 a reg_bitmap;
    emit_u8 a num_allocs;
    let rec emit_nibbles = function
      | [] -> ()
      | [n] -> emit_u8 a (n land 0xf)
      | n :: m :: rest ->
        emit_u8 a (n land 0xf lor ((m land 0xf) lsl 4));
        emit_nibbles rest
    in
    emit_nibbles alloc_nibbles);
  (* Live stack slots: a word-granular bitmap over the frame. Its width derives
     from the size+flags byte, so no count byte is needed. *)
  Array.iter (emit_u8 a) slot_bitmap;
  emit_debug_words a fd

(* Emit one descriptor preceded by retaddr delta. The first descriptor of a
   frametable (or of a link-order piece), and any descriptor that does not fit
   the short format, escapes: a 0 delta byte followed by the existing
   normal/long descriptor (which carries its own relative return address). A
   short descriptor is preceded by its return-address delta from the previous
   descriptor, as ULEB128. *)
let emit_descr a prev fd =
  let escape () =
    emit_u8 a 0;
    emit_escaped_frame a fd;
    Some fd
  in
  if !disable_short_descriptors
  then escape ()
  else
    match prev with
    | Some prev_fd when prev_fd.fd_section = fd.fd_section -> (
      (* Same text section as the previous descriptor, so the delta is an
         assembly-time constant. *)
      match short_encoding fd with
      | Some enc ->
        a.efa_label_delta fd.fd_lbl prev_fd.fd_lbl;
        emit_short_body a fd enc;
        Some fd
      | None -> escape ())
    | Some _ | None ->
      (* First descriptor of the frametable, or first of a new text section (no
         same-section previous return address for a delta): escape. *)
      escape ()

(* Emit the pending descriptors into the current section, first one escaping,
   and clear them. Descriptors are recorded in increasing return-address order
   (calls inline as they are emitted; allocations and polls when their
   out-of-line GC stub is emitted), so the prepended list is in decreasing
   order. Reverse it to emit the frame table in increasing return-address
   order. *)
let emit_frames_for_function a =
  let descrs = List.rev !frame_descriptors in
  ignore (List.fold_left (emit_descr a) None descrs);
  frame_descriptors := []

(* No alignment here: a link-order piece is a byte-granular descriptor stream
   that is delimited only by decoding it, so zero fill would read as an
   escape. *)
let emit_frametable_piece ~link_symbol a =
  if not (Misc.Stdlib.List.is_empty !frame_descriptors)
  then (
    Asm_targets.Asm_directives.switch_to_section
      (Frametable_piece { link_symbol });
    emit_frames_for_function a)

let fully_pack_info fd_raise d has_next =
  (* See format in caml_debuginfo_location in runtime/backtrace-nat.c *)
  let open Debuginfo in
  let kind = if fd_raise then 1 else 0
  and has_next = if has_next then 1 else 0
  and char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
  let char_end_offset = d.dinfo_end_bol - d.dinfo_start_bol in
  Int64.(
    add
      (shift_left (of_int d.dinfo_line) 51)
      (add
         (shift_left (of_int (d.dinfo_end_line - d.dinfo_line)) 48)
         (add
            (shift_left (of_int d.dinfo_char_start) 42)
            (add
               (shift_left (of_int char_end) 35)
               (add
                  (shift_left (of_int char_end_offset) 26)
                  (add (shift_left (of_int kind) 1) (of_int has_next)))))))

let partially_pack_info fd_raise d has_next =
  (* Partially packed debuginfo: 1lllllllllmmmmmmmmddddddddddddkn 1 - d points
     to a name_and_loc_info struct l (19 bits) - start line number m (18 bits) -
     offset of end line number from start d (24 bits) - memory offset to
     name_and_loc_info struct k (1 bit) - fd_raise flag n (1 bit) - has_next
     flag *)
  let open Debuginfo in
  let start_line = Int.min 0x7FFFF d.dinfo_line
  and end_line = Int.min 0x3FFFF (d.dinfo_end_line - d.dinfo_line)
  and kind = if fd_raise then 1 else 0
  and has_next = if has_next then 1 else 0 in
  Int64.(
    add (shift_left Int64.one 63)
      (add
         (shift_left (of_int start_line) 44)
         (add
            (shift_left (of_int end_line) 26)
            (add (shift_left (of_int kind) 1) (of_int has_next)))))

(* Matches [Debuginfo_jump_bias] in runtime/backtrace_nat.c. *)
let debuginfo_jump_bias = 0x0300_0000l

let emit_debuginfo a (rs, dbg) lbl =
  let rdbg = dbg |> Debuginfo.Dbg.to_list |> List.rev in
  (* Due to inlined functions, a single debuginfo may have multiple locations.
     These are represented sequentially in memory (innermost frame first), with
     the low bit of the packed debuginfo being 0 on the last entry. *)
  let rec contents rs ds =
    match ds with
    | [] -> []
    | d :: rest ->
      let open Debuginfo in
      let defname =
        Scoped_location.string_of_scopes ~include_zero_alloc:false
          d.dinfo_scopes
      in
      let char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
      let is_fully_packable =
        d.dinfo_line <= 0xFFF
        && d.dinfo_end_line - d.dinfo_line <= 0x7
        && d.dinfo_char_start <= 0x3F && char_end <= 0x7F
        && d.dinfo_end_bol - d.dinfo_start_bol <= 0x1FF
      in
      let info =
        if is_fully_packable
        then fully_pack_info rs d (not (Misc.Stdlib.List.is_empty rest))
        else partially_pack_info rs d (not (Misc.Stdlib.List.is_empty rest))
      in
      let loc =
        if is_fully_packable
        then None
        else
          Some
            ( Int.min 0xFFFF d.dinfo_char_start,
              (* start_chr *)
              Int.min 0xFFFF char_end,
              (* end_chr *)
              Int.min 0x3FFFFFFF d.dinfo_char_end )
        (* end_offset *)
      in
      (label_defname d.dinfo_file defname loc, info) :: contents false rest
  in
  let elts = contents rs rdbg in
  assert (not (Misc.Stdlib.List.is_empty elts));
  a.efa_align 4;
  a.efa_def_label lbl;
  let rec emit_elts start_lbl elts =
    match elts with
    | [] -> ()
    | (name_lbl, info) :: rest -> (
      match Hashtbl.find_opt emitted_suffixes elts with
      | Some target ->
        (* The whole remaining suffix was already emitted: jump to it. *)
        a.efa_label_rel target debuginfo_jump_bias
      | None ->
        let start_lbl =
          match start_lbl with
          | Some l -> l
          | None ->
            let l = Cmm.new_label () in
            a.efa_def_label l;
            l
        in
        Hashtbl.add emitted_suffixes elts start_lbl;
        a.efa_label_rel name_lbl (Int64.to_int32 info);
        (* We use [efa_i32] directly here instead of [emit_i32] to avoid a
           round-trip via [int], which would break on 32-bit platforms. The
           right shift ensures that the integer is in range of [int32]. *)
        a.efa_i32 (Int64.to_int32 (Int64.shift_right info 32));
        emit_elts None rest)
  in
  emit_elts (Some lbl) elts

let emit_frames_tail ~debug_strings_section a =
  let module D = Asm_targets.Asm_directives in
  let module L = Asm_targets.Asm_label in
  let module Asm_section = Asm_targets.Asm_section in
  if not (Misc.Stdlib.List.is_empty !frame_descriptors)
  then
    Misc.fatal_error
      "Emitaux.emit_frames_tail: pending frame descriptors were never emitted";
  (* Debuginfo filename and defname strings go in [debug_strings_section]. On
     MacOS the assembler discards L<n> labels as temporaries; to make it into
     the symbol table we use l_caml<n> _private symbols_ instead (which the
     linker treats as local and strips). *)
  let macos_cstrings =
    Target_system.is_macos ()
    && Asm_section.equal debug_strings_section Asm_section.Debuginfo_strings
  in
  (* References carry the frametable section (the directive layer requires a
     referenced label's section to match the current one); definitions carry the
     strings section. The encoded name is the same either way. *)
  let string_label ~section lbl =
    if macos_cstrings
    then L.create_private_int section (Label.to_int lbl)
    else L.create_int section (Label.to_int lbl)
  in
  let string_label_rel lbl ofs =
    D.between_this_and_label_offset_32bit_expr
      ~upper:(string_label ~section:Asm_section.Read_only_data lbl)
      ~offset_upper:(Targetint.of_int32 ofs)
  in
  let emit_merged_string str lbl =
    D.define_label (string_label ~section:debug_strings_section lbl);
    D.string (str ^ "\000")
  in
  let emit_defname (_filename, defname, loc) (file_lbl, lbl) =
    let emit_loc (start_chr, end_chr, end_offset) =
      emit_u16 a start_chr;
      emit_u16 a end_chr;
      emit_i32 a end_offset
    in
    (* The name_info / name_and_loc_info struct. Must be 32-bit aligned, because
       the low 2 bits of its address are used for flags in debuginfo *)
    a.efa_align 4;
    a.efa_def_label lbl;
    string_label_rel file_lbl 0l;
    (* [defname_offs] relative to the start of the struct, so offset by 4 *)
    string_label_rel (label_defstring defname) 4l;
    (* Then the extra 64 bits of location info that didn't pack into the main
       debuginfo word (name_and_loc_info only). *)
    Option.iter emit_loc loc
  in
  Label_table.iter (emit_debuginfo a) debuginfos;
  (* The name structs are kept near the debuginfo words that reference them (a
     23-bit, 32 MB offset; bit 25 above it flags a suffix-sharing jump word).
     Emitting them also populates [defstrings]. *)
  Hashtbl.iter emit_defname defnames;
  a.efa_align Arch.size_addr;
  D.switch_to_section debug_strings_section;
  Hashtbl.iter emit_merged_string filenames;
  Hashtbl.iter emit_merged_string defstrings;
  D.switch_to_section Asm_section.Read_only_data;
  reset_frame_tables ()

let emit_frames ~debug_strings_section a =
  a.efa_word (List.length !frame_descriptors);
  emit_frames_for_function a;
  emit_frames_tail ~debug_strings_section a

let make_frame_actions ~type_labels : emit_frame_actions =
  let module D = Asm_targets.Asm_directives in
  let module L = Asm_targets.Asm_label in
  let asm_label ~section lbl = L.create_int section (Label.to_int lbl) in
  let type_label lbl ~(ty : D.symbol_type) =
    if type_labels then D.type_label lbl ~ty
  in
  { efa_code_label =
      (fun lbl ->
        let lbl = asm_label ~section:Text lbl in
        type_label lbl ~ty:Function;
        D.label lbl);
    efa_data_label =
      (fun lbl ->
        let lbl = asm_label ~section:Data lbl in
        type_label lbl ~ty:Object;
        D.label lbl);
    efa_i8 = (fun n -> D.int8 n);
    efa_i16 = (fun n -> D.int16 n);
    efa_i32 = (fun n -> D.int32 n);
    efa_u8 = (fun n -> D.uint8 n);
    efa_u16 = (fun n -> D.uint16 n);
    efa_u32 = (fun n -> D.uint32 n);
    efa_word = (fun n -> D.targetint (Targetint.of_int_exn n));
    efa_align = (fun n -> D.align ~fill:Zero ~bytes:n);
    efa_label_rel =
      (fun lbl ofs ->
        (* Descriptors may be emitted into a frametable piece rather than
           [Read_only_data], so the label takes the current section. *)
        let lbl = asm_label ~section:(D.current_section ()) lbl in
        D.between_this_and_label_offset_32bit_expr ~upper:lbl
          ~offset_upper:(Targetint.of_int32 ofs));
    efa_label_delta =
      (fun upper lower ->
        (* The return-address labels live in the text section. *)
        let upper = asm_label ~section:Text upper in
        let lower = asm_label ~section:Text lower in
        D.delta_uleb128 ~upper ~lower);
    efa_def_label =
      (fun lbl ->
        let lbl = asm_label ~section:(D.current_section ()) lbl in
        D.define_label lbl)
  }

(* Detection of functions that can be duplicated between a DLL and the main
   program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.equal (String.sub s2 0 (String.length s1)) s1

let is_generic_function name =
  List.exists
    (fun p -> isprefix p name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

(* CFI directives *)

let is_cfi_enabled () = Config.asm_cfi_supported

(* Emit debug information *)

(* This assoc list is expected to be very short *)
let file_pos_nums = (ref [] : (string * int) list ref)

(* Number of files *)
let file_pos_num_cnt = ref 1

(* Reset debug state at beginning of asm file *)
let reset_debug_info () =
  file_pos_nums := [];
  file_pos_num_cnt := 1

let with_snapshot ~f =
  let saved_file_pos_nums = !file_pos_nums in
  let saved_file_pos_num_cnt = !file_pos_num_cnt in
  let saved_frame_descriptors = !frame_descriptors in
  let saved_frame_section_epoch = !frame_section_epoch in
  let saved_current_code_section = !current_code_section in
  let saved_current_link_symbol = !current_link_symbol_ref in
  let result = f () in
  file_pos_nums := saved_file_pos_nums;
  file_pos_num_cnt := saved_file_pos_num_cnt;
  frame_descriptors := saved_frame_descriptors;
  frame_section_epoch := saved_frame_section_epoch;
  current_code_section := saved_current_code_section;
  current_link_symbol_ref := saved_current_link_symbol;
  result

let get_file_num ~file_emitter file_name =
  try List.assoc file_name !file_pos_nums
  with Not_found ->
    let file_num = !file_pos_num_cnt in
    incr file_pos_num_cnt;
    file_emitter ~file_num ~file_name;
    file_pos_nums := (file_name, file_num) :: !file_pos_nums;
    file_num

(* Some assemblers always build DWARF-5 line tables. The line table header
   contains a table of file names, and a ".file N" directive defines the entry
   at index N of that table; so a "file number" is nothing more than an index
   into the file name table. (".loc" directives, and attributes such as
   DW_AT_decl_file, identify files by these indexes.) In DWARF-5 the table is
   indexed from 0, rather than from 1 as in DWARF-4, and the entry at index 0 is
   defined to name the compilation unit's primary source file. If no ".file 0"
   directive is emitted, the assembler synthesizes the entry at index 0 by
   duplicating the one at index 1, which both records the wrong primary source
   file (the first file we register is the "none" placeholder; see
   [Asm_directives.debug_header]) and causes DWARF verifiers to warn about the
   duplicated entry. Emitting ".file 0" to name the real source file avoids both
   problems. *)
let register_primary_file ~file_emitter ~sourcefile =
  if Config.asm_file0_supported
  then (
    file_emitter ~file_num:0 ~file_name:sourcefile;
    file_pos_nums := (sourcefile, 0) :: !file_pos_nums)

(* We only display .file if the file has not been seen before. We display .loc
   for every instruction. *)
let emit_debug_info_gen ?discriminator dbg file_emitter loc_emitter =
  let dbg = Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg) in
  if is_cfi_enabled () && (!Clflags.debug || Config.with_frame_pointers)
  then
    match List.rev dbg with
    | [] -> ()
    | ({ Debuginfo.dinfo_line = line; dinfo_char_start = col; _ } as item) :: _
      ->
      if line > 0
      then
        (* PR#6243 *)
        let file_num =
          get_file_num ~file_emitter (Debuginfo.item_file_path item)
        in
        loc_emitter ~file_num ~line ~col ?discriminator ()

let binary_backend_available = ref false

let reduce_heap_size ~reset =
  let _minor, _promoted, major_words = Gc.counters () in
  (* Uses [major_words] because it doesn't require a heap traversal to compute
     and for this workload a majority of major words are live at this point. *)
  let heap_reduction_threshold =
    if !Oxcaml_flags.heap_reduction_threshold >= 0
    then float !Oxcaml_flags.heap_reduction_threshold
    else Float.infinity
  in
  if Float.compare major_words heap_reduction_threshold > 0
  then
    Profile.record_call "compact" (fun () ->
        reset ();
        Gc.compact ())

module Dwarf_helpers = struct
  let dwarf = ref None

  let sourcefile_for_dwarf = ref None

  let ppf_dump = ref Format.err_formatter

  let record_function_range ~function_symbol ~start_label ~end_label
      ~offset_past_end_label =
    Option.iter
      (fun d ->
        Dwarf.record_function_range d ~function_symbol ~start_label ~end_label
          ~offset_past_end_label)
      !dwarf

  let begin_dwarf ~code_begin ~code_end ~file_emitter =
    match !sourcefile_for_dwarf with
    | None -> ()
    | Some sourcefile ->
      let asm_directives =
        Asm_targets.Asm_directives_dwarf.build_asm_directives ()
      in
      let get_file_num = get_file_num ~file_emitter in
      register_primary_file ~file_emitter ~sourcefile;
      Asm_targets.Asm_directives.debug_header ~get_file_num;
      let unit_name =
        (* CR lmaurer: This doesn't actually need to be an [Ident.t] *)
        Current_unit.symbol () |> Symbol.linkage_name |> Linkage_name.to_string
        |> Ident.create_persistent
      in
      let code_begin = Asm_targets.Asm_symbol.create_global code_begin in
      let code_end = Asm_targets.Asm_symbol.create_global code_end in
      let code_layout : Dwarf_state.code_layout =
        if
          !Clflags.function_sections
          || !Oxcaml_flags.basic_block_sections
          || !Oxcaml_flags.module_entry_functions_section
        then
          (* Use Function_sections mode - ranges will be recorded via
             [record_function_range] as functions are emitted *)
          Dwarf_state.Function_sections
        else Dwarf_state.Continuous_code_section { code_begin; code_end }
      in
      dwarf
        := Some
             (Dwarf.create ~sourcefile ~unit_name ~asm_directives
                ~get_file_id:get_file_num ~code_layout)

  let reset_dwarf ppf =
    dwarf := None;
    sourcefile_for_dwarf := None;
    ppf_dump := ppf

  let init ~ppf_dump ~disable_dwarf ~sourcefile =
    reset_dwarf ppf_dump;
    let can_emit_dwarf = !Clflags.debug && not disable_dwarf in
    match
      ( can_emit_dwarf,
        Target_system.architecture (),
        Target_system.derived_system () )
    with
    | true, (X86_64 | AArch64), _ -> sourcefile_for_dwarf := sourcefile
    | true, (IA32 | ARM | POWER | Z | Riscv), _ | false, _, _ -> ()

  let emit_dwarf () =
    Option.iter
      (Dwarf.emit ~binary_backend_available:!binary_backend_available)
      !dwarf

  let emit_delayed_dwarf () =
    Option.iter
      (Dwarf.emit_delayed ~binary_backend_available:!binary_backend_available)
      !dwarf

  let record_dwarf_for_fundecl fundecl =
    match !dwarf with
    | None -> None
    | Some dwarf ->
      let fun_end_label = Cmm.new_label () in
      let ppf_dump = !ppf_dump in
      Some (Dwarf.dwarf_for_fundecl dwarf fundecl ~fun_end_label ~ppf_dump)
end

let report_error_doc ppf = function
  | Stack_frame_too_large n ->
    Format_doc.fprintf ppf
      "stack frame too large (%d bytes). \nUse -long-frames compiler flag." n
  | Stack_frame_way_too_large n ->
    Format_doc.fprintf ppf "stack frame too large (%d bytes)." n
  | Inconsistent_probe_init (name, dbg) ->
    Format_doc.fprintf ppf
      "Inconsistent use of ~enabled_at_init in [%%probe %s ..] at %a" name
      Debuginfo.doc_print_compact dbg

let report_error = Format_doc.compat report_error_doc

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error_doc err)
    | _ -> None)

type preproc_stack_check_result =
  { max_frame_size : int;
    contains_nontail_calls : bool
  }

let preproc_stack_check ~fun_body ~frame_size ~trap_size =
  let rec loop (i : Linear.instruction) fs max_fs nontail_flag =
    match i.desc with
    | Lend -> { max_frame_size = max_fs; contains_nontail_calls = nontail_flag }
    | Ladjust_stack_offset { delta_bytes } ->
      let s = fs + delta_bytes in
      loop i.next s (max s max_fs) nontail_flag
    | Lpushtrap _ ->
      let s = fs + trap_size in
      loop i.next s (max s max_fs) nontail_flag
    | Lpoptrap _ -> loop i.next (fs - trap_size) max_fs nontail_flag
    | Lop (Stackoffset n) ->
      let s = fs + n in
      loop i.next s (max s max_fs) nontail_flag
    | Lcall_op (Lcall_ind | Lcall_imm _) -> loop i.next fs max_fs true
    | Lprologue | Lepilogue_open | Lepilogue_close
    | Lop
        ( Move | Spill | Reload | Opaque | Begin_region | End_region | Dls_get
        | Tls_get | Domain_index | Poll | Pause | Const_int _ | Const_float32 _
        | Const_float _ | Const_symbol _ | Const_vec128 _ | Const_vec256 _
        | Const_vec512 _ | Const_mask _ | Load _
        | Store (_, _, _)
        | Intop _ | Int128op _
        | Intop_imm (_, _)
        | Intop_atomic _
        | Floatop (_, _)
        | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
        | Specific _ | Name_for_debugger _ | Alloc _ )
    | Lcall_op (Ltailcall_ind | Ltailcall_imm _ | Lextcall _ | Lprobe _)
    | Lreloadretaddr | Lreturn | Llabel _ | Lbranch _ | Lcondbranch _
    | Lcondbranch3 _ | Lswitch _ | Lentertrap | Lraise _ ->
      loop i.next fs max_fs nontail_flag
    | Lstackcheck _ ->
      (* should not be already present *)
      Misc.fatal_error
        "Emitaux.preproc_stack_check: Lstackcheck already present"
  in
  loop fun_body frame_size frame_size false

let add_stack_checks_if_needed (fundecl : Linear.fundecl) ~stack_offset
    ~stack_threshold_size ~trap_size =
  if Config.no_stack_checks
  then fundecl
  else
    let frame_size =
      Proc.frame_size ~stack_offset ~num_stack_slots:fundecl.fun_num_stack_slots
        ~contains_calls:fundecl.fun_contains_calls
    in
    let { max_frame_size; contains_nontail_calls } =
      preproc_stack_check ~fun_body:fundecl.fun_body ~frame_size ~trap_size
    in
    let insert_stack_check =
      contains_nontail_calls || max_frame_size >= stack_threshold_size
    in
    if insert_stack_check
    then
      let fun_body =
        (* CR mshinwell: These availability sets aren't taking into account any
           potential clobbers by the stack check. *)
        Linear.instr_cons
          (Lstackcheck { max_frame_size_bytes = max_frame_size })
          [||] [||] ~available_before:fundecl.fun_body.available_before
          ~available_across:fundecl.fun_body.available_across fundecl.fun_body
          ~phantom_available_before:fundecl.fun_body.phantom_available_before
      in
      { fundecl with fun_body }
    else fundecl

let stapsdt_base_emitted = ref false

let emit_stapsdt_base_section () =
  let module D = Asm_targets.Asm_directives in
  let module S = Asm_targets.Asm_symbol in
  if not !stapsdt_base_emitted
  then (
    stapsdt_base_emitted := true;
    D.switch_to_section Stapsdt_base;
    (* Note that the Stapsdt symbols do not follow the usual symbol encoding
       convention. Hence, in this rare case, we create the symbol as a raw
       symbol for which no subsequent encoding will be done.*)
    let stapsdt_sym = S.Predef.stapsdt_base in
    if not (Target_system.is_macos ())
    then (
      D.weak stapsdt_sym;
      D.hidden stapsdt_sym);
    D.define_symbol_label ~section:Stapsdt_base stapsdt_sym;
    D.space ~bytes:1;
    D.size_const stapsdt_sym
      1L (* 1 byte; alternative would be . - _.stapsdt.base *))

let emit_elf_note ~section ~owner ~typ ~emit_desc =
  let module D = Asm_targets.Asm_directives in
  let module L = Asm_targets.Asm_label in
  let bytes = if Target_system.is_macos () then 8 else 4 in
  D.align ~fill:Zero ~bytes;
  let a = L.create section in
  let b = L.create section in
  let c = L.create section in
  let d = L.create section in
  D.between_labels_32_bit ~upper:b ~lower:a ();
  D.between_labels_32_bit ~upper:d ~lower:c ();
  D.int32 typ;
  D.define_label a;
  D.string (owner ^ "\000");
  D.define_label b;
  D.align ~fill:Zero ~bytes;
  D.define_label c;
  emit_desc ();
  D.define_label d;
  D.align ~fill:Zero ~bytes

type emit_data_item_actions =
  { global_maybe_protected : Asm_targets.Asm_symbol.t -> unit;
    symbol_defined : string -> unit;
    symbol_used : string -> unit
  }

let symbol_of_cmm_symbol (s : Cmm.symbol) : Asm_targets.Asm_symbol.t =
  let visibility : Asm_targets.Asm_symbol.visibility =
    match s.sym_global with Cmm.Global -> Global | Cmm.Local -> Local
  in
  Asm_targets.Asm_symbol.create ~visibility s.sym_name

(* Switches to the section for a data phrase: with link-order frametables and
   function sections, a phrase defining a symbol gets its own section, named
   after the first symbol it defines, so that the linker can discard the data
   when it is unreferenced. *)
let enter_data_section (l : Cmm.data_item list) =
  let module D = Asm_targets.Asm_directives in
  let own_section_symbol =
    if Config.link_order_frametables && !Clflags.function_sections
    then
      List.find_map
        (fun[@ocaml.warning "-4"] (item : Cmm.data_item) ->
          match item with
          | Cdefine_symbol s ->
            Some (Asm_targets.Asm_symbol.encode (symbol_of_cmm_symbol s))
          | _ -> None)
        l
    else None
  in
  match own_section_symbol with
  | Some sym -> D.switch_to_section (Data_symbol sym)
  | None -> D.data ()

let emit_frametable_marker actions ~link_symbol sym_name =
  let module D = Asm_targets.Asm_directives in
  let piece : Asm_targets.Asm_section.t = Frametable_piece { link_symbol } in
  let sym = Asm_targets.Asm_symbol.create_global sym_name in
  D.switch_to_section piece;
  actions.global_maybe_protected sym;
  actions.symbol_defined sym_name;
  D.define_symbol_label ~section:piece sym

let emit_data_item actions (d : Cmm.data_item) =
  let module D = Asm_targets.Asm_directives in
  let module L = Asm_targets.Asm_label in
  match d with
  | Cdefine_symbol s -> (
    (* Labels are named independently of their section; only definitions are
       checked against the current section (possibly a [Data_symbol]). *)
    let sym = symbol_of_cmm_symbol s in
    let section = D.current_section () in
    match s.sym_global with
    | Local -> D.define_label (L.create_label_for_local_symbol section sym)
    | Global ->
      actions.global_maybe_protected sym;
      actions.symbol_defined s.sym_name;
      D.define_joint_label_and_symbol ~section sym)
  | Cint8 n -> D.int8 (Numbers.Int8.of_int_exn n)
  | Cint16 n -> D.int16 (Numbers.Int16.of_int_exn n)
  | Cint32 n -> D.int32 (Numbers.Int64.to_int32_exn (Int64.of_nativeint n))
  (* CR mshinwell: Add [Targetint.of_nativeint] *)
  | Cint n -> D.targetint (Targetint.of_int64 (Int64.of_nativeint n))
  | Csingle f -> D.float32 f
  | Cdouble f -> D.float64 f
  (* SIMD vectors respect little-endian byte order *)
  | Cvec128 { word0; word1 } ->
    D.float64_from_bits word0;
    D.float64_from_bits word1
  | Cvec256 { word0; word1; word2; word3 } ->
    D.float64_from_bits word0;
    D.float64_from_bits word1;
    D.float64_from_bits word2;
    D.float64_from_bits word3
  | Cvec512 { word0; word1; word2; word3; word4; word5; word6; word7 } ->
    D.float64_from_bits word0;
    D.float64_from_bits word1;
    D.float64_from_bits word2;
    D.float64_from_bits word3;
    D.float64_from_bits word4;
    D.float64_from_bits word5;
    D.float64_from_bits word6;
    D.float64_from_bits word7
  | Csymbol_address s -> (
    actions.symbol_used s.sym_name;
    let sym = symbol_of_cmm_symbol s in
    match s.sym_global with
    | Global -> D.symbol sym
    | Local -> D.label (L.create_label_for_local_symbol Data sym))
  | Csymbol_offset (s, o) -> (
    actions.symbol_used s.sym_name;
    let sym = symbol_of_cmm_symbol s in
    match s.sym_global with
    | Global ->
      D.symbol_plus_offset ~offset_in_bytes:(Targetint.of_int_exn o) sym
    | Local ->
      D.label_plus_offset ~offset_in_bytes:(Targetint.of_int_exn o)
        (L.create_label_for_local_symbol Data sym))
  | Cstring s -> D.string s
  | Cskip n -> D.space ~bytes:n
  | Calign n -> D.align ~fill:Zero ~bytes:n

let reset () =
  reset_debug_info ();
  frame_descriptors := [];
  reset_frame_tables ();
  stapsdt_base_emitted := false
