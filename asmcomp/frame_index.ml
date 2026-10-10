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

[@@@ocaml.warning "+a-40-41-42"]

module Buf = Compiler_owee.Owee_buf
module Elf = Compiler_owee.Owee_elf
module Archive = Compiler_owee.Owee_archive
module Section_index = Compiler_owee.Owee_elf_relocation.Section_index
module Layout = Frame_index_layout

type error =
  | No_symbol_table of { file : string }
  | Index_symbol_not_found of { file : string }
  | Bad_reservation of
      { file : string;
        expected_size : int;
        actual_size : int
      }
  | Too_many_descriptors of
      { file : string;
        entries : int;
        reserved : int
      }
  | Duplicate_return_address of
      { file : string;
        retaddr : int
      }
  | Text_too_sparse of
      { file : string;
        text_lo : int;
        text_hi : int;
        bucket_budget : int
      }
  | Offset_overflow of
      { file : string;
        what : string
      }
  | Self_check_failed of
      { file : string;
        retaddr : int
      }

exception Error of error

let frametable_suffix = "__frametable"

let is_frametable_symbol (sym : Elf.symbol) =
  String.ends_with ~suffix:frametable_suffix sym.name
  && Section_index.is_defined sym.st_shndx

(* Addresses and offsets are manipulated as [int]: the index is only built for
   64-bit ELF executables, on a 64-bit host. *)
let to_int = Int64.to_int

let read_u8 buf pos = Bigarray.Array1.get buf pos

let read_u16 buf pos = read_u8 buf pos lor (read_u8 buf (pos + 1) lsl 8)

let read_u32 buf pos = read_u16 buf pos lor (read_u16 buf (pos + 2) lsl 16)

let read_i32 buf pos =
  let u = read_u32 buf pos in
  if u land 0x8000_0000 <> 0 then u - 0x1_0000_0000 else u

let read_u64 buf pos =
  let lo = read_u32 buf pos and hi = read_u32 buf (pos + 4) in
  if hi land 0x8000_0000 <> 0
  then Misc.fatal_errorf "Frame_index: 64-bit value out of range"
  else lo lor (hi lsl 32)

let find_section_of_type (sections : Elf.section array) ty =
  Array.find_opt
    (fun (section : Elf.section) ->
      Elf.Section_type.(equal (of_u32 section.sh_type) ty))
    sections

let find_symtab sections =
  find_section_of_type sections Elf.Section_type.sht_symtab

let read_symbols ~file buf (sections : Elf.section array) =
  match find_symtab sections with
  | None -> [||]
  | Some symtab ->
    if symtab.sh_link >= Array.length sections
    then Misc.fatal_errorf "Frame_index: symtab sh_link out of range in %s" file;
    let strtab = sections.(symtab.sh_link) in
    Elf.read_symbols
      ~symtab_body:(Elf.section_body buf symtab)
      ~strtab_body:(Elf.section_body buf strtab)

(* The section holding a defined symbol. An object with 65280 or more sections
   marks such a symbol with SHN_XINDEX and stores its section index in the
   SHT_SYMTAB_SHNDX table, indexed by symbol number. *)
let section_of_symbol ~file buf (sections : Elf.section array) ~sym_index
    (sym : Elf.symbol) =
  let index =
    if
      Section_index.to_int sym.st_shndx
      = Section_index.to_int Section_index.xindex
    then
      match find_section_of_type sections Elf.Section_type.sht_symtab_shndx with
      | Some shndx -> read_u32 (Elf.section_body buf shndx) (4 * sym_index)
      | None ->
        Misc.fatal_errorf
          "Frame_index: symbol %s of %s uses SHN_XINDEX but the file has no \
           SHT_SYMTAB_SHNDX section"
          sym.name file
    else Section_index.to_int sym.st_shndx
  in
  if index <= 0 || index >= Array.length sections
  then
    Misc.fatal_errorf "Frame_index: symbol %s has section index %d in %s"
      sym.name index file;
  sections.(index)

(* File offset of a defined symbol. [st_value] is section-relative in a
   relocatable object (where [sh_addr] is zero) and a virtual address in an
   executable. *)
let file_offset_of_symbol ~file buf sections ~sym_index (sym : Elf.symbol) =
  let section = section_of_symbol ~file buf sections ~sym_index sym in
  to_int section.sh_offset + (to_int sym.st_value - to_int section.sh_addr)

(* ---- Pre-link estimate ---- *)

(* The frametable symbols of the given units; [None] when the units are not
   known, in which case every frametable of the file counts. *)
let frametable_symbols_of_units units =
  match units with
  | [] -> None
  | _ :: _ ->
    Some
      (Misc.Stdlib.String.Set.of_list
         (List.map
            (fun compilation_unit ->
              Cmm_helpers.make_symbol ~compilation_unit "frametable")
            units))

let count_descriptors_in_elf ~file ~wanted buf =
  let _header, sections = Elf.read_elf buf in
  let symbols = read_symbols ~file buf sections in
  let total = ref 0 in
  Array.iteri
    (fun sym_index (sym : Elf.symbol) ->
      let wanted =
        match wanted with
        | None -> true
        | Some wanted -> Misc.Stdlib.String.Set.mem sym.name wanted
      in
      if wanted && is_frametable_symbol sym
      then
        total
          := !total
             + read_u64 buf
                 (file_offset_of_symbol ~file buf sections ~sym_index sym))
    symbols;
  !total

let count_descriptors_in_file unix { Linkenv.path = file; units } =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  if String.starts_with ~prefix:"-" file || not (Sys.file_exists file)
  then 0
  else
    let wanted = frametable_symbols_of_units units in
    let buf = Buf.map_binary (module Unix) file in
    if Elf.is_elf buf
    then count_descriptors_in_elf ~file ~wanted buf
    else if Archive.is_archive buf
    then
      let archive, members = Archive.read buf in
      List.fold_left
        (fun acc member ->
          let body = Archive.member_body archive member in
          if Elf.is_elf body
          then acc + count_descriptors_in_elf ~file ~wanted body
          else acc)
        0 members
    else 0

let estimate_descriptors unix objfiles =
  Profile.record_call "frame_index_estimate" (fun () ->
      List.fold_left
        (fun acc objfile -> acc + count_descriptors_in_file unix objfile)
        0 objfiles)

(* ---- Decoding frametables in the linked executable ---- *)

(* Reads bytes of one section of a mapped executable by virtual address. *)
module Section_reader = struct
  type t =
    { buf : Buf.t;
      file : string;
      name : string;
      lo : int;
      hi : int;
      base : int
    }

  let create ~file buf (section : Elf.section) =
    let lo = to_int section.sh_addr in
    { buf;
      file;
      name = section.sh_name_str;
      lo;
      hi = lo + to_int section.sh_size;
      base = to_int section.sh_offset - lo
    }

  let check t vaddr len =
    if vaddr < t.lo || vaddr + len > t.hi
    then
      Misc.fatal_errorf
        "Frame_index: read of %d bytes at 0x%x outside section %s of %s" len
        vaddr t.name t.file

  let u8 t vaddr =
    check t vaddr 1;
    read_u8 t.buf (vaddr + t.base)

  let u16 t vaddr =
    check t vaddr 2;
    read_u16 t.buf (vaddr + t.base)

  let u32 t vaddr =
    check t vaddr 4;
    read_u32 t.buf (vaddr + t.base)

  let i32 t vaddr =
    check t vaddr 4;
    read_i32 t.buf (vaddr + t.base)

  let u64 t vaddr =
    check t vaddr 8;
    read_u64 t.buf (vaddr + t.base)
end

(* Constants of the descriptor encoding; see runtime/caml/frame_descriptors.h
   and [Emitaux.emit_frames]. *)
let frame_descriptor_debug = 1

let frame_descriptor_alloc = 2

let frame_return_to_c = 0xFFFF

let frame_long_marker = 0x7FFF

let word_size = 8

(* Decode the descriptor whose delta field starts at virtual address [p], given
   the previous descriptor's return address. Returns its return address, the
   address of its body (the [frame_descr *] the runtime uses), and the address
   of the next descriptor's delta field. This mirrors [caml_decode_frame_descr]
   in runtime/frame_descriptors.c. *)
let decode_descriptor reader p ~prev_retaddr =
  let module R = Section_reader in
  let has_allocs flags = flags land frame_descriptor_alloc <> 0 in
  let has_debug flags = flags land frame_descriptor_debug <> 0 in
  let debug_words ~flags ~num_allocs q =
    let num_debuginfo = if has_allocs flags then num_allocs else 1 in
    if has_debug flags then q + (4 * num_debuginfo) else q
  in
  if R.u8 reader p = 0
  then
    (* Escaped: the descriptor carries its own return address, relative to the
       field holding it. *)
    let retaddr = p + 1 + R.i32 reader (p + 1) in
    let frame_data16 = R.u16 reader (p + 5) in
    let next =
      if frame_data16 = frame_return_to_c
      then p + 9
      else
        let flags, q =
          if frame_data16 = frame_long_marker
          then
            let frame_data = R.u32 reader (p + 9) in
            let num_live = R.u32 reader (p + 13) in
            frame_data land 3, p + 17 + (4 * num_live)
          else
            let num_live = R.u16 reader (p + 7) in
            frame_data16 land 3, p + 9 + (2 * num_live)
        in
        let num_allocs, q =
          if has_allocs flags
          then
            let num_allocs = R.u8 reader q in
            num_allocs, q + 1 + num_allocs
          else 0, q
        in
        debug_words ~flags ~num_allocs q
    in
    retaddr, p, next
  else
    (* Short: a ULEB128 delta (which an assembler may pad to a non-minimal
       encoding) followed by the body. *)
    let rec uleb128 q shift acc =
      let byte = R.u8 reader q in
      let acc = acc lor ((byte land 0x7f) lsl shift) in
      if byte land 0x80 <> 0
      then uleb128 (q + 1) (shift + 7) acc
      else q + 1, acc
    in
    let body, delta = uleb128 p 0 0 in
    let retaddr = prev_retaddr + delta in
    let size_flags = R.u8 reader body in
    let flags = size_flags land 3 in
    let frame_size = (size_flags lsr 2) * 16 in
    let num_allocs, q =
      if has_allocs flags
      then
        let num_allocs = R.u8 reader (body + 2) in
        num_allocs, body + 3 + ((num_allocs + 1) / 2)
      else 0, body + 1
    in
    let live_bytes = ((frame_size / word_size) + 7) / 8 in
    let next = debug_words ~flags ~num_allocs (q + live_bytes) in
    retaddr, body, next

(* [(retaddr, body)] for every descriptor of the frametable at [table]. *)
let decode_frametable reader ~table acc =
  let count = Section_reader.u64 reader table in
  let rec loop p ~prev_retaddr remaining acc =
    if remaining = 0
    then acc
    else
      let retaddr, body, next = decode_descriptor reader p ~prev_retaddr in
      loop next ~prev_retaddr:retaddr (remaining - 1) ((retaddr, body) :: acc)
  in
  loop (table + word_size) ~prev_retaddr:0 count acc

(* ---- Building the index ---- *)

type index =
  { layout : Layout.t;
    shift : int;
    text_lo : int;
    num_granules : int;
    ft_lo : int;
    ft_hi : int;
    entries : (int * int) array;
    (* sorted by return address *)
    bucket : int array;
    largest_bucket : int
  }

let choose_shift ~file ~layout ~text_min ~text_max =
  let budget = layout.Layout.bucket_budget in
  let rec choose shift =
    let granule = 1 lsl shift in
    let text_lo = text_min land lnot (granule - 1) in
    let num_granules = ((text_max - text_lo) lsr shift) + 1 in
    if num_granules <= budget
    then shift, text_lo, num_granules
    else if shift >= Layout.max_shift
    then
      raise
        (Error
           (Text_too_sparse
              { file;
                text_lo = text_min;
                text_hi = text_max + 1;
                bucket_budget = budget
              }))
    else choose (shift + 1)
  in
  choose Layout.min_shift

let make_index ~file ~layout ~ft_lo ~ft_hi descriptors =
  let entries = Array.of_list descriptors in
  Array.sort (fun (a, _) (b, _) -> Int.compare a b) entries;
  let num_entries = Array.length entries in
  for i = 1 to num_entries - 1 do
    let retaddr, _ = entries.(i) in
    if retaddr = fst entries.(i - 1)
    then raise (Error (Duplicate_return_address { file; retaddr }))
  done;
  if num_entries > layout.Layout.reserved_entries
  then
    raise
      (Error
         (Too_many_descriptors
            { file;
              entries = num_entries;
              reserved = layout.Layout.reserved_entries
            }));
  let shift, text_lo, num_granules =
    if num_entries = 0
    then Layout.min_shift, 0, 0
    else
      choose_shift ~file ~layout
        ~text_min:(fst entries.(0))
        ~text_max:(fst entries.(num_entries - 1))
  in
  (* [bucket.(g)] is the index of the first entry in granule [g]; the last
     element is [num_entries]. *)
  let bucket = Array.make (num_granules + 1) num_entries in
  let next_granule = ref 0 in
  Array.iteri
    (fun i (retaddr, _) ->
      let granule = (retaddr - text_lo) lsr shift in
      while !next_granule <= granule do
        bucket.(!next_granule) <- i;
        incr next_granule
      done)
    entries;
  let largest_bucket = ref 0 in
  for g = 0 to num_granules - 1 do
    largest_bucket := max !largest_bucket (bucket.(g + 1) - bucket.(g))
  done;
  let fits_u32 what v =
    if v < 0 || v > 0xFFFF_FFFF
    then raise (Error (Offset_overflow { file; what }))
  in
  Array.iter
    (fun (_, body) -> fits_u32 "descriptor offset" (body - ft_lo))
    entries;
  fits_u32 "number of granules" num_granules;
  fits_u32 "number of entries" num_entries;
  { layout;
    shift;
    text_lo;
    num_granules;
    ft_lo;
    ft_hi;
    entries;
    bucket;
    largest_bucket = !largest_bucket
  }

let serialize index =
  let layout = index.layout in
  let bytes = Bytes.make (Layout.total_bytes layout) '\000' in
  let set_u32 pos v = Bytes.set_int32_le bytes pos (Int32.of_int v) in
  let set_u64 pos v = Bytes.set_int64_le bytes pos (Int64.of_int v) in
  Bytes.set_int64_le bytes Layout.magic_offset Layout.magic;
  set_u32 Layout.version_offset Layout.version;
  set_u32 Layout.shift_offset index.shift;
  set_u64 Layout.text_lo_offset index.text_lo;
  set_u64 Layout.num_granules_offset index.num_granules;
  set_u64 Layout.num_entries_offset (Array.length index.entries);
  set_u64 Layout.ft_lo_offset index.ft_lo;
  set_u64 Layout.ft_hi_offset index.ft_hi;
  set_u32 Layout.reserved_entries_offset layout.reserved_entries;
  set_u32 Layout.bucket_budget_offset layout.bucket_budget;
  Array.iteri
    (fun g first -> set_u32 (Layout.bucket_offset + (4 * g)) first)
    index.bucket;
  let entries = Layout.entries_offset layout in
  let granule_mask = (1 lsl index.shift) - 1 in
  Array.iteri
    (fun i (retaddr, body) ->
      let entry = entries + (Layout.entry_size * i) in
      set_u32 entry ((retaddr - index.text_lo) land granule_mask);
      set_u32 (entry + 4) (body - index.ft_lo))
    index.entries;
  bytes

(* The runtime's lookup ([caml_find_frame_descr]), run against the index as
   written to the file. Returns the descriptor's virtual address. *)
let lookup buf ~at pc =
  let u32 ofs = read_u32 buf (at + ofs) in
  let u64 ofs = read_u64 buf (at + ofs) in
  let shift = u32 Layout.shift_offset in
  let text_lo = u64 Layout.text_lo_offset in
  let num_granules = u64 Layout.num_granules_offset in
  let layout =
    Layout.of_header
      ~reserved_entries:(u32 Layout.reserved_entries_offset)
      ~bucket_budget:(u32 Layout.bucket_budget_offset)
  in
  if pc < text_lo || (pc - text_lo) lsr shift >= num_granules
  then None
  else
    let granule = (pc - text_lo) lsr shift in
    let lo = u32 (Layout.bucket_offset + (4 * granule)) in
    let hi = u32 (Layout.bucket_offset + (4 * (granule + 1))) in
    let off = (pc - text_lo) land ((1 lsl shift) - 1) in
    let entries = Layout.entries_offset layout in
    let rec scan i =
      if i >= hi
      then None
      else
        let entry = entries + (Layout.entry_size * i) in
        if u32 entry = off
        then Some (u64 Layout.ft_lo_offset + u32 (entry + 4))
        else scan (i + 1)
    in
    scan lo

let self_check ~file buf ~at index =
  let entries = index.entries in
  let num_entries = Array.length entries in
  let is_retaddr pc =
    let rec search lo hi =
      if lo >= hi
      then false
      else
        let mid = (lo + hi) / 2 in
        let r = fst entries.(mid) in
        if r = pc
        then true
        else if r < pc
        then search (mid + 1) hi
        else search lo mid
    in
    search 0 num_entries
  in
  let fail retaddr = raise (Error (Self_check_failed { file; retaddr })) in
  Array.iter
    (fun (retaddr, body) ->
      match lookup buf ~at retaddr with
      | Some found when found = body -> ()
      | Some _ | None -> fail retaddr)
    entries;
  (* Addresses next to descriptors, and beyond both ends of the covered text,
     must not be found. *)
  let expect_absent pc = if lookup buf ~at pc <> None then fail pc in
  let stride = max 1 (num_entries / 64) in
  let i = ref 0 in
  while !i < num_entries do
    let retaddr, _ = entries.(!i) in
    if not (is_retaddr (retaddr + 1)) then expect_absent (retaddr + 1);
    if retaddr > 0 && not (is_retaddr (retaddr - 1))
    then expect_absent (retaddr - 1);
    i := !i + stride
  done;
  if num_entries > 0
  then (
    expect_absent (fst entries.(num_entries - 1) + 1);
    let first = fst entries.(0) in
    if first > 0 then expect_absent (first - 1))

let build unix ~file =
  let module Unix = (val unix : Compiler_owee.Unix_intf.S) in
  let buf = Buf.map_binary (module Unix) file in
  let _header, sections = Elf.read_elf buf in
  if Option.is_none (find_symtab sections)
  then raise (Error (No_symbol_table { file }));
  let symbols = read_symbols ~file buf sections in
  let index_sym_index, index_sym =
    let rec find sym_index =
      if sym_index >= Array.length symbols
      then raise (Error (Index_symbol_not_found { file }))
      else
        let sym = symbols.(sym_index) in
        if
          String.equal sym.name Layout.symbol_name
          && Section_index.is_defined sym.st_shndx
        then sym_index, sym
        else find (sym_index + 1)
    in
    find 0
  in
  let index_section =
    section_of_symbol ~file buf sections ~sym_index:index_sym_index index_sym
  in
  if not Elf.Section_type.(equal (of_u32 index_section.sh_type) sht_progbits)
  then
    Misc.fatal_errorf "Frame_index: section %s of %s is not PROGBITS"
      index_section.sh_name_str file;
  let at =
    file_offset_of_symbol ~file buf sections ~sym_index:index_sym_index
      index_sym
  in
  let layout =
    Layout.of_header
      ~reserved_entries:(read_u32 buf (at + Layout.reserved_entries_offset))
      ~bucket_budget:(read_u32 buf (at + Layout.bucket_budget_offset))
  in
  let actual_size = to_int index_sym.st_size in
  let expected_size = Layout.total_bytes layout in
  if actual_size <> expected_size
  then raise (Error (Bad_reservation { file; expected_size; actual_size }));
  if Layout.is_empty layout
  then ()
  else
    match Elf.find_section sections "caml_frametables" with
    | None ->
      if !Clflags.verbose
      then
        Printf.eprintf
          "frame index: %s has no caml_frametables section; index left absent\n\
           %!"
          file
    | Some ft_section ->
      let reader = Section_reader.create ~file buf ft_section in
      let ft_lo = reader.Section_reader.lo
      and ft_hi = reader.Section_reader.hi in
      let descriptors, skipped =
        Array.fold_left
          (fun (descriptors, skipped) (sym : Elf.symbol) ->
            if not (is_frametable_symbol sym)
            then descriptors, skipped
            else
              let table = to_int sym.st_value in
              if table >= ft_lo && table < ft_hi
              then decode_frametable reader ~table descriptors, skipped
              else descriptors, sym.name :: skipped)
          ([], []) symbols
      in
      if !Clflags.verbose
      then
        List.iter
          (fun name ->
            Printf.eprintf
              "frame index: %s lies outside caml_frametables; not indexed\n%!"
              name)
          skipped;
      let index = make_index ~file ~layout ~ft_lo ~ft_hi descriptors in
      let bytes = serialize index in
      let oc = open_out_gen [Open_wronly; Open_binary] 0 file in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          seek_out oc at;
          output_bytes oc bytes);
      let buf = Buf.map_binary (module Unix) file in
      self_check ~file buf ~at index;
      if !Clflags.verbose
      then
        Printf.eprintf
          "frame index: %d entries, %d-byte granules, %d granules, largest \
           bucket %d, %d bytes\n\
           %!"
          (Array.length index.entries)
          (1 lsl index.shift) index.num_granules index.largest_bucket
          (Bytes.length bytes)

let build unix ~file =
  Profile.record_call "frame_index_build" (fun () -> build unix ~file)

(* ---- Error reporting ---- *)

let report_error ppf = function
  | No_symbol_table { file } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ the executable has no \
       symbol table (was it linked with -s?).@ Strip it after linking, or link \
       with -no-frametable-index"
      file
  | Index_symbol_not_found { file } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ the executable does not \
       define %s"
      file Layout.symbol_name
  | Bad_reservation { file; expected_size; actual_size } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ %s is %d bytes but its \
       header describes a reservation of %d bytes"
      file Layout.symbol_name actual_size expected_size
  | Too_many_descriptors { file; entries; reserved } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ the executable holds %d \
       frame descriptors but only %d were reserved for at link time"
      file entries reserved
  | Duplicate_return_address { file; retaddr } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ two frame descriptors \
       share the return address 0x%x"
      file retaddr
  | Text_too_sparse { file; text_lo; text_hi; bucket_budget } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ the return addresses \
       span 0x%x-0x%x, too sparse for the %d buckets reserved"
      file text_lo text_hi bucket_budget
  | Offset_overflow { file; what } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ %s does not fit in 32 \
       bits"
      file what
  | Self_check_failed { file; retaddr } ->
    Format_doc.fprintf ppf
      "Cannot build the frame-descriptor index of %s:@ the self-check of the \
       written index failed at 0x%x"
      file retaddr

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
