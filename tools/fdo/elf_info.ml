(* The little we need to know about the profiled executable's ELF file: its
   loadable segments (to translate the runtime addresses perf sampled back to
   the link-time addresses of the FDO metadata, which for a position-independent
   executable differ by the load address) and the contents of its FDO metadata
   section. *)

module Owee_buf = Compiler_owee.Owee_buf
module Owee_elf = Compiler_owee.Owee_elf

(* A PT_LOAD program header: the bytes at file offsets [offset, offset + filesz)
   are loaded at link-time addresses [vaddr, vaddr + filesz). *)
type segment =
  { offset : int64;
    vaddr : int64;
    filesz : int64;
    executable : bool
  }

type t =
  { filename : string;
    e_type : int;
    buffer : Owee_buf.t;
    sections : Owee_elf.section array;
    segments : segment list
  }

let et_dyn = 3

let is_pie t = t.e_type = et_dyn

(* Eta-expansion gives [create_process] and [waitpid] the unannotated types
   required by [Compiler_owee.Unix_intf.S]. *)
module Unix_for_owee = struct
  include Unix

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args stdin stdout stderr

  let waitpid flags pid = Unix.waitpid flags pid
end

let pt_load = 1

(* The ELF64 program header table; [Owee_elf] does not read it. *)
let read_segments buffer (header : Owee_elf.header) =
  List.init header.e_phnum (fun i ->
      let at = Int64.to_int header.e_phoff + (i * header.e_phentsize) in
      let cursor = Owee_buf.cursor ~at buffer in
      let p_type = Owee_buf.Read.u32 cursor in
      let p_flags = Owee_buf.Read.u32 cursor in
      let offset = Owee_buf.Read.u64 cursor in
      let vaddr = Owee_buf.Read.u64 cursor in
      let _p_paddr = Owee_buf.Read.u64 cursor in
      let filesz = Owee_buf.Read.u64 cursor in
      if p_type = pt_load
      then Some { offset; vaddr; filesz; executable = p_flags land 1 <> 0 }
      else None)
  |> List.filter_map Fun.id

let read filename =
  let buffer =
    Owee_buf.map_binary
      (module Unix_for_owee : Compiler_owee.Unix_intf.S)
      filename
  in
  let header, sections = Owee_elf.read_elf buffer in
  { filename;
    e_type = header.e_type;
    buffer;
    sections;
    segments = read_segments buffer header
  }

let section_body t name =
  match Owee_elf.find_section t.sections name with
  | None -> None
  | Some section when Int64.equal (Int64.logand section.sh_flags 0x800L) 0L ->
    Some (Owee_elf.section_body_string t.buffer section)
  | Some _ ->
    (* SHF_COMPRESSED: use the same objcopy as the compiler's debug splitter.
       --dump-section runs before decompression, so read the resulting ELF. *)
    let temporary = Filename.temp_file "fdo-debug" ".elf" in
    Fun.protect
      ~finally:(fun () -> Misc.remove_file temporary)
      (fun () ->
        let command =
          Printf.sprintf "%s --decompress-debug-sections %s %s" Config.objcopy
            (Filename.quote t.filename)
            (Filename.quote temporary)
        in
        if Ccomp.command command <> 0
        then failwith ("Cannot decompress debug sections in " ^ t.filename);
        let elf = read temporary in
        match Owee_elf.find_section elf.sections name with
        | None -> failwith ("Decompression removed section " ^ name)
        | Some section -> Some (Owee_elf.section_body_string elf.buffer section))

let code_byte t address =
  List.find_map
    (fun { offset; vaddr; filesz; executable } ->
      if (not executable) || Int64.compare address vaddr < 0
      then None
      else
        let delta = Int64.sub address vaddr in
        if Int64.compare delta filesz >= 0
        then None
        else
          let at = Int64.add offset delta in
          if
            Int64.unsigned_compare at (Int64.of_int (Owee_buf.size t.buffer))
            >= 0
          then None
          else Some (Bigarray.Array1.get t.buffer (Int64.to_int at)))
    t.segments

let address_of_offset t offset =
  List.find_map
    (fun { offset = start; vaddr; filesz; executable = _ } ->
      if
        Int64.compare start offset <= 0
        && Int64.compare offset (Int64.add start filesz) < 0
      then Some (Int64.add vaddr (Int64.sub offset start))
      else None)
    t.segments
