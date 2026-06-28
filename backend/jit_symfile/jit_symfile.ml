(** Generic in-memory ELF symfile builder. See .mli for overview. *)

module Buf = Compiler_owee.Owee_buf
module Elf = Compiler_owee.Owee_elf

(* ELF constants *)
let elf_header_size = 64

let section_header_size = 64

let symbol_entry_size = 24

let sht_null = 0

let sht_progbits = 1

let sht_symtab = 2

let sht_strtab = 3

let shf_write = 0x1

let shf_alloc = 0x2

let shf_execinstr = 0x4

let stb_global = 1

let stt_notype = 0

let stt_object = 1

let stt_func = 2

(* Append-only byte string accumulator with offset tracking; index 0 is
   reserved for the empty string. *)
module Strtab : sig
  type t

  val create : unit -> t

  (** [add t s] appends [s] (zero-terminated) and returns its offset. *)
  val add : t -> string -> int

  val size : t -> int

  val write : t -> Buf.cursor -> unit
end = struct
  type t =
    { mutable rev_entries : string list;
      mutable size : int
    }

  let create () = { rev_entries = [ "" ]; size = 1 }

  let add t s =
    let off = t.size in
    t.rev_entries <- s :: t.rev_entries;
    t.size <- off + String.length s + 1;
    off

  let size t = t.size

  let write t cursor =
    List.iter
      (Buf.Write.zero_terminated_string cursor)
      (List.rev t.rev_entries)
end

let is_local_label name =
  String.length name >= 2 && Char.equal name.[0] '.' && Char.equal name.[1] 'L'

let starts_with ~prefix s =
  let pl = String.length prefix in
  String.length s >= pl && String.equal (String.sub s 0 pl) prefix

let is_text_like name =
  String.equal name ".text" || starts_with ~prefix:".text." name

let is_data_like name =
  String.equal name ".data"
  || starts_with ~prefix:".data." name
  || String.equal name ".bss"
  || String.equal name ".rodata"
  || starts_with ~prefix:".rodata." name

let section_flags name =
  if is_text_like name then Int64.of_int (shf_alloc lor shf_execinstr)
  else if is_data_like name then Int64.of_int (shf_alloc lor shf_write)
  else Int64.of_int shf_alloc

let section_alignment name = if is_text_like name then 16L else 8L

let default_symbol_type name =
  if is_text_like name then stt_func
  else if is_data_like name then stt_object
  else stt_notype

let st_info ~bind ~ty = (bind lsl 4) lor ty

let target_to_name (target : Binary_emitter_intf.target) =
  match target with
  | Binary_emitter_intf.Symbol s -> Asm_targets.Asm_symbol.encode s
  | Binary_emitter_intf.Label l -> Asm_targets.Asm_label.encode l

(* A symbol with its string-table offset already resolved. *)
type symbol =
  { st_name : int;
    st_value : int64;
    st_size : int64;
    st_info : int;
    st_other : int;
    st_shndx : int
  }

let collect_symbols (type a r)
    (module S : Binary_emitter_intf.Assembled_section
      with type t = a
       and type relocation = r) ~strtab sections =
  let acc = ref [] in
  List.iteri
    (fun i (name, raw) ->
      let shndx = i + 1 in
      let ty = default_symbol_type name in
      S.iter_labels_and_symbols raw ~f:(fun target ~offset ->
          let sym_name = target_to_name target in
          if not (is_local_label sym_name)
          then
            let st_name = Strtab.add strtab sym_name in
            acc
              := { st_name;
                   st_value = Int64.of_int offset;
                   st_size = 0L;
                   st_info = st_info ~bind:stb_global ~ty;
                   st_other = 0;
                   st_shndx = shndx
                 }
                 :: !acc))
    sections;
  List.rev !acc

let write_symbol cursor s =
  Buf.Write.u32 cursor s.st_name;
  Buf.Write.u8 cursor s.st_info;
  Buf.Write.u8 cursor s.st_other;
  Buf.Write.u16 cursor s.st_shndx;
  Buf.Write.u64 cursor s.st_value;
  Buf.Write.u64 cursor s.st_size

(* Build a section header. *)
let make_section_header ~sh_name ~sh_type ~sh_flags ~sh_addr ~sh_offset
    ~sh_size ~sh_link ~sh_info ~sh_addralign ~sh_entsize ~sh_name_str :
    Elf.section =
  { sh_name;
    sh_type;
    sh_flags;
    sh_addr;
    sh_offset;
    sh_size;
    sh_link;
    sh_info;
    sh_addralign;
    sh_entsize;
    sh_name_str
  }

let null_section_header () =
  make_section_header ~sh_name:0 ~sh_type:sht_null ~sh_flags:0L ~sh_addr:0L
    ~sh_offset:0L ~sh_size:0L ~sh_link:0 ~sh_info:0 ~sh_addralign:0L
    ~sh_entsize:0L ~sh_name_str:""

(* Compute body sizes in the file. For each input section, sh_size is the
   body's footprint in the file (= max of body_size and runtime_size). *)
let resolve_sh_size raw runtime_size body_size_fn =
  let body_size = body_size_fn raw in
  let sh_size =
    match runtime_size with
    | None -> body_size
    | Some n when n >= body_size -> n
    | Some n ->
      failwith
        (Printf.sprintf
           "Jit_symfile.build: section_runtime_size (%d) < body size (%d)"
           n body_size)
  in
  body_size, sh_size

let build (type a r)
    (module S : Binary_emitter_intf.Assembled_section
      with type t = a
       and type relocation = r)
    ~e_machine ~sections ~section_address ~section_runtime_size =
  let n_input = List.length sections in
  let symtab_shndx = n_input + 1 in
  let strtab_shndx = n_input + 2 in
  let shstrtab_shndx = n_input + 3 in
  let total_shnum = n_input + 4 in
  let shstrtab = Strtab.create () in
  let strtab = Strtab.create () in
  (* Compute layout offsets for input section bodies. We register section
     names in shstrtab in the same order as the section header table to keep
     order deterministic. *)
  let layout =
    Array.make n_input (0, 0, 0L, 0L, 0L, 0, 0L, 0L)
    (* (sh_name, body_size, sh_size_64, sh_offset, sh_addr, _shndx, sh_flags,
       sh_addralign) *)
  in
  let cur_offset = ref (Int64.of_int elf_header_size) in
  List.iteri
    (fun i (name, raw) ->
      let body_size, sh_size =
        resolve_sh_size raw (section_runtime_size name) S.size
      in
      let sh_addr =
        match section_address name with None -> 0L | Some a -> a
      in
      let sh_name = Strtab.add shstrtab name in
      layout.(i)
        <- ( sh_name,
             body_size,
             Int64.of_int sh_size,
             !cur_offset,
             sh_addr,
             i + 1,
             section_flags name,
             section_alignment name );
      cur_offset := Int64.add !cur_offset (Int64.of_int sh_size))
    sections;
  (* Collect symbols. This populates strtab for symbol names. *)
  let symbols = collect_symbols (module S) ~strtab sections in
  let n_symbols_with_null = List.length symbols + 1 in
  let symtab_size = symbol_entry_size * n_symbols_with_null in
  let symtab_offset = !cur_offset in
  cur_offset := Int64.add !cur_offset (Int64.of_int symtab_size);
  let symtab_sh_name = Strtab.add shstrtab ".symtab" in
  let strtab_offset = !cur_offset in
  cur_offset := Int64.add !cur_offset (Int64.of_int (Strtab.size strtab));
  let strtab_sh_name = Strtab.add shstrtab ".strtab" in
  let shstrtab_offset = !cur_offset in
  let shstrtab_sh_name = Strtab.add shstrtab ".shstrtab" in
  cur_offset := Int64.add !cur_offset (Int64.of_int (Strtab.size shstrtab));
  let sh_table_offset = !cur_offset in
  let total_size =
    Int64.to_int sh_table_offset
    + (total_shnum * section_header_size)
  in
  (* Build the section header array. *)
  let headers = Array.make total_shnum (null_section_header ()) in
  Array.iteri
    (fun i
         ( sh_name,
           _body_size,
           sh_size,
           sh_offset,
           sh_addr,
           _shndx,
           sh_flags,
           sh_addralign ) ->
      let name, _ = List.nth sections i in
      headers.(i + 1)
        <- make_section_header ~sh_name ~sh_type:sht_progbits ~sh_flags
             ~sh_addr ~sh_offset ~sh_size ~sh_link:0 ~sh_info:0 ~sh_addralign
             ~sh_entsize:0L ~sh_name_str:name)
    layout;
  (* Symbol table header. sh_info = number of local symbols (= 1 because the
     NULL entry is the only local; everything else we emit is GLOBAL). *)
  headers.(symtab_shndx)
    <- make_section_header ~sh_name:symtab_sh_name ~sh_type:sht_symtab
         ~sh_flags:0L ~sh_addr:0L ~sh_offset:symtab_offset
         ~sh_size:(Int64.of_int symtab_size) ~sh_link:strtab_shndx ~sh_info:1
         ~sh_addralign:8L
         ~sh_entsize:(Int64.of_int symbol_entry_size)
         ~sh_name_str:".symtab";
  headers.(strtab_shndx)
    <- make_section_header ~sh_name:strtab_sh_name ~sh_type:sht_strtab
         ~sh_flags:0L ~sh_addr:0L ~sh_offset:strtab_offset
         ~sh_size:(Int64.of_int (Strtab.size strtab))
         ~sh_link:0 ~sh_info:0 ~sh_addralign:1L ~sh_entsize:0L
         ~sh_name_str:".strtab";
  headers.(shstrtab_shndx)
    <- make_section_header ~sh_name:shstrtab_sh_name ~sh_type:sht_strtab
         ~sh_flags:0L ~sh_addr:0L ~sh_offset:shstrtab_offset
         ~sh_size:(Int64.of_int (Strtab.size shstrtab))
         ~sh_link:0 ~sh_info:0 ~sh_addralign:1L ~sh_entsize:0L
         ~sh_name_str:".shstrtab";
  let header : Elf.header =
    { e_ident =
        { elf_class = 2;
          (* ELFCLASS64 *)
          elf_data = 1;
          (* ELFDATA2LSB *)
          elf_version = 1;
          elf_osabi = 0;
          elf_abiversion = 0
        };
      e_type = 1;
      (* ET_REL *)
      e_machine;
      e_version = 1;
      e_entry = 0L;
      e_phoff = 0L;
      e_shoff = sh_table_offset;
      e_flags = 0;
      e_ehsize = elf_header_size;
      e_phentsize = 0;
      e_phnum = 0;
      e_shentsize = section_header_size;
      e_shnum = total_shnum;
      e_shstrndx = shstrtab_shndx
    }
  in
  let buf =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout total_size
  in
  (* Trailing padding (where a section's sh_size exceeds its body) must be
     zero. *)
  Bigarray.Array1.fill buf 0;
  (* Write ELF header + section header table. *)
  Elf.write_elf buf header headers;
  (* Write section bodies. *)
  List.iteri
    (fun i (_, raw) ->
      let _, body_size, _, sh_offset, _, _, _, _ = layout.(i) in
      let body = S.contents_mut raw in
      let cursor = Buf.cursor buf ~at:(Int64.to_int sh_offset) in
      Buf.Write.fixed_bytes cursor body_size body)
    sections;
  (* Write symtab. *)
  let cursor = Buf.cursor buf ~at:(Int64.to_int symtab_offset) in
  (* NULL entry (24 zero bytes — already zeroed by Array1.fill, just advance
     the cursor by writing a zero entry explicitly so that subsequent entries
     are positioned correctly). *)
  Buf.Write.u32 cursor 0;
  Buf.Write.u8 cursor 0;
  Buf.Write.u8 cursor 0;
  Buf.Write.u16 cursor 0;
  Buf.Write.u64 cursor 0L;
  Buf.Write.u64 cursor 0L;
  List.iter (write_symbol cursor) symbols;
  (* Write strtab and shstrtab. *)
  Strtab.write strtab (Buf.cursor buf ~at:(Int64.to_int strtab_offset));
  Strtab.write shstrtab
    (Buf.cursor buf ~at:(Int64.to_int shstrtab_offset));
  buf
