[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open Owee_buf

let read_magic t =
  ensure t 4 "Magic number truncated";
  let { buffer; position } = t in
  let valid =
    buffer.{position + 0} = 0x7f
    && buffer.{position + 1} = Char.code 'E'
    && buffer.{position + 2} = Char.code 'L'
    && buffer.{position + 3} = Char.code 'F'
  in
  if not valid
  then
    invalid_format
      (Printf.sprintf "No ELF magic number (found %02x %02x %02x %02x)"
         buffer.{position + 0} buffer.{position + 1} buffer.{position + 2}
         buffer.{position + 3});
  advance t 4

let write_magic t =
  ensure t 4 "Magic number truncated";
  let {buffer; position} = t in
  buffer.{position + 0} <- 0x7f;
  buffer.{position + 1} <- Char.code 'E';
  buffer.{position + 2} <- Char.code 'L';
  buffer.{position + 3} <- Char.code 'F';
  advance t 4

type identification = {
  elf_class      : u8;
  elf_data       : u8;
  elf_version    : u8;
  elf_osabi      : u8;
  elf_abiversion : u8;
}

let elfclass64 = 2

let read_identification t =
  ensure t 12 "Identification truncated";
  let elf_class      = Read.u8 t in
  let elf_data       = Read.u8 t in
  let elf_version    = Read.u8 t in
  let elf_osabi      = Read.u8 t in
  let elf_abiversion = Read.u8 t in
  if not (Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0 &&
          Read.u8 t = 0)
  then begin
    invalid_format "Incorrect padding after identification"
  end;
  if elf_class != elfclass64 then begin
    failwith "owee only supports ELFCLASS64 files"
  end;
  { elf_class; elf_data; elf_version;
    elf_osabi; elf_abiversion }

let write_identification t identification =
  ensure t 12 "Identification truncated";
  Write.u8 t identification.elf_class;
  Write.u8 t identification.elf_data;
  Write.u8 t identification.elf_version;
  Write.u8 t identification.elf_osabi;
  Write.u8 t identification.elf_abiversion;
  (* Padding *)
  Write.u8 t 0;
  Write.u8 t 0;
  Write.u8 t 0;
  Write.u8 t 0;
  Write.u8 t 0;
  Write.u8 t 0;
  Write.u8 t 0

type header = {
  e_ident     : identification;
  e_type      : u16;
  e_machine   : u16; (* Architecture type.                 *)
  e_version   : u32; (* Object file version.               *)
  e_entry     : u64; (* Entry point virtual address.       *)
  e_phoff     : u64; (* Program header table file offset.  *)
  e_shoff     : u64; (* Section header table file offset.  *)
  e_flags     : u32; (* Processor-specific flags.          *)
  e_ehsize    : u16; (* ELF header size in bytes.          *)
  e_phentsize : u16; (* Program header table entry size.   *)
  e_phnum     : u16; (* Program header table entry count.  *)
  e_shentsize : u16; (* Section header table entry size.   *)
  e_shnum     : u16; (* Section header table entry count.  *)
  e_shstrndx  : u16; (* Section header string table index. *)
}

let read_header t e_ident =
  assert (t.position = 16);
  ensure t 48 "Header truncated";
  let e_type      = Read.u16 t in
  let e_machine   = Read.u16 t in
  let e_version   = Read.u32 t in
  let e_entry     = Read.u64 t in
  let e_phoff     = Read.u64 t in
  let e_shoff     = Read.u64 t in
  let e_flags     = Read.u32 t in
  let e_ehsize    = Read.u16 t in
  let e_phentsize = Read.u16 t in
  let e_phnum     = Read.u16 t in
  let e_shentsize = Read.u16 t in
  let e_shnum     = Read.u16 t in
  let e_shstrndx  = Read.u16 t in
  { e_type; e_machine; e_version; e_entry;
    e_phoff; e_shoff; e_flags; e_ehsize;
    e_phentsize; e_phnum; e_shentsize;
    e_shnum; e_shstrndx; e_ident }


let write_header t header =
  assert (t.position = 16);
  ensure t 48 "Header truncated";
  Write.u16 t header.e_type;
  Write.u16 t header.e_machine;
  Write.u32 t header.e_version;
  Write.u64 t header.e_entry;
  Write.u64 t header.e_phoff;
  Write.u64 t header.e_shoff;
  Write.u32 t header.e_flags;
  Write.u16 t header.e_ehsize;
  Write.u16 t header.e_phentsize;
  Write.u16 t header.e_phnum;
  Write.u16 t header.e_shentsize;
  Write.u16 t header.e_shnum;
  Write.u16 t header.e_shstrndx;

(* Section header *)
type section = {
  sh_name      : u32;
  sh_type      : u32;
  sh_flags     : u64;
  sh_addr      : u64;
  sh_offset    : u64;
  sh_size      : u64;
  sh_link      : u32;
  sh_info      : u32;
  sh_addralign : u64;
  sh_entsize   : u64;
  sh_name_str : string;
}

module Section_type = struct
  type t = int

  let equal = Int.equal
  let to_u32 t = t
  let of_u32 t = t

  let sht_null = 0
  let sht_progbits = 1
  let sht_symtab = 2
  let sht_strtab = 3
  let sht_rela = 4
  let sht_symtab_shndx = 18
end

module Section_flags = struct
  type t = int64

  let to_u64 t = t
  let of_u64 t = t

  let empty = 0L
  let ( + ) = Int64.logor

  let shf_write = 0x1L
  let shf_alloc = 0x2L
  let shf_execinstr = 0x4L
  let shf_info_link = 0x40L

  let is_set flags ~flag = Int64.logand flags flag <> 0L
  let is_alloc flags = is_set flags ~flag:shf_alloc
end

let make_progbits_section ~sh_name ~sh_name_str ~sh_flags ~sh_offset ~sh_size
    ~sh_addralign =
  { sh_name;
    sh_type = Section_type.sht_progbits;
    sh_flags;
    sh_addr = 0L;
    sh_offset;
    sh_size;
    sh_link = 0;
    sh_info = 0;
    sh_addralign;
    sh_entsize = 0L;
    sh_name_str
  }

let rela_entry_size = 24

let make_rela_section ~sh_name ~sh_name_str ~sh_offset ~sh_size ~sh_link
    ~sh_info =
  { sh_name;
    sh_type = Section_type.sht_rela;
    sh_flags = Section_flags.shf_info_link;
    sh_addr = 0L;
    sh_offset;
    sh_size;
    sh_link;
    sh_info;
    sh_addralign = 8L;
    sh_entsize = Int64.of_int rela_entry_size;
    sh_name_str
  }

(* Size of an entry in SYMTAB_SHNDX section (Elf32_Word) *)
let symtab_shndx_entry_size = 4

let make_symtab_shndx_section ~sh_name ~sh_name_str ~sh_offset ~sh_size ~sh_link
    =
  { sh_name;
    sh_type = Section_type.sht_symtab_shndx;
    sh_flags = 0L;
    sh_addr = 0L;
    sh_offset;
    sh_size;
    sh_link;
    sh_info = 0;
    sh_addralign = 4L;
    sh_entsize = Int64.of_int symtab_shndx_entry_size;
    sh_name_str
  }

let read_section header t n =
  seek t ((Int64.to_int header.e_shoff) + n * header.e_shentsize);
  ensure t 64 "Shdr truncated";
  let sh_name      = Read.u32 t in
  let sh_type      = Read.u32 t in
  let sh_flags     = Read.u64 t in
  let sh_addr      = Read.u64 t in
  let sh_offset    = Read.u64 t in
  let sh_size      = Read.u64 t in
  let sh_link      = Read.u32 t in
  let sh_info      = Read.u32 t in
  let sh_addralign = Read.u64 t in
  let sh_entsize   = Read.u64 t in
  { sh_name; sh_type; sh_flags; sh_addr;
    sh_offset; sh_size; sh_link; sh_info;
    sh_addralign; sh_entsize; sh_name_str = "" }

let write_section header t n section =
  seek t ((Int64.to_int header.e_shoff) + n * header.e_shentsize);
  ensure t 64 "Shdr truncated";
  Write.u32 t section.sh_name;
  Write.u32 t section.sh_type;
  Write.u64 t section.sh_flags;
  Write.u64 t section.sh_addr;
  Write.u64 t section.sh_offset;
  Write.u64 t section.sh_size;
  Write.u32 t section.sh_link;
  Write.u32 t section.sh_info;
  Write.u64 t section.sh_addralign;
  Write.u64 t section.sh_entsize

let read_section_name shstrndx t shdr =
  let n = shdr.sh_name in
  let offset = Int64.to_int shstrndx.sh_offset + n in
  seek t offset;
  match Read.zero_string t ~maxlen:(Int64.to_int shstrndx.sh_size - n) () with
  | None ->
    invalid_format
      (Printf.sprintf "Unterminated section name at offset %d (sh_name=%d)"
         offset n)
  | Some s -> s

let write_section_name shstrndx t shdr name =
  let n = shdr.sh_name in
  seek t ((Int64.to_int shstrndx.sh_offset) + n);
  Write.zero_terminated_string t name

(* ELF extended section numbering constants *)
let shn_xindex = 0xFFFF
let shn_loreserve = 0xFF00

(* Resolve extended section numbering. Returns (actual_shnum, actual_shstrndx).
   If e_shnum is 0, the actual count is in sh_size of section header 0.
   If e_shstrndx >= SHN_LORESERVE, the actual string table index is in
   sh_link of section header 0.
   Note: if e_shoff = 0, there is no section header table at all (common in
   stripped binaries), so we return (0, 0). *)
let resolve_extended_numbering header t =
  if header.e_shoff = 0L
  then (* No section header table *)
    0, 0
  else if header.e_shnum = 0 || header.e_shstrndx >= shn_loreserve
  then
    let section0 = read_section header t 0 in
    let actual_shnum =
      if header.e_shnum = 0 then Int64.to_int section0.sh_size
      else header.e_shnum
    in
    let actual_shstrndx =
      if header.e_shstrndx >= shn_loreserve then section0.sh_link
      else header.e_shstrndx
    in
    actual_shnum, actual_shstrndx
  else header.e_shnum, header.e_shstrndx

let read_sections header t =
  let e_shnum, e_shstrndx = resolve_extended_numbering header t in
  if e_shnum = 0
  then [||]
  else
    let sections = Array.init e_shnum (read_section header t) in
    let shstrndx = sections.(e_shstrndx) in
    Array.map
      (fun s -> { s with sh_name_str = read_section_name shstrndx t s })
      sections

let write_sections header t sections =
  (* Handle extended section numbering when writing. The header passed in
     should have the actual (resolved) values for e_shnum and e_shstrndx.
     We need to write extended numbering if either exceeds SHN_LORESERVE. *)
  let num_sections = Array.length sections in
  let shstrndx_val = header.e_shstrndx in
  let needs_extended = num_sections >= shn_loreserve || shstrndx_val >= shn_loreserve in
  (* Update section 0 for extended numbering if needed *)
  if needs_extended && num_sections > 0 then begin
    let s0 = sections.(0) in
    sections.(0) <- { s0 with
      sh_size = if num_sections >= shn_loreserve then Int64.of_int num_sections else s0.sh_size;
      sh_link = if shstrndx_val >= shn_loreserve then shstrndx_val else s0.sh_link
    }
  end;
  let shstrndx = sections.(shstrndx_val) in
  Array.iteri (write_section header t) sections;
  Array.iter (fun section ->
      write_section_name shstrndx t section section.sh_name_str) sections

let read_elf buffer =
  let elf = cursor buffer in
  read_magic elf;
  let e_ident = read_identification elf in
  let raw_header = read_header elf e_ident in
  let actual_shnum, actual_shstrndx = resolve_extended_numbering raw_header elf in
  (* Return header with resolved values so callers can use them directly *)
  let header = { raw_header with e_shnum = actual_shnum; e_shstrndx = actual_shstrndx } in
  header, read_sections raw_header elf

let write_elf buffer header sections =
  let elf = cursor buffer in
  write_magic elf;
  write_identification elf header.e_ident;
  (* Write header with extended numbering markers if needed *)
  let num_sections = Array.length sections in
  let write_header_adjusted =
    if num_sections >= shn_loreserve || header.e_shstrndx >= shn_loreserve then
      { header with
        e_shnum = if num_sections >= shn_loreserve then 0 else num_sections;
        e_shstrndx = if header.e_shstrndx >= shn_loreserve then shn_xindex else header.e_shstrndx
      }
    else header
  in
  write_header elf write_header_adjusted;
  write_sections header elf sections

let section_body buffer shdr =
  Bigarray.Array1.sub buffer
    (Int64.to_int shdr.sh_offset) (Int64.to_int shdr.sh_size)

exception Found of section
let find_section sections name =
  try
    Array.iter (fun section ->
        if section.sh_name_str = name then
          raise (Found section))
      sections;
    None
  with Found section ->
    Some section

let find_section_body buf sections ~section_name =
  match find_section sections section_name with
  | None -> None
  | Some section -> Some (section_body buf section)

module String_table = struct
  type t = Owee_buf.t

  let get_string t ~index =
    if index < 0 || index >= Owee_buf.size t then
      None
    else
      let cursor = Owee_buf.cursor t ~at:index in
      Owee_buf.Read.zero_string cursor ()
end

let find_string_table buf sections =
  find_section_body buf sections ~section_name:".strtab"

let debug_line_pointers buf sections =
  { Owee_debug_line.
     debug_line_str =
       find_section_body
         buf
         sections
         ~section_name:".debug_line_str";
     debug_str =
       find_section_body
         buf
         sections
         ~section_name:".debug_str";
   }

module Symbol_table = struct
  module Symbol = struct
    type t = {
      st_name : u32;
      st_info : u8;
      st_other : u8;
      st_shndx : u16;
      st_value : Int64.t;
      st_size : Int64.t;
      symbol_end : Int64.t;
    }

    let struct_size = (32 + 8 + 8 + 16 + 64 + 64) / 8

    type type_attribute =
      | Notype
      | Object
      | Func
      | Section
      | File
      | Common
      | TLS
      | GNU_ifunc
      | Other of int

    type binding_attribute =
      | Local
      | Global
      | Weak
      | GNU_unique
      | Other of int

    type visibility =
      | Default
      | Internal
      | Hidden
      | Protected

    let name t string_table =
      String_table.get_string string_table ~index:t.st_name

    let value t = t.st_value
    let size_in_bytes t = t.st_size

    let type_attribute t =
      match t.st_info land 0xf with
      | 0 -> Notype
      | 1 -> Object
      | 2 -> Func
      | 3 -> Section
      | 4 -> File
      | 5 -> Common
      | 6 -> TLS
      | 10 -> GNU_ifunc
      | x -> Other x

    let binding_attribute t =
      match t.st_info lsr 4 with
      | 0 -> Local
      | 1 -> Global
      | 2 -> Weak
      | 10 -> GNU_unique
      | x -> Other x

    let visibility t =
      match t.st_other land 0x3 with
      | 0 -> Default
      | 1 -> Internal
      | 2 -> Hidden
      | 3 -> Protected
      | _ -> assert false

    let section_header_table_index t = t.st_shndx

    let symbol_end t = t.symbol_end
  end

  module One_table = struct
    type t = Symbol.t Owee_interval_map.t

    let extract buf ~index : Symbol.t =
      let cursor = Owee_buf.cursor buf ~at:(index * Symbol.struct_size) in
      let st_name = Owee_buf.Read.u32 cursor in
      let st_info = Owee_buf.Read.u8 cursor in
      let st_other = Owee_buf.Read.u8 cursor in
      let st_shndx = Owee_buf.Read.u16 cursor in
      let st_value = Owee_buf.Read.u64 cursor in
      let st_size = Owee_buf.Read.u64 cursor in
      let symbol_end =
        if Int64.compare st_size 0L = 0 then
          Int64.add st_value 1L
        else
          Int64.add st_value st_size
      in
      { st_name; st_info; st_other; st_shndx; st_value; st_size; symbol_end; }

    let create buf =
      let num_symbols = (Owee_buf.size buf) / Symbol.struct_size in
      Owee_interval_map.create num_symbols
        ~f:(fun index ->
            let symbol = extract buf ~index in
            Owee_interval_map.interval
              (Symbol.value symbol)
              (Symbol.symbol_end symbol)
              symbol
          )

    let symbols_enclosing_address_exn t ~address =
      List.map
        (fun (interval : Symbol.t Owee_interval_map.interval) ->
           interval.value)
        (Owee_interval_map.query t address)
  end

  type t = One_table.t list

  let create bufs =
    List.map (fun buf -> One_table.create buf) bufs

  let symbols_enclosing_address t ~address =
    List.fold_left (fun acc one_table ->
        (One_table.symbols_enclosing_address_exn one_table ~address)
          @ acc)
      [] t

  let functions_enclosing_address t ~address =
    List.filter (fun sym ->
        match Symbol.type_attribute sym with
        | Func -> true
        | Notype | Object | Section | File
        | Common | TLS | GNU_ifunc | Other _ -> false)
      (symbols_enclosing_address t ~address)

  let iter t ~f =
    let f sym = f sym.Owee_interval_map.value in
    List.iter (fun tree -> Owee_interval_map.iter tree ~f) t
end

let find_symbol_table buf sections =
  match find_section_body buf sections ~section_name:".symtab" with
  | None -> None
  | Some symtab -> Some (Symbol_table.create [symtab])

let iter_symbols ~symtab_body ~strtab_body ~f =
  let num_symbols = (size symtab_body) / Symbol_table.Symbol.struct_size in
  for i = 0 to num_symbols - 1 do
    let sym_cursor = cursor symtab_body ~at:(i * Symbol_table.Symbol.struct_size) in
    let st_name_offset = Read.u32 sym_cursor in
    let st_info = Read.u8 sym_cursor in
    let st_other = Read.u8 sym_cursor in
    let st_shndx = Read.u16 sym_cursor in
    let st_value = Read.u64 sym_cursor in
    let st_size = Read.u64 sym_cursor in
    let name =
      if st_name_offset = 0
      then ""
      else
        let name_cursor = cursor strtab_body ~at:st_name_offset in
        match Read.zero_string name_cursor () with
        | Some s -> s
        | None -> ""
    in
    f ~name ~st_info ~st_other ~st_shndx ~st_value ~st_size
  done
