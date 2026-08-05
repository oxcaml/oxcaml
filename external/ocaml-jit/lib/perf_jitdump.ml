(* Linux [perf] jitdump writer. See .mli for protocol overview.

   Format reference:
   linux/tools/perf/Documentation/jitdump-specification.txt *)

external is_linux : unit -> bool = "caml_perf_jitdump_is_linux" [@@noalloc]

external clock_monotonic_ns : unit -> int64
  = "caml_perf_jitdump_clock_monotonic"

(* Generates a single PROT_EXEC mmap event then unmaps. The kernel records
   the event at mmap-time, so the mapping itself does not need to persist. *)
external mmap_exec_marker : Unix.file_descr -> int -> unit
  = "caml_perf_jitdump_mmap_marker"

(* Six u32s (magic, version, total_size, elf_mach, pad1, pid) plus two u64s
   (timestamp, flags) = 24 + 16 = 40 bytes. *)
let header_size = 40

let record_prefix_size = 16

let code_load_fixed_body_size = 40

let id_jit_code_load = 0

let magic = 0x4A695444 (* "JiTD" in native endian *)

let version = 1

type handle =
  { fd : Unix.file_descr;
    pid : int;
    mutable code_index : int;
    mutable disabled : bool
  }

(* Little-endian byte writers. The jitdump format is "native endian" with
   readers detecting via the magic; on Linux x86-64 and arm64 native is
   little-endian, which is what we emit. *)

let buf_u32 buf v =
  Buffer.add_char buf (Char.chr (v land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xff))

let buf_u64_int64 buf v =
  for i = 0 to 7 do
    let b =
      Int64.logand (Int64.shift_right_logical v (i * 8)) 0xffL |> Int64.to_int
    in
    Buffer.add_char buf (Char.chr b)
  done

let buf_u64_int buf v = buf_u64_int64 buf (Int64.of_int v)

(* Write all of [bytes] to [fd], retrying short writes and EINTR. *)
let rec write_all fd buf off len =
  if len = 0
  then ()
  else
    match Unix.write fd buf off len with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd buf off len
    | 0 -> failwith "perf_jitdump: write returned 0"
    | n -> write_all fd buf (off + n) (len - n)

let dump_dir () =
  match Sys.getenv_opt "JITDUMP_DIR" with
  | Some d -> d
  | None -> (
    match Sys.getenv_opt "HOME" with
    | Some h -> Filename.concat (Filename.concat h ".debug") "jit"
    | None -> (
      match Sys.getenv_opt "TMPDIR" with Some t -> t | None -> "/tmp"))

let mkdir_p dir =
  let rec go dir =
    if Sys.file_exists dir
    then ()
    else (
      go (Filename.dirname dir);
      try Unix.mkdir dir 0o755
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  go dir

let build_header ~elf_mach ~pid =
  let buf = Buffer.create header_size in
  buf_u32 buf magic;
  buf_u32 buf version;
  buf_u32 buf header_size;
  buf_u32 buf elf_mach;
  buf_u32 buf 0 (* pad1 *);
  buf_u32 buf pid;
  buf_u64_int64 buf (clock_monotonic_ns ()) (* timestamp *);
  buf_u64_int64 buf 0L (* flags *);
  assert (Buffer.length buf = header_size);
  Buffer.to_bytes buf

let init ~elf_mach =
  if not (is_linux ())
  then None
  else if elf_mach = 0
  then None
  else
    try
      let dir = dump_dir () in
      mkdir_p dir;
      let pid = Unix.getpid () in
      let path = Filename.concat dir (Printf.sprintf "jit-%d.dump" pid) in
      let fd =
        Unix.openfile path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR ] 0o644
      in
      let header = build_header ~elf_mach ~pid in
      write_all fd header 0 header_size;
      mmap_exec_marker fd header_size;
      Some { fd; pid; code_index = 0; disabled = false }
    with _ -> None

let emit_code_load h ~name ~code_addr ~code_size ~code =
  if h.disabled
  then ()
  else if String.length code <> code_size
  then h.disabled <- true (* mismatch — refuse to emit corrupt records *)
  else
    let idx = h.code_index in
    h.code_index <- idx + 1;
    let name_len_with_nul = String.length name + 1 in
    let total =
      record_prefix_size + code_load_fixed_body_size + name_len_with_nul
      + code_size
    in
    let buf = Buffer.create total in
    (* Record prefix *)
    buf_u32 buf id_jit_code_load;
    buf_u32 buf total;
    buf_u64_int64 buf (clock_monotonic_ns ());
    (* JIT_CODE_LOAD body *)
    buf_u32 buf h.pid;
    buf_u32 buf h.pid (* tid; ocaml-jit is single-threaded *);
    buf_u64_int64 buf code_addr (* vma *);
    buf_u64_int64 buf code_addr (* code_addr *);
    buf_u64_int buf code_size;
    buf_u64_int buf idx;
    Buffer.add_string buf name;
    Buffer.add_char buf '\x00';
    Buffer.add_string buf code;
    let bytes = Buffer.to_bytes buf in
    let len = Bytes.length bytes in
    assert (len = total);
    try write_all h.fd bytes 0 len
    with _ ->
      (* If a write fails the dump is now truncated and unparseable; stop
         emitting further records to avoid making it worse. *)
      h.disabled <- true
