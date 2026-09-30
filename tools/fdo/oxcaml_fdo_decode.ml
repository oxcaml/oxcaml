(* Decodes a Linux perf profile of an OxCaml-compiled executable, sampled with
   LBR branch stacks:

   perf record -e cycles:u -j any,u -- ./prog ... oxcaml-fdo-decode -dump-trace
   -perf-data perf.data -binary ./prog

   (the runtime's single-stepping emulator produces equivalent output, consumed
   via -perf-script-output). The sampled runtime addresses are translated to the
   executable's link-time addresses through perf's mmap events, so
   position-independent executables work like any other (see [Perf_script]). The
   executable's own FDO metadata section (it must have been compiled with
   -fdo-counters; no debug info is needed) supplies the static stacks to count
   and the actions that track dynamic callers. -dump-trace prints the decoding
   decisions for each sample; -dump-metadata prints the metadata itself.
   Locations are handled hashed, so the dumps print hashes unless the executable
   was compiled with -fdo-names. *)

module Elf_info = Fdo_decode_lib.Elf_info
module Fdo_metadata_decode = Fdo_decode_lib.Fdo_metadata_decode
module Perf_script = Fdo_decode_lib.Perf_script

let usage =
  "usage: oxcaml-fdo-decode -dump-trace -perf-data <perf.data> -binary <exe>\n\
  \         [-debug-file <exe.debug>]\n\
  \       oxcaml-fdo-decode -dump-metadata [-binary <exe> | -debug-file \
   <exe.debug>]"

let perf_data = ref "perf.data"

let perf_script_output = ref ""

let binary = ref ""

let debug_file = ref ""

let dump_metadata = ref false

let dump_trace = ref false

let args =
  [ ( "-perf-data",
      Arg.Set_string perf_data,
      "<file>  perf profile to decode (default: perf.data)" );
    "-binary", Arg.Set_string binary, "<file>  the profiled executable";
    ( "-debug-file",
      Arg.Set_string debug_file,
      "<file>  read FDO metadata from this debug sidecar instead of -binary" );
    ( "-perf-script-output",
      Arg.Set_string perf_script_output,
      "<file>  read pre-captured 'perf script -F period,ip,brstack' output\n\
      \     (link-time addresses, as the runtime's emulator prints them)\n\
      \     instead of running perf" );
    ( "-dump-trace",
      Arg.Set dump_trace,
      " print the decoding decisions for each sample" );
    ( "-dump-metadata",
      Arg.Set dump_metadata,
      " print the FDO metadata of -binary (or -debug-file) and exit" ) ]

let read_metadata elf =
  let filename, elf =
    if String.equal !debug_file ""
    then !binary, elf
    else !debug_file, Elf_info.read !debug_file
  in
  match Elf_info.section_body elf ".debug_fdo_metadata" with
  | Some data -> Fdo_metadata_decode.parse data
  | None ->
    Printf.eprintf
      "%s has no .debug_fdo_metadata section; use -debug-file for split debug \
       info, or compile with -fdo-counters.\n"
      filename;
    exit 1

let decode () =
  if String.equal !binary "" || not !dump_trace
  then (
    prerr_endline usage;
    exit 2);
  let elf = Elf_info.read !binary in
  let metadata = read_metadata elf in
  let sample (sample : Perf_script.sample) =
    Format.printf "sample@.";
    Fdo_metadata_decode.process_sample
      ~trace:(Fdo_metadata_decode.trace_printer metadata Format.std_formatter)
      metadata ~code_byte:(Elf_info.code_byte elf) ~branches:sample.branches
      ~f:ignore
  in
  if String.equal !perf_script_output ""
  then
    let executable : Perf_script.executable =
      { path = !binary; address_of_offset = Elf_info.address_of_offset elf }
    in
    Perf_script.collect ~executable:(Some executable) ~perf_data:!perf_data
      ~f:sample
  else (
    (* Pre-captured output carries link-time addresses (the emulator's). *)
    if Elf_info.is_pie elf
    then
      Printf.eprintf
        "Warning: %s is position-independent; pre-captured perf script output\n\
         is used with its addresses unchanged.\n"
        !binary;
    In_channel.with_open_text !perf_script_output (fun ic ->
        Perf_script.iter_channel ic ~f:sample))

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad ("unexpected argument " ^ anon)))
    usage;
  if !dump_metadata
  then
    let filename = if String.equal !binary "" then !debug_file else !binary in
    Fdo_metadata_decode.print Format.std_formatter
      (read_metadata (Elf_info.read filename))
  else decode ()
