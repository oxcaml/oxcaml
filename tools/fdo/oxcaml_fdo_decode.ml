(* Decodes a Linux perf profile of an OxCaml-compiled executable into an FDO
   profile (see [Source_position_profile]):

   perf record -e cycles:u -j any,u -- ./prog ... oxcaml-fdo-decode -perf-data
   perf.data -binary ./prog -o prog.fdo

   The profile must have been recorded with LBR branch stacks (-j any,u; the
   runtime's single-stepping emulator produces equivalent output, consumed via
   -perf-script-output). The sampled runtime addresses are translated to the
   executable's link-time addresses through perf's mmap events, so
   position-independent executables work like any other (see [Perf_script]). The
   executable's own FDO metadata section (it must have been compiled with
   -fdo-counters or -fdo-profile; no debug info is needed) supplies the static
   stacks to count and the actions that track dynamic callers. Counters are
   handled hashed, so -dump prints hashes unless names are present; the
   compiler's -dfdo shows the same counts by position. *)

module P = Source_position_profile
module Elf_info = Fdo_decode_lib.Elf_info
module Fdo_metadata_decode = Fdo_decode_lib.Fdo_metadata_decode
module Perf_script = Fdo_decode_lib.Perf_script

let usage =
  "usage: oxcaml-fdo-decode -perf-data <perf.data> -binary <exe> -o <out>\n\
  \         [-debug-file <exe.debug>]\n\
  \       oxcaml-fdo-decode -dump <profile> [-binary <exe> | -debug-file \
   <exe.debug>]"

let perf_data = ref "perf.data"

let perf_script_output = ref ""

let binary = ref ""

let debug_file = ref ""

let output = ref ""

let dump = ref ""

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
    "-o", Arg.Set_string output, "<file>  profile file to write";
    ( "-perf-script-output",
      Arg.Set_string perf_script_output,
      "<file>  read pre-captured 'perf script -F period,ip,brstack' output\n\
      \     (link-time addresses, as the runtime's emulator prints them)\n\
      \     instead of running perf" );
    ( "-dump",
      Arg.Set_string dump,
      "<file>  print the given profile and exit: its trie of counters, by\n\
      \     hash, or by name where -binary or -debug-file contains names\n\
      \     emitted with -fdo-names" );
    ( "-dump-trace",
      Arg.Set dump_trace,
      " print the decoding decisions for each sample instead of writing a\n\
      \     profile" );
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

let dump_profile filename =
  let p = P.load ~filename in
  let names =
    let filename = if String.equal !binary "" then !debug_file else !binary in
    if String.equal filename ""
    then Fdo_counter.Hash.Tbl.create 0
    else Fdo_metadata_decode.names (read_metadata (Elf_info.read filename))
  in
  let name (hash : Fdo_counter.Hash.t) =
    match Fdo_counter.Hash.Tbl.find_opt names hash with
    | Some name -> name
    | None -> Printf.sprintf "%08lx" (hash :> int32)
  in
  (* A node's count, and how much of it was recorded with no further context
     when that is not all of it (leaves) or none of it. *)
  P.iter p ~f:(fun ~hash ~depth ~count ~ending ->
      Printf.printf "%s%s: %Ld%s\n"
        (String.make (2 * depth) ' ')
        (name hash) count
        (if Int64.equal ending 0L || Int64.equal ending count
         then ""
         else Printf.sprintf " (%Ld without further context)" ending));
  print_endline "bodies:";
  P.iter_bodies p
    ~f:(fun ~hash ~(function_body_hash : Fdo_counter.Function_body_hash.t) ->
      Printf.printf "  %s: %08lx\n" (name hash) (function_body_hash :> int32));
  print_endline "call targets:";
  let last_callsite = ref None in
  P.iter_call_targets p ~f:(fun ~callsite ~callee ->
      if
        not (Option.equal Fdo_counter.Hash.equal !last_callsite (Some callsite))
      then (
        last_callsite := Some callsite;
        Printf.printf "  %s\n" (name callsite));
      Printf.printf "    %s\n" (name callee))

let decode () =
  if String.equal !binary "" || ((not !dump_trace) && String.equal !output "")
  then (
    prerr_endline usage;
    exit 2);
  let elf = Elf_info.read !binary in
  let metadata = read_metadata elf in
  let writer = P.Writer.create () in
  Fdo_counter.Hash.Tbl.iter
    (fun hash function_body_hash ->
      P.Writer.add_body writer ~hash ~function_body_hash)
    (Fdo_metadata_decode.bodies metadata);
  let total = ref 0L in
  let sample (sample : Perf_script.sample) =
    total
      := Int64.add !total
           (Int64.mul sample.count (Int64.of_int (List.length sample.branches)));
    let trace =
      if !dump_trace
      then (
        Format.printf "sample@.";
        Some (Fdo_metadata_decode.trace_printer metadata Format.std_formatter))
      else None
    in
    Fdo_metadata_decode.process_sample ?trace metadata
      ~code_byte:(Elf_info.code_byte elf) ~branches:sample.branches
      ~f:(fun hashes ->
        P.Writer.add_hashed_stack writer ~hashes ~count:sample.count)
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
        Perf_script.iter_channel ic ~f:sample));
  if not !dump_trace
  then (
    P.Writer.write writer ~filename:!output;
    Printf.eprintf "%s: %Ld taken branches observed; wrote %s\n" !binary !total
      !output)

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad ("unexpected argument " ^ anon)))
    usage;
  if !dump_metadata
  then
    let filename = if String.equal !binary "" then !debug_file else !binary in
    Fdo_metadata_decode.print Format.std_formatter
      (read_metadata (Elf_info.read filename))
  else if not (String.equal !dump "")
  then dump_profile !dump
  else decode ()
