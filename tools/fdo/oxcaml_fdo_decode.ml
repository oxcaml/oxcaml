(* The FDO tool for OxCaml executables. For now, it prints the FDO metadata
   section of an executable compiled with -fdo-counters (see
   [Fdo_metadata_decode]):

   oxcaml-fdo-decode -dump-metadata -binary ./prog

   The metadata may also be read from a split debug file (-debug-file).
   Locations are handled hashed, so the dump prints hashes unless the executable
   was compiled with -fdo-names. *)

module Elf_info = Fdo_decode_lib.Elf_info
module Fdo_metadata_decode = Fdo_decode_lib.Fdo_metadata_decode

let usage =
  "usage: oxcaml-fdo-decode -dump-metadata [-binary <exe> | -debug-file \
   <exe.debug>]"

let binary = ref ""

let debug_file = ref ""

let dump_metadata = ref false

let args =
  [ "-binary", Arg.Set_string binary, "<file>  the executable";
    ( "-debug-file",
      Arg.Set_string debug_file,
      "<file>  read FDO metadata from this debug sidecar instead of -binary" );
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

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad ("unexpected argument " ^ anon)))
    usage;
  let filename = if String.equal !binary "" then !debug_file else !binary in
  if (not !dump_metadata) || String.equal filename ""
  then (
    prerr_endline usage;
    exit 2);
  Fdo_metadata_decode.print Format.std_formatter
    (read_metadata (Elf_info.read filename))
