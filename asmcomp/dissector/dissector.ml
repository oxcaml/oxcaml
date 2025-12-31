(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

module MOF = Measure_object_files

type error =
  | Measure_error of MOF.error
  | Partition_error of Partition_object_files.error
  | Partial_link_error of Partial_link.error
  | Nodynlink_incompatible

exception Error of error

let report_error ppf = function
  | Measure_error err -> MOF.report_error ppf err
  | Partition_error err -> Partition_object_files.report_error ppf err
  | Partial_link_error err -> Partial_link.report_error ppf err
  | Nodynlink_incompatible ->
    Format.fprintf ppf
      "The dissector is incompatible with -nodynlink.@ The -nodynlink flag \
       must not be used when using -dissector."

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

let log = Dissector_log.log

let log_verbose = Dissector_log.log_verbose

(* Extract existing linker script from -ccopt arguments. Looks for patterns like
   "-Wl,-T,<path>" or "-Wl,--script=<path>". Returns the first match found, or
   None if no linker script is specified. *)
let extract_linker_script_from_ccopts ccopts =
  let extract_from_wl_arg arg =
    (* Handle -Wl,... arguments which are comma-separated *)
    if String.starts_with ~prefix:"-Wl," arg
    then
      let parts = String.split_on_char ',' arg in
      (* Look for -T followed by path, or --script=path *)
      let rec find_script = function
        | [] -> None
        | "-T" :: path :: _ -> Some path
        | part :: rest ->
          if String.starts_with ~prefix:"--script=" part
          then Some (String.sub part 9 (String.length part - 9))
          else find_script rest
      in
      find_script parts
    else None
  in
  List.find_map extract_from_wl_arg ccopts

module Result = struct
  type t =
    { linked_partitions : Partition.Linked.t list;
      passthrough_files : string list;
      linker_script : string
    }

  let linked_partitions r = r.linked_partitions

  let passthrough_files r = r.passthrough_files

  let linker_script r = r.linker_script
end

let dump_sizes file_sizes =
  Printf.eprintf "Dissector: allocated section sizes:\n";
  let total =
    List.fold_left
      (fun acc entry ->
        Printf.eprintf "  %12Ld  %s\n" (MOF.File_size.size entry)
          (MOF.File_size.filename entry);
        Int64.add acc (MOF.File_size.size entry))
      0L file_sizes
  in
  Printf.eprintf "  %12Ld  TOTAL\n%!" total

let run ~(unix : (module Compiler_owee.Unix_intf.S)) ~temp_dir ~ml_objfiles
    ~startup_obj ~ccobjs ~runtime_libs ~cached_genfns =
  (* Check that -nodynlink was not used. The dissector requires GOTPCREL
     relocations which are only emitted when dlcode=true. *)
  if not !Clflags.dlcode then raise (Error Nodynlink_incompatible);
  (* Check that we're targeting Linux *)
  (match Target_system.system () with
  | Linux -> ()
  | Windows _ | MacOS_like | FreeBSD | NetBSD | OpenBSD | Generic_BSD | Solaris
  | Dragonfly | GNU | BeOS | Unknown ->
    Misc.fatal_error "The dissector pass is only supported on Linux targets");
  (* Check that we're running on a 64-bit architecture. The dissector parses
     ELF64 files and uses int64 arithmetic extensively. *)
  if Sys.word_size <> 64
  then Misc.fatal_error "The dissector requires a 64-bit host architecture";
  (* Collect files to analyze for partitioning. C stubs and runtime libraries
     are passthrough (passed directly to final linker, not partially linked). *)
  let files_to_measure =
    List.map (fun f -> f, MOF.OCaml) ml_objfiles
    @ [startup_obj, MOF.Startup]
    @ match cached_genfns with None -> [] | Some f -> [f, MOF.Cached_genfns]
  in
  let passthrough_files = ccobjs @ runtime_libs in
  (* Measure file sizes for partitioning *)
  let file_sizes =
    try MOF.measure_files unix ~files:files_to_measure
    with MOF.Error err -> raise (Error (Measure_error err))
  in
  (* Dump sizes if requested *)
  if !Clflags.ddissector_sizes then dump_sizes file_sizes;
  (* Compute partition threshold *)
  let threshold =
    match !Clflags.dissector_partition_size with
    | Some gb -> Partition_object_files.bytes_of_gb gb
    | None -> Partition_object_files.default_partition_size
  in
  (* Partition files *)
  let partitions =
    try Partition_object_files.partition_files ~threshold file_sizes
    with Partition_object_files.Error err ->
      raise (Error (Partition_error err))
  in
  let total =
    List.fold_left
      (fun acc entry -> Int64.add acc (MOF.File_size.size entry))
      0L file_sizes
  in
  log "total allocated section size = %Ld bytes" total;
  log "partitioned into %d partition(s)" (List.length partitions);
  log "%d passthrough file(s) (will bypass partial linking)"
    (List.length passthrough_files);
  let linked_partitions =
    try Partial_link.link_partitions ~temp_dir partitions
    with Partial_link.Error err -> raise (Error (Partial_link_error err))
  in
  log "partially linked %d partition(s)" (List.length linked_partitions);
  let relocations =
    Extract_relocations.extract_from_linked_partitions unix linked_partitions
  in
  log "found %d PLT relocations and %d GOT relocations"
    (List.length (Extract_relocations.convert_to_plt relocations))
    (List.length (Extract_relocations.convert_to_got relocations));
  List.iter
    (fun linked ->
      let kind = Partition.kind (Partition.Linked.partition linked) in
      let prefix = Partition.symbol_prefix kind in
      let igot_and_iplt = Build_igot_and_iplt.build ~prefix relocations in
      log "built IGOT with %d entries, IPLT with %d entries (prefix=%s)"
        (List.length (Igot.entries (Build_igot_and_iplt.igot igot_and_iplt)))
        (List.length (Iplt.entries (Build_igot_and_iplt.iplt igot_and_iplt)))
        prefix;
      let input_file = Partition.Linked.linked_object linked in
      let output_file = input_file ^ ".rewritten" in
      Rewrite_sections.rewrite unix ~input_file ~output_file
        ~partition_kind:kind ~igot_and_iplt ~relocations;
      log "rewrote %s -> %s" input_file output_file)
    linked_partitions;
  let existing_script = extract_linker_script_from_ccopts !Clflags.all_ccopts in
  (match existing_script with
  | Some path -> log "found existing linker script: %s" path
  | None -> ());
  let linker_script = Filename.concat temp_dir "linker.script" in
  Linker_script.write ~output_file:linker_script ~existing_script
    ~partitions:linked_partitions;
  log "generated linker script: %s" linker_script;
  { Result.linked_partitions; passthrough_files; linker_script }
