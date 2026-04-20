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

(* CR mshinwell: This file needs to be code reviewed *)

module MOF = Measure_object_files

type error =
  | Linker_error of
      { partition_index : int;
        exit_code : int;
        files : string list
      }

exception Error of error

let report_error ppf = function
  | Linker_error { partition_index; exit_code; files } ->
    Format_doc.fprintf ppf
      "@[<v>Dissector: partial link of partition %d failed with exit code %d@,\
       Files in partition:@,\
      \  @[<v>%a@]@]"
      partition_index exit_code
      (Format_doc.pp_print_list ~pp_sep:Format_doc.pp_print_cut
         Format_doc.pp_print_string)
      files

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)

let write_response_file ~filename files =
  let oc = open_out filename in
  List.iter
    (fun entry ->
      output_string oc (MOF.File_size.filename entry);
      output_char oc '\n')
    files;
  close_out oc

(* Build the linker command for one partition. *)
let partition_cmd ~output_file ~response_file =
  (* Config.native_pack_linker is something like "ld -r -o " *)
  Printf.sprintf "%s%s --whole-archive @%s --no-whole-archive"
    Config.native_pack_linker
    (Filename.quote output_file)
    (Filename.quote response_file)

let link_one_partition ~temp_dir ~partition_index partition =
  let response_file =
    Filename.concat temp_dir (Printf.sprintf "partition%d.txt" partition_index)
  in
  let output_file =
    Filename.concat temp_dir (Printf.sprintf "partition%d.o" partition_index)
  in
  write_response_file ~filename:response_file (Partition.files partition);
  Misc.try_finally
    (fun () ->
      let cmd = partition_cmd ~output_file ~response_file in
      let exit_code = Ccomp.command cmd in
      (if exit_code <> 0
       then
         let files =
           List.map MOF.File_size.filename (Partition.files partition)
         in
         raise (Error (Linker_error { partition_index; exit_code; files })));
      Partition.Linked.create ~partition ~linked_object:output_file)
    ~always:(fun () -> Misc.remove_file response_file)

(* Write a Makefile that links all partitions as independent targets so that
   make -j can run them concurrently. The recipes use '@' to suppress echoing
   since we print the commands ourselves when -verbose is set. *)
let write_parallel_makefile ~filename jobs =
  let oc = open_out filename in
  (try
     output_string oc "all:";
     List.iter
       (fun (output_file, _) ->
         output_char oc ' ';
         output_string oc output_file)
       jobs;
     output_char oc '\n';
     List.iter
       (fun (output_file, cmd) ->
         Printf.fprintf oc "\n%s:\n\t@%s\n" output_file cmd)
       jobs
   with exn ->
     close_out oc;
     raise exn);
  close_out oc

let link_partitions ~temp_dir partitions =
  match partitions with
  | [] -> []
  | [partition] ->
    (* Single partition: no benefit from the make -j overhead. *)
    [link_one_partition ~temp_dir ~partition_index:0 partition]
  | _ ->
    (* Multiple partitions: prepare response files and commands, then run all
       linker invocations concurrently via make -j. *)
    let jobs =
      List.mapi
        (fun partition_index partition ->
          let response_file =
            Filename.concat temp_dir
              (Printf.sprintf "partition%d.txt" partition_index)
          in
          let output_file =
            Filename.concat temp_dir
              (Printf.sprintf "partition%d.o" partition_index)
          in
          write_response_file ~filename:response_file
            (Partition.files partition);
          let cmd = partition_cmd ~output_file ~response_file in
          if !Clflags.verbose then Printf.eprintf "+ %s\n%!" cmd;
          partition_index, cmd, output_file, response_file, partition)
        partitions
    in
    let makefile = Filename.concat temp_dir "partitions.mk" in
    Misc.try_finally
      (fun () ->
        write_parallel_makefile ~filename:makefile
          (List.map (fun (_, cmd, out, _, _) -> out, cmd) jobs);
        let make_cmd =
          Printf.sprintf "make -j -f %s" (Filename.quote makefile)
        in
        let exit_code = Ccomp.command make_cmd in
        (if exit_code <> 0
         then
           (* Identify the first partition whose output file is absent. *)
           let failed_opt =
             List.find_opt
               (fun (_, _, output_file, _, _) ->
                 not (Sys.file_exists output_file))
               jobs
           in
           let partition_index, _, _, _, partition =
             match failed_opt with
             | Some job -> job
             | None ->
               (* All outputs exist but make still reported failure; fall back
                  to reporting partition 0. *)
               List.hd jobs
           in
           let files =
             List.map MOF.File_size.filename (Partition.files partition)
           in
           raise (Error (Linker_error { partition_index; exit_code; files })));
        List.map
          (fun (_, _, output_file, _, partition) ->
            Partition.Linked.create ~partition ~linked_object:output_file)
          jobs)
      ~always:(fun () ->
        Misc.remove_file makefile;
        List.iter
          (fun (_, _, _, response_file, _) -> Misc.remove_file response_file)
          jobs)
