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

(* A linker invocation running in a child process, partially linking one
   partition. *)
module Link_job : sig
  type t

  val partition : t -> Partition.t

  val partition_index : t -> int

  val response_file : t -> string

  val output_file : t -> string

  (* Starts the linker in a child process, linking the files listed in
     [response_file] into [output_file]. The response file must already have
     been written. *)
  val start :
    (module Compiler_owee.Unix_intf.S) ->
    partition:Partition.t ->
    partition_index:int ->
    response_file:string ->
    output_file:string ->
    t

  (* The exit condition of the child process running the linker command. *)
  type exit_condition =
    | Success
    | Failed of { exit_code : int } (* nonzero *)
    | Could_not_run of { cmd : string }
      (* the shell exits with code 127, as for [Sys.command] *)
    | Killed_or_stopped_by_signal of { signal : int }

  (* Waits for the child process to exit. *)
  val wait : t -> exit_condition
end = struct
  type t =
    { unix : (module Compiler_owee.Unix_intf.S);
      pid : int;
      cmd : string;
      partition : Partition.t;
      partition_index : int;
      response_file : string;
      output_file : string
    }

  let partition t = t.partition

  let partition_index t = t.partition_index

  let response_file t = t.response_file

  let output_file t = t.output_file

  let start (unix : (module Compiler_owee.Unix_intf.S)) ~partition
      ~partition_index ~response_file ~output_file =
    (* Config.native_pack_linker is something like "ld -r -o " *)
    let cmd =
      Printf.sprintf "%s%s --whole-archive @%s --no-whole-archive"
        Config.native_pack_linker
        (Filename.quote output_file)
        (Filename.quote response_file)
    in
    let module Unix = (val unix) in
    let spawn prog argv =
      Unix.create_process prog argv Unix.stdin Unix.stdout Unix.stderr
    in
    let pid = Ccomp.start_command ~spawn cmd in
    { unix; pid; cmd; partition; partition_index; response_file; output_file }

  type exit_condition =
    | Success
    | Failed of { exit_code : int } (* nonzero *)
    | Could_not_run of { cmd : string }
      (* the shell exits with code 127, as for [Sys.command] *)
    | Killed_or_stopped_by_signal of { signal : int }

  let wait { unix; pid; cmd; _ } =
    let module Unix = (val unix) in
    match snd (Unix.waitpid [] pid) with
    | WEXITED 0 -> Success
    | WEXITED 127 -> Could_not_run { cmd }
    | WEXITED exit_code -> Failed { exit_code }
    | WSIGNALED signal | WSTOPPED signal ->
      Killed_or_stopped_by_signal { signal }
end

let start unix ~temp_dir ~partition_index partition =
  let response_file =
    Filename.concat temp_dir (Printf.sprintf "partition%d.txt" partition_index)
  in
  let output_file =
    Filename.concat temp_dir (Printf.sprintf "partition%d.o" partition_index)
  in
  write_response_file ~filename:response_file (Partition.files partition);
  Link_job.start unix ~partition ~partition_index ~response_file ~output_file

let wait job =
  Misc.try_finally
    (fun () ->
      let partition = Link_job.partition job in
      let partition_index = Link_job.partition_index job in
      match Link_job.wait job with
      | Success ->
        Partition.Linked.create ~partition
          ~linked_object:(Link_job.output_file job)
      | Could_not_run { cmd } ->
        (* As for [Ccomp.command] *)
        raise (Sys_error cmd)
      | Failed { exit_code } ->
        let files =
          List.map MOF.File_size.filename (Partition.files partition)
        in
        raise (Error (Linker_error { partition_index; exit_code; files }))
      | Killed_or_stopped_by_signal { signal } ->
        (* [signal] uses OCaml's signal numbering (see [Sys.sigabrt] etc.) *)
        Misc.fatal_errorf
          "Dissector: partial link of partition %d was killed or stopped by \
           signal %d"
          partition_index signal)
    ~always:(fun () -> Misc.remove_file (Link_job.response_file job))

let link_one_partition unix ~temp_dir ~partition_index partition =
  wait (start unix ~temp_dir ~partition_index partition)

let link_partitions unix ~temp_dir partitions =
  List.mapi
    (fun partition_index partition ->
      link_one_partition unix ~temp_dir ~partition_index partition)
    partitions
