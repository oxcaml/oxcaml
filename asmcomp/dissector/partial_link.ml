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
  (* The linker tokenizes the response file like a shell: whitespace separates
     arguments, and quotes and backslashes are special. Filenames can contain
     such characters (unit names may contain '), so they are quoted with
     [Filename.quote], as [Ccomp.build_response_file] does. *)
  (* The -u flags must precede the files: the linker only extracts an archive
     member if its symbol is undefined when the archive is scanned, and it never
     rescans an archive. *)
  List.iter
    (fun entry ->
      List.iter
        (fun symbol ->
          Printf.fprintf oc "-u %s\n" (Asm_targets.Asm_symbol.encode symbol))
        (MOF.File_size.required_symbols entry))
    files;
  List.iter
    (fun entry ->
      Printf.fprintf oc "%s\n" (Filename.quote (MOF.File_size.filename entry)))
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

  (* Sends SIGTERM to the child process; does not wait for it. Must not be
     called after [wait] has returned: the pid may have been recycled. *)
  val kill : t -> unit
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
      Printf.sprintf "%s%s @%s" Config.native_pack_linker
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

  let kill { unix; pid; _ } =
    let module Unix = (val unix) in
    Unix.kill pid Sys.sigterm
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

(* A window of concurrently running partial links over the partitions. Links are
   started in partition order and their results are returned in partition
   order. *)
module Link_window = struct
  type t =
    { bound : Misc.Maybe_bounded.t;
      mutable in_flight : Link_job.t list; (* oldest link first *)
      mutable unstarted : (int * Partition.t) list;
      start_link : int * Partition.t -> Link_job.t;
      wait_link : Link_job.t -> Partition.Linked.t
    }

  (* No links are started yet; see [start_linking]. *)
  let create ~bound ~start_link ~wait_link partitions =
    { bound;
      in_flight = [];
      unstarted =
        List.mapi (fun partition_index p -> partition_index, p) partitions;
      start_link;
      wait_link
    }

  (* Starts the links of the first [bound] partitions. *)
  let start_linking t =
    let first_window, unstarted =
      List.partition
        (fun (partition_index, _) ->
          Misc.Maybe_bounded.is_in_bounds partition_index t.bound)
        t.unstarted
    in
    t.unstarted <- unstarted;
    List.iter
      (fun next -> t.in_flight <- t.in_flight @ [t.start_link next])
      first_window

  (* Waits for the oldest in-flight link, then refills the window by starting
     the next unstarted link. The refill happens only after the wait returns, so
     at most [bound] children run at any time. [None] once every partition has
     been linked. The oldest link is popped before the wait, which reaps it
     whether it returns or raises; [kill_and_reap_in_flight] must not see it. *)
  let next_linked t =
    match t.in_flight with
    | [] -> None
    | oldest :: in_flight ->
      t.in_flight <- in_flight;
      let linked = t.wait_link oldest in
      (match t.unstarted with
      | [] -> ()
      | next :: unstarted ->
        t.unstarted <- unstarted;
        t.in_flight <- t.in_flight @ [t.start_link next]);
      Some linked

  (* Best-effort cleanup when linking is aborted: kill (SIGTERM) and reap the
     in-flight children, so that none outlives the failing compilation, and
     remove their response files. *)
  let kill_and_reap_in_flight t =
    let jobs = t.in_flight in
    t.in_flight <- [];
    List.iter (fun job -> try Link_job.kill job with _ -> ()) jobs;
    List.iter
      (fun job ->
        (match Link_job.wait job with
        | (_ : Link_job.exit_condition) -> ()
        | exception _ -> ());
        Misc.remove_file (Link_job.response_file job))
      jobs

  (* [fold_linked ~init ~f window] starts the links of [window], whose links
     must not have been started yet, and folds [f] over the linked partitions in
     partition order. *)
  let fold_linked ~init ~f window =
    start_linking window;
    let rec aux acc =
      match next_linked window with
      | None -> acc
      | Some linked -> aux (f acc linked)
    in
    aux init
end

let link_all unix ~temp_dir ~max_parallelism ~init ~f partitions =
  let start_link (partition_index, partition) =
    start unix ~temp_dir ~partition_index partition
  in
  let wait_link job =
    Profile.record_call ~accumulate:true "dissector/partial_link_wait"
      (fun () -> wait job)
  in
  let window =
    Link_window.create ~bound:max_parallelism ~start_link ~wait_link partitions
  in
  Misc.try_finally
    ~always:(fun () -> Link_window.kill_and_reap_in_flight window)
    (fun () -> Link_window.fold_linked ~init ~f window)
