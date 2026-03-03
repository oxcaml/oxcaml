(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 *                         Basile Clément, OCamlPro                           *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 OCamlPro                                                *
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

module Cmd : sig
  (** This module is a tiny version of the [Cmd] interface from
      {{:https://erratique.ch/software/bos} Daniel Bünzli's [Bos] library},
      under the ISC license. *)

  (* Copyright (c) 2016 The bos programmers

     Permission to use, copy, modify, and/or distribute this software for any
     purpose with or without fee is hereby granted, provided that the above
     copyright notice and this permission notice appear in all copies.

     THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
     WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
     MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
     SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
     WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
     ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
     IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

  type t

  val v : string -> t

  val empty : t

  val ( % ) : t -> string -> t

  val ( %% ) : t -> t -> t

  val if' : bool -> t -> t

  val of_list : ?slip:string -> string list -> t

  val to_list : t -> string list

  val to_string : t -> string

  val get_line_tool : t -> string
end = struct
  type t = string list
  (* in reverse order *)

  let v cmd = [cmd]

  let empty = []

  let get_line_tool line =
    match List.rev line with
    | tool :: _ -> tool
    | _ -> invalid_arg "get_line_tool: command line is empty"

  let ( % ) line arg = arg :: line

  let ( %% ) line args = args @ line

  let if' bool line = if bool then line else empty

  let of_list ?slip args =
    match slip with
    | None -> List.rev args
    | Some slip ->
      let rec loop ~slip acc = function
        | [] -> acc
        | arg :: args -> loop ~slip (arg :: slip :: acc) args
      in
      loop ~slip [] args

  let to_list line = List.rev line

  let to_string line = String.concat " " (List.rev_map Filename.quote line)
end

(** {2 Process execution} *)

let rec waitpid_non_intr pid =
  try snd (Unix.waitpid [] pid)
  with Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let run_no_capture prog args =
  let pid = Unix.create_process prog args Unix.stdin Unix.stdout Unix.stderr in
  match waitpid_non_intr pid with
  | WEXITED 0 -> Ok ()
  | WEXITED _exitcode -> Error ()
  | WSIGNALED _signum -> Error ()
  | WSTOPPED _ ->
    failwith "internal error: waitpid returned WSTOPPED without WUNTRACED"

let run_with_capture prog args =
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let stderr_write = Unix.dup ~cloexec:true stdout_write in
  let pid =
    Unix.create_process prog args stdin_read stdout_write stderr_write
  in
  Unix.close stdin_read;
  Unix.close stdin_write;
  Unix.close stdout_write;
  Unix.close stderr_write;
  let rec loop buf scratch =
    let n = Unix.read stdout_read scratch 0 (Bytes.length scratch) in
    if n = 0
    then (
      Unix.close stdout_read;
      Buffer.contents buf)
    else (
      Buffer.add_subbytes buf scratch 0 n;
      loop buf scratch)
  in
  let stdout = loop (Buffer.create 1024) (Bytes.create 1024) in
  match waitpid_non_intr pid with
  | WEXITED 0 -> Ok stdout
  | WEXITED _exitcode -> Error stdout
  | WSIGNALED _signum -> Error stdout
  | WSTOPPED _ ->
    failwith "internal error: waitpid returned WSTOPPED without WUNTRACED"

let eval_cmd cmd =
  let args = Cmd.to_list cmd in
  run_no_capture (List.hd args) (Array.of_list args)

let eval_cmd_with_capture cmd =
  let args = Cmd.to_list cmd in
  run_with_capture (List.hd args) (Array.of_list args)
