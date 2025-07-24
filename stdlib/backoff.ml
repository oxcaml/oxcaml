# 2 "backoff.ml"
(****************************************************************************)
(*                                                                          *)
(*                                 OxCaml                                   *)
(*                                                                          *)
(* Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>                   *)
(* Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>             *)
(* Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>                 *)
(*                                                                          *)
(* Permission to use, copy, modify, and/or distribute this software for any *)
(* purpose with or without fee is hereby granted, provided that the above   *)
(* copyright notice and this permission notice appear in all copies.        *)
(*                                                                          *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *)
(*                                                                          *)
(****************************************************************************)

type t = int

external is_runtime5 : unit -> bool @@ portable = "%runtime5"
external poll : unit -> unit @@ portable = "%poll"

let cpu_relax =
  if is_runtime5 ()
  then Domain.cpu_relax
  else poll

let recommended_domain_count =
  if is_runtime5 ()
  then Domain.recommended_domain_count
  else fun () -> 1

let single_mask = Bool.to_int (recommended_domain_count () = 1) - 1
let bits = 5
let max_wait_log = 30 (* [Random.bits] returns 30 random bits. *)
let mask = (1 lsl bits) - 1

let create ?(lower_wait_log = 4) ?(upper_wait_log = 17) () =
  assert (
    0 <= lower_wait_log
    && lower_wait_log <= upper_wait_log
    && upper_wait_log <= max_wait_log);
  (upper_wait_log lsl (bits * 2))
  lor (lower_wait_log lsl bits) lor lower_wait_log

let get_upper_wait_log backoff = backoff lsr (bits * 2)
let get_lower_wait_log backoff = (backoff lsr bits) land mask
let get_wait_log backoff = backoff land mask

let reset backoff =
  let lower_wait_log = get_lower_wait_log backoff in
  backoff land lnot mask lor lower_wait_log

(* We don't want [once] to be inlined.  This may avoid code bloat. *)
let[@inline never] once backoff =
  (* We call [Random.bits] first.  In this case this helps to reduce register
     pressure so that fewer words will be allocated from the stack. *)
  let t = Random.bits () in
  let wait_log = get_wait_log backoff in
  let wait_mask = (1 lsl wait_log) - 1 in
  (* We use a ref and a countdown while-loop (uses one variable) instead of a
     for-loop (uses two variables) to reduce register pressure.  Local ref does
     not allocate with native compiler. *)
  let t = ref (t land wait_mask land single_mask) in
  while 0 <= !t do
    cpu_relax ();
    t := !t - 1
  done;
  let upper_wait_log = get_upper_wait_log backoff in
  (* We recompute [wait_log] to reduce register pressure. *)
  let wait_log = get_wait_log backoff in
  (* [Bool.to_int] generates branchless code, this reduces branch predictor
     pressure and generates shorter code. *)
  let next_wait_log = wait_log + Bool.to_int (wait_log < upper_wait_log) in
  backoff - wait_log + next_wait_log

let default = create ()
