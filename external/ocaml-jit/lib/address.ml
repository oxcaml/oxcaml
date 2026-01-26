(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* TODO: use Target_int *)
type t = nativeint

let size = Nativeint.size / 8

let placeholder = Nativeint.zero

let add_int t int = Nativeint.(add t (of_int int))

let add_int64 t int64 = Nativeint.(add t (Int64.to_nativeint int64))

let emit buf t =
  match Nativeint.size with
  | 32 -> Buffer.add_int32_ne buf (Nativeint.to_int32 t)
  | 64 -> Buffer.add_int64_ne buf (Int64.of_nativeint t)
  | _ -> assert false

let emit_string t =
  let buf = Buffer.create size in
  emit buf t;
  Buffer.contents buf

let to_int64 t = Int64.of_nativeint t

let pp fmt t = Format.fprintf fmt "%nx" t

external to_obj : t -> Obj.t = "jit_addr_to_obj"
