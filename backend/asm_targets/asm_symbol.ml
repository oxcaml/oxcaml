(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

open! Int_replace_polymorphic_compare

let symbol_prefix () =
  (* CR mshinwell: needs checking *)
  match Target_system.architecture () with
  | IA32 | X86_64 | AArch64 -> (
    match Target_system.derived_system () with
    | Linux | Win32 | Win64 | MinGW_32 | MinGW_64 | Cygwin | FreeBSD | NetBSD
    | OpenBSD | Generic_BSD | Solaris | BeOS | GNU | Dragonfly | Unknown ->
      "" (* checked ok. *)
    | MacOS_like -> "_" (* checked ok. *))
  | ARM | POWER | Z | Riscv -> ""

let should_be_escaped = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> false
  | _c -> true

type visibility =
  | Global
  | Local

type t =
  { name : string;
    already_encoded : bool;
    visibility : visibility
  }

let output chan { name; _ } = Printf.fprintf chan "%s" name

let print fmt { name; _ } = Format.pp_print_string fmt name

let create ~visibility name = { name; already_encoded = false; visibility }

let create_global name = create ~visibility:Global name

let create_local name = create ~visibility:Local name

let create_without_encoding ~visibility name =
  { name; already_encoded = true; visibility }

let to_raw_string { name; _ } = name

let visibility t = t.visibility

let is_global t = match t.visibility with Global -> true | Local -> false

let is_local t = match t.visibility with Local -> true | Global -> false

let escape name =
  let escaped_nb = ref 0 in
  for i = 0 to String.length name - 1 do
    if should_be_escaped (String.unsafe_get name i) then incr escaped_nb
  done;
  if !escaped_nb = 0
  then name
  else
    (* Each escaped character is replaced by 3 characters (a $, and 2 for its
       hexadecimal representation)*)
    let b = Buffer.create (String.length name + (2 * !escaped_nb)) in
    String.iter
      (fun c ->
        if should_be_escaped c
        then Printf.bprintf b "$%02x" (Char.code c)
        else Buffer.add_char b c)
      name;
    Buffer.contents b

let to_escaped_string ?suffix ~symbol_prefix t =
  let suffix = match suffix with None -> "" | Some suffix -> suffix in
  symbol_prefix ^ escape t ^ suffix

let encode { name; already_encoded } =
  if already_encoded
  then name
  else
    let symbol_prefix = symbol_prefix () in
    to_escaped_string ~symbol_prefix name

(* Comparison and hashing are based on the encoded form, so two symbols that
   produce the same assembly output are considered equal regardless of how they
   were constructed. *)
let compare t1 t2 = String.compare (encode t1) (encode t2)

let equal t1 t2 = compare t1 t2 = 0

let hash t = Hashtbl.hash (encode t)

include Identifiable.Make (struct
  type nonrec t = t

  let compare = compare

  let equal = equal

  let hash = hash

  let output = output

  let print = print
end)

(* We predefine several common runtime symbols. All are global. *)
module Predef = struct
  let caml_call_gc = create_global "caml_call_gc"

  let caml_call_gc_sse = create_global "caml_call_gc_sse"

  let caml_call_gc_avx = create_global "caml_call_gc_avx"

  let caml_call_gc_avx512 = create_global "caml_call_gc_avx512"

  let caml_c_call = create_global "caml_c_call"

  let caml_allocN = create_global "caml_allocN"

  let caml_allocN_sse = create_global "caml_allocN_sse"

  let caml_allocN_avx = create_global "caml_allocN_avx"

  let caml_allocN_avx512 = create_global "caml_allocN_avx512"

  let caml_alloc1 = create_global "caml_alloc1"

  let caml_alloc1_sse = create_global "caml_alloc1_sse"

  let caml_alloc1_avx = create_global "caml_alloc1_avx"

  let caml_alloc1_avx512 = create_global "caml_alloc1_avx512"

  let caml_alloc2 = create_global "caml_alloc2"

  let caml_alloc2_sse = create_global "caml_alloc2_sse"

  let caml_alloc2_avx = create_global "caml_alloc2_avx"

  let caml_alloc2_avx512 = create_global "caml_alloc2_avx512"

  let caml_alloc3 = create_global "caml_alloc3"

  let caml_alloc3_sse = create_global "caml_alloc3_sse"

  let caml_alloc3_avx = create_global "caml_alloc3_avx"

  let caml_alloc3_avx512 = create_global "caml_alloc3_avx512"

  let caml_ml_array_bound_error = create_global "caml_ml_array_bound_error"

  let caml_ml_array_align_error = create_global "caml_ml_array_align_error"

  let caml_raise_exn = create_global "caml_raise_exn"

  let caml_reraise_exn = create_global "caml_reraise_exn"

  let caml_call_local_realloc = create_global "caml_call_local_realloc"

  let caml_call_realloc_stack = create_global "caml_call_realloc_stack"

  let caml_c_call_stack_args = create_global "caml_c_call_stack_args"

  let stapsdt_base = create_without_encoding ~visibility:Global "_.stapsdt.base"

  let caml_probes_semaphore ~name =
    create_global ("caml_probes_semaphore_" ^ name)

  let caml_negf_mask = create_global "caml_negf_mask"

  let caml_absf_mask = create_global "caml_absf_mask"

  let caml_negf32_mask = create_global "caml_negf32_mask"

  let caml_absf32_mask = create_global "caml_absf32_mask"
end
