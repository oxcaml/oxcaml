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

open! Import

module Target = struct
  type t = Direct of string | Got of string | Plt of string

  let from_label label =
    match String.split_on_char ~sep:'@' label with
    | [ label ] -> Some (Direct label)
    | [ label; "GOTPCREL" ] -> Some (Got label)
    | [ label; "PLT" ] -> Some (Plt label)
    | _ -> None

  let from_label_err label =
    match from_label label with
    | None -> errorf "Don't know how to interpret relocation target: %S" label
    | Some target -> Ok target
end

module Size = struct
  type t = S64 | S32

  let to_data_size = function
    | S64 -> X86_binary_emitter.B64
    | S32 -> X86_binary_emitter.B32

  let to_int = function S32 -> 4 | S64 -> 8
end

module Kind = struct
  type t = Absolute | Relative
end

type t = {
  offset_from_section_beginning : int;
  size : Size.t;
  kind : Kind.t;
  target : Target.t;
  addend : int64;
}

let size_from_reloc_kind (kind : X86_binary_emitter.Relocation.Kind.t) =
  match kind with REL32 _ | DIR32 _ -> Size.S32 | DIR64 _ -> Size.S64

let kind_from_reloc_kind (kind : X86_binary_emitter.Relocation.Kind.t) =
  match kind with
  | DIR32 _ | DIR64 _ -> Kind.Absolute
  | REL32 _ -> Kind.Relative

let label_from_reloc_kind (kind : X86_binary_emitter.Relocation.Kind.t) =
  match kind with
  | REL32 (label, _) | DIR32 (label, _) | DIR64 (label, _) -> label

let addend_from_reloc_kind (kind : X86_binary_emitter.Relocation.Kind.t) =
  match kind with
  | REL32 (_, addend) | DIR32 (_, addend) | DIR64 (_, addend) -> addend

let from_x86_relocation relocation =
  let open Option.Op in
  let ({ offset_from_section_beginning; kind }
        : X86_binary_emitter.Relocation.t) =
    relocation
  in
  let label = label_from_reloc_kind kind in
  let addend = addend_from_reloc_kind kind in
  let size = size_from_reloc_kind kind in
  let kind = kind_from_reloc_kind kind in
  let+ target = Target.from_label label in
  { target; addend; size; kind; offset_from_section_beginning }

let from_x86_relocation_err relocation =
  let open Result.Op in
  let ({ offset_from_section_beginning; kind }
        : X86_binary_emitter.Relocation.t) =
    relocation
  in
  let label = label_from_reloc_kind kind in
  let addend = addend_from_reloc_kind kind in
  let size = size_from_reloc_kind kind in
  let kind = kind_from_reloc_kind kind in
  let+ target = Target.from_label_err label in
  { target; addend; size; kind; offset_from_section_beginning }

let value_comparator t =
  match t.kind with
  | Absolute -> Int64.unsigned_compare
  | Relative -> Int64.compare

let min_value t =
  match t.kind with
  | Absolute -> 0L
  | Relative ->
    match t.size with
    | S32 -> Int32.min_int |> Int64.of_int32
    | S64 -> Int64.min_int

let max_value t =
  match t.kind with
  | Absolute -> (
    match t.size with
    | S32 -> 0xffff_ffffL
    | S64 -> 0xffff_ffff_ffff_ffffL
  )
  | Relative ->
    match t.size with
    | S32 -> Int32.max_int |> Int64.of_int32
    | S64 -> Int64.max_int

