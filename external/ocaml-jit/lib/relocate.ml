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

open Import

let out_of_text_error ~got_or_plt ~section_name =
  errorf
    "Relocation through %s in section %S. Such relocations should not be found \
     outside .text section"
    got_or_plt section_name

type table_lookup = string -> Address.t option

let one (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    ~symbols ~got_lookup ~plt_lookup ~section_name (binary_section : a addressed) (reloc : r) =
  let open Result.Op in
  let _sym = E.Relocation.target_symbol reloc in
  let is_got = E.Relocation.is_got_reloc reloc in
  let is_plt = E.Relocation.is_plt_reloc reloc in
  (* Build lookup function that routes to GOT/PLT/symbols as appropriate *)
  let lookup_target target =
    let name = Symbols.target_to_string target in
    if is_got then
      match got_lookup with
      | None -> None
      | Some lookup ->
        let result = Option.map Address.to_int64 (lookup name) in
        (match Sys.getenv_opt "OCAML_JIT_DEBUG", result with
        | Some ("true" | "1"), Some addr ->
          Printf.eprintf "GOT lookup for %s -> entry at %Lx\n%!" name addr
        | Some ("true" | "1"), None ->
          Printf.eprintf "GOT lookup for %s -> NOT FOUND\n%!" name
        | _ -> ());
        result
    else if is_plt then
      match plt_lookup with
      | None -> None
      | Some lookup ->
        Option.map Address.to_int64 (lookup name)
    else
      Option.map Address.to_int64 (Symbols.find symbols name)
  in
  (* Check for invalid GOT/PLT outside .text *)
  let* () =
    if is_got && Option.is_none got_lookup then
      out_of_text_error ~got_or_plt:"GOT" ~section_name
    else if is_plt && Option.is_none plt_lookup then
      out_of_text_error ~got_or_plt:"PLT" ~section_name
    else Ok ()
  in
  let offset = E.Relocation.offset_from_section_beginning reloc in
  let place_address =
    Int64.add (Address.to_int64 binary_section.address) (Int64.of_int offset)
  in
  (* Provide a callback to read the 32-bit instruction at the relocation site.
     This is needed for ARM64 to read-modify-write instruction bit fields. *)
  let read_instruction () =
    let contents = E.Assembled_section.contents binary_section.value in
    let b0 = Char.code (String.get contents offset) in
    let b1 = Char.code (String.get contents (offset + 1)) in
    let b2 = Char.code (String.get contents (offset + 2)) in
    let b3 = Char.code (String.get contents (offset + 3)) in
    (* Little-endian: b0 is least significant *)
    Int32.(logor (logor (of_int b0) (shift_left (of_int b1) 8))
      (logor (shift_left (of_int b2) 16) (shift_left (of_int b3) 24)))
  in
  let* data = E.Relocation.compute_value reloc ~place_address ~lookup_target ~read_instruction in
  let size = E.Relocation.size reloc in
  E.Assembled_section.add_patch binary_section.value ~offset ~size ~data;
  Ok ()

let all (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    ~symbols ~got_lookup ~plt_lookup ~section_name (binary_section : a addressed) =
  let relocs = E.Assembled_section.relocations binary_section.value in
  Result.List.iter_all relocs
    ~f:(one (module E) ~symbols ~got_lookup ~plt_lookup ~section_name binary_section)
