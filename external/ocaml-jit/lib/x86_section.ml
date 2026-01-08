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
module DLL = Oxcaml_utils.Doubly_linked_list

let name s_l s_opt s_l' =
  let first = String.concat ~sep:"," s_l in
  let mid = match s_opt with None -> "" | Some s -> Printf.sprintf ",%S" s in
  let last = match s_l' with [] -> "" | l -> "," ^ String.concat ~sep:"," l in
  first ^ mid ^ last

type t = { name : string; instructions : X86_ast.asm_program }

let assemble ~arch { name; instructions } =
  let section =
    {
      X86_binary_emitter.sec_name = name;
      sec_instrs = DLL.to_array instructions;
    }
  in
  X86_binary_emitter.assemble_section arch section

module Map = struct
  type nonrec t = X86_ast.asm_program String.Map.t

  let append key dll t =
    String.Map.update ~key t ~f:(function
      | None -> Some dll
      | Some dll' ->
        DLL.transfer ~to_:dll' ~from:dll ();
        Some dll')

  let from_program prog =
    let open X86_ast in
    let section_name section first_occurrence =
      let first_occurrence =
        match first_occurrence with
        | `First_occurrence -> true
        | `Not_first_occurrence -> false
      in
      let details =
        Asm_targets.Asm_section.details section ~first_occurrence
      in
      name details.names details.flags details.args
    in
    match DLL.hd prog with
    | None -> String.Map.empty
    | Some (Directive (Section (section, first_occurrence))) ->
      let initial_section = section_name section first_occurrence in
      let acc = ref String.Map.empty in
      let current_section = ref initial_section in
      let current_instrs = ref (DLL.make_empty ()) in
      DLL.iter prog ~f:(fun instr ->
        match instr with
        | Directive (Section (section, first_occurrence)) ->
          acc := append !current_section !current_instrs !acc;
          current_section := section_name section first_occurrence;
          current_instrs := DLL.make_empty ()
        | _ ->
          DLL.add_end !current_instrs instr);
      acc := append !current_section !current_instrs !acc;
      !acc
    | Some _ -> failwithf "Invalid program, should start with section"
end
