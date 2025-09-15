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

let name s_l s_opt s_l' =
  let first = String.concat ~sep:"," s_l in
  let mid = match s_opt with None -> "" | Some s -> Printf.sprintf ",%S" s in
  let last = match s_l' with [] -> "" | l -> "," ^ String.concat ~sep:"," l in
  first ^ mid ^ last

type t = { name : string; instructions : X86_ast.asm_line list }

let assemble ~arch { name; instructions } =
  let section =
    {
      X86_binary_emitter.sec_name = name;
      sec_instrs = Array.of_list instructions;
    }
  in
  X86_binary_emitter.assemble_section arch section

module Map = struct
  type nonrec t = X86_ast.asm_line list String.Map.t

  let append key l t =
    String.Map.update ~key t ~f:(function
      | None -> Some l
      | Some l' -> Some (l' @ l))

  let from_program l =
    let open X86_ast in
    let rec aux acc current_section current_instrs l =
      let add_current () =
        append current_section (List.rev current_instrs) acc
      in
      match l with
      | [] -> add_current ()
      | Section (s_l, s_opt, s_l') :: tl ->
          let acc = add_current () in
          let current_section = name s_l s_opt s_l' in
          let current_instrs = [] in
          aux acc current_section current_instrs tl
      | (_ as instr) :: tl ->
          aux acc current_section (instr :: current_instrs) tl
    in
    match l with
    | [] -> String.Map.empty
    | Section (s_l, s_opt, s_l') :: tl ->
        let current_section = name s_l s_opt s_l' in
        let current_instrs = [] in
        aux String.Map.empty current_section current_instrs tl
    | _line :: _ -> failwithf "Invalid program, should start with section"
end
