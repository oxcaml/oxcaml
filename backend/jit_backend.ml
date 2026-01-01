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

(* JIT backend dispatch - architecture-independent interface for JIT. This
   module allows jit.ml to register a callback without knowing about
   architecture-specific emitters (X86_binary_emitter, Arm64_binary_emitter). *)

module String_map = Map.Make (String)

(* Packed sections with their Binary_emitter.S module, hiding the
   architecture-specific types using an existential. *)
type packed_sections =
  | Packed :
      { emitter :
          (module Binary_emitter_intf.S
             with type Assembled_section.t = 'a
              and type Relocation.t = 'r);
        sections : 'a String_map.t
      }
      -> packed_sections

type callback = packed_sections -> unit

let current_callback : callback option ref = ref None

(* For x86, we need to save/restore the old internal_assembler *)
let saved_x86_internal_assembler = ref None

let register callback =
  current_callback := Some callback;
  match Target_system.architecture () with
  | X86_64 ->
    (* Save old x86 internal assembler and register our hook *)
    saved_x86_internal_assembler := !X86_proc.internal_assembler;
    X86_proc.register_internal_assembler (fun ~delayed:_ sections _filename ->
        (* Assemble each section *)
        let sections_map =
          List.fold_left
            (fun map (name, instrs) ->
              let name_str = X86_proc.Section_name.to_string name in
              let section =
                { X86_binary_emitter.sec_name = name_str;
                  sec_instrs = Array.of_list instrs
                }
              in
              let binary_section =
                X86_binary_emitter.assemble_section X86_ast.X64 section
              in
              if X86_binary_emitter.size binary_section = 0
              then map
              else String_map.add name_str binary_section map)
            String_map.empty sections
        in
        let packed =
          Packed
            { emitter = (module X86_binary_emitter.For_jit);
              sections = sections_map
            }
        in
        callback packed)
  | AArch64 ->
    (* ARM64 binary emitter JIT support not yet available *)
    Misc.fatal_error "JIT not yet supported on AArch64"
  | _ -> Misc.fatal_error "JIT not supported on this architecture"

let unregister () =
  current_callback := None;
  match Target_system.architecture () with
  | X86_64 ->
    X86_proc.internal_assembler := !saved_x86_internal_assembler;
    saved_x86_internal_assembler := None
  | AArch64 ->
    (* ARM64 binary emitter JIT support not yet available *)
    ()
  | _ -> Misc.fatal_error "JIT not supported on this architecture"
