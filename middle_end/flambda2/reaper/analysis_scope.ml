(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
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

type t =
  | Current_unit
  | Lto_participants of Compilation_unit.Set.t

let contains_unit t unit =
  match t with
  | Current_unit -> Current_unit.is_current unit
  | Lto_participants units -> Compilation_unit.Set.mem unit units

let contains_code_id t code_id =
  contains_unit t (Code_id.get_compilation_unit code_id)

let is_local_field t field =
  Flambda_features.reaper_local_fields ()
  &&
  match Field.view field with
  | Value_slot vs -> contains_unit t (Value_slot.get_compilation_unit vs)
  | Function_slot fs -> contains_unit t (Function_slot.get_compilation_unit fs)
  | Block _ | Call_witness _ | Return_of_call _ | Code_id_of_call_witness
  | Is_int | Get_tag | Boxed_number _ ->
    false
