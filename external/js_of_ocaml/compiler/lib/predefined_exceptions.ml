(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)
open! Stdlib
open Code

let predefined_exceptions () =
  (* Register predefined exceptions in case of separate compilation *)
  let predefined_exceptions =
    Runtimedef.builtin_exceptions |> Array.to_list |> List.mapi ~f:(fun i name -> i, name)
  in
  let body =
    let open Code in
    List.map predefined_exceptions ~f:(fun (index, name) ->
        assert (String.is_valid_utf_8 name);
        let exn = Var.fresh () in
        let v_name = Var.fresh () in
        let v_index = Var.fresh () in
        [ Let (v_name, Constant (String name))
        ; Let
            ( v_index
            , Constant
                (Int
                   ((* Predefined exceptions are registered in
                       Symtable.init with [-index - 1] *)
                    Targetint.of_int_exn
                      (-index - 1))) )
        ; Let (exn, Block (248, [| v_name; v_index |], NotArray, Immutable))
        ]
        @
        match Config.target () with
        | `JavaScript ->
            let v_name_js = Var.fresh () in
            [ Let (v_name_js, Constant (NativeString (Native_string.of_string name)))
            ; Let
                ( Var.fresh ()
                , Prim
                    ( Extern "caml_register_global"
                    , [ Pc (Int (Targetint.of_int_exn index)); Pv exn; Pv v_name_js ] ) )
            ]
        | `Wasm ->
            [ Let
                ( Var.fresh ()
                , Prim
                    ( Extern "caml_register_global"
                    , [ Pc (Int (Targetint.of_int_exn index)); Pv exn; Pv v_name ] ) )
              (* Also make the exception available to the generated code *)
            ; Let
                ( Var.fresh ()
                , Prim (Extern "caml_set_global", [ Pc (String name); Pv exn ]) )
            ])
    |> List.concat
  in
  let block = { params = []; body; branch = Stop } in
  let unit_info =
    { Unit_info.provides = StringSet.of_list (List.map ~f:snd predefined_exceptions)
    ; requires = StringSet.empty
    ; force_link = true
    ; effects_without_cps = false
    ; primitives = []
    ; aliases = []
    }
  in
  { start = 0; blocks = Addr.Map.singleton 0 block; free_pc = 1 }, unit_info
