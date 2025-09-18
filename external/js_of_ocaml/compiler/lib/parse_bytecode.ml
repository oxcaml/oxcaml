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

let times = Debug.find "times"

let predefined_exceptions =
  Runtimedef.builtin_exceptions |> Array.to_list |> List.mapi ~f:(fun i name -> i, name)

module Debug = struct
  type path = string

  type ml_unit =
    { module_name : string
    ; paths : path list
    }
  [@@ocaml.warning "-unused-field"]

  module UnitTable = Hashtbl.Make (struct
    type t = string * string option

    let hash = Hashtbl.hash

    let equal (a, b) (c, d) = String.equal a c && Option.equal String.equal b d
  end)

  type summary =
    { is_empty : bool
    ; units : ml_unit UnitTable.t
    }

  let default_summary () = { is_empty = true; units = UnitTable.create 0 }

  let is_empty t = t.is_empty

  let paths (s : summary) ~units =
    let paths =
      UnitTable.fold
        (fun _ u acc -> if StringSet.mem u.module_name units then u.paths :: acc else acc)
        s.units
        []
    in
    StringSet.of_list (List.concat paths)
end

type cmj_body =
  { program : Code.program
  ; last_var : Code.Addr.t
        (** Highest used variable in the translation, since it is kept track by a
        mutable state (in [Var]), and the [ocamlj] compiler and [js_of_ocaml]
        need to have these in sync *)
  ; imported_compilation_units : Compilation_unit.t list
        (** Compilation units fetched from JSOO's global data table. Needed to fill in
      [Unit_info.t] in JSOO *)
  ; exported_compilation_unit : Compilation_unit.t
        (** Current compilation unit. Needed to fill in [Unit_info.t] in JSOO *)
  }

type one =
  { code : Code.program
  ; cmis : StringSet.t
  ; debug : Debug.summary
  }

type compilation_unit =
  { name : string
  ; info : Unit_info.t
  ; contents : one
  }

let primitives (_ : one) = []

(* Note: .cmja files are now JavaScript archives, not binary format *)

let from_cmj ic ~name =
  let timer = Timer.make () in
  let { program; last_var; imported_compilation_units; exported_compilation_unit } :
      cmj_body =
    Marshal.from_channel ic
  in
  Code.invariant program;
  Code.Var.set_last ~min:last_var;
  if times () then Format.eprintf "  parsing: %a (%s)@." Timer.print timer name;
  let code : one =
    (* CR-soon selee: currently [ocamlj] does not pass any debug-related information.
       This should be changed in the [.cmj] file format. *)
    { code = program; cmis = StringSet.empty; debug = Debug.default_summary () }
  in
  let uinfo =
    Unit_info.of_compilation_units
      ~exported:exported_compilation_unit
      ~imported:imported_compilation_units
  in
  { name; info = uinfo; contents = code }

let load
    ~filename
    ~include_dirs:(_ : string list)
    ~include_cmis:(_ : bool)
    ~debug:(_ : bool) =
  let ic = open_in_bin filename in
  Fun.protect
    (fun () ->
      let name = Filename.remove_extension (Filename.basename filename) in
      (* CR-soon jvanburen: include [name] in the cmj format so we don't have to  *)
      let magic = Magic_number.from_channel_exn ic in
      let bad_magic_number () = raise Magic_number.(Bad_magic_number (to_string magic)) in
      match Magic_number.kind magic with
      | `Other _ -> bad_magic_number ()
      | (`Cmj | `Cmja) as kind
        when Config.Flag.check_magic ()
             && not (Magic_number.equal magic (Magic_number.current kind)) ->
          bad_magic_number ()
      | `Cmj -> `Cmj (from_cmj ic ~name)
      | `Cmja ->
          (* .cmja files are now JavaScript archives, not binary format *)
          failwith ".cmja files are now JavaScript archives and should be treated as .js files")
    ~finally:(fun () -> close_in ic)

let predefined_exceptions () =
  (* Register predefined exceptions in case of separate compilation *)
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
