(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Is_parameter = struct
  type t =
    | Local_var
    | Parameter of { index : int }
    | Implicit_parameter

  let local_var = Local_var

  let parameter ~index =
    if index < 0 then Misc.fatal_errorf "Bad parameter index %d" index;
    Parameter { index }

  let implicit_parameter = Implicit_parameter

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Local_var, Local_var -> 0
      | Parameter { index = index1 }, Parameter { index = index2 } ->
        Int.compare index1 index2
      | Implicit_parameter, Implicit_parameter -> 0
      | Local_var, (Parameter _ | Implicit_parameter) -> -1
      | (Parameter _ | Implicit_parameter), Local_var -> 1
      | Parameter _, Implicit_parameter -> -1
      | Implicit_parameter, Parameter _ -> 1

    let equal t1 t2 = compare t1 t2 = 0

    let hash t =
      match t with
      | Local_var -> Hashtbl.hash "Local_var"
      | Parameter { index } -> Hashtbl.hash ("Parameter", index)
      | Implicit_parameter -> Hashtbl.hash "Implicit_parameter"

    let print ppf t =
      match t with
      | Local_var -> Format.pp_print_string ppf "local_var"
      | Parameter { index } ->
        Format.fprintf ppf "@[(parameter@ (index %d))@]" index
      | Implicit_parameter -> Format.pp_print_string ppf "implicit_parameter"

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

type t =
  { var : Variable.t;
    debug_uid : Flambda_debug_uid.t;
    name_mode : Name_mode.t;
    dbg : Debuginfo.t;
    is_parameter : Is_parameter.t
  }

let print_debug_uid ppf duid =
  if !Clflags.dump_debug_uids
  then Format.fprintf ppf "%@{%a}" Flambda_debug_uid.print duid

let print_is_parameter ppf (dbg, (is_parameter : Is_parameter.t)) =
  let depth = List.length (Debuginfo.to_items dbg) in
  if depth >= 1
  then
    match is_parameter with
    | Parameter { index } ->
      Format.fprintf ppf "[param-inlined-fn:%d@%a]" index
        Debuginfo.print_compact dbg
    | Local_var | Implicit_parameter -> ()

let print ppf { var; debug_uid; name_mode = _; dbg; is_parameter } =
  Format.fprintf ppf "%a%a%a" Variable.print var print_is_parameter
    (dbg, is_parameter) print_debug_uid debug_uid

let create var debug_uid name_mode ~dbg ~is_parameter =
  (* Note that [name_mode] might be [In_types], e.g. when dealing with function
     return types and also using [Typing_env.add_definition]. *)
  { var; debug_uid; name_mode; dbg; is_parameter }

let var t = t.var

let name t = Name.var (var t)

let debug_uid t = t.debug_uid

let name_mode t = t.name_mode

let dbg t = t.dbg

let is_parameter t = t.is_parameter

let add_inlined_debuginfo t inlined_debuginfo =
  { t with dbg = Inlined_debuginfo.rewrite inlined_debuginfo t.dbg }

let with_var t var = { t with var }

let with_name_mode t name_mode = { t with name_mode }

let rename t = with_var t (Variable.rename t.var)

let is_renamed_version_of t t' =
  Name_mode.equal t.name_mode t'.name_mode
  && Variable.is_renamed_version_of t.var t'.var

let apply_renaming t renaming =
  with_var t (Renaming.apply_variable renaming t.var)

let free_names t = Name_occurrences.singleton_variable t.var t.name_mode

let ids_for_export
    { var; debug_uid = _; name_mode = _; dbg = _; is_parameter = _ } =
  Ids_for_export.add_variable Ids_for_export.empty var

let renaming { var; debug_uid = _; name_mode = _; dbg = _; is_parameter = _ }
    ~guaranteed_fresh =
  let { var = guaranteed_fresh;
        debug_uid = _;
        name_mode = _;
        dbg = _;
        is_parameter = _
      } =
    guaranteed_fresh
  in
  Renaming.add_fresh_variable Renaming.empty var ~guaranteed_fresh
