(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2020 OCamlPro SAS                                    *)
(*   Copyright 2018--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Simple = Int_ids.Simple

type t =
  { param : Variable.t;
    uid : Flambda_debug_uid.t;
    kind : Flambda_kind.With_subkind.t;
    may_be_in_region_ended_in_body : bool
  }

include Container_types.Make (struct
  type nonrec t = t

  let compare
      { param = param1;
        kind = kind1;
        uid = uid1;
        may_be_in_region_ended_in_body = flag1
      }
      { param = param2;
        kind = kind2;
        uid = uid2;
        may_be_in_region_ended_in_body = flag2
      } =
    let c = Variable.compare param1 param2 in
    if c <> 0
    then c
    else
      let c = Flambda_kind.With_subkind.compare kind1 kind2 in
      if c <> 0
      then c
      else
        let c = Flambda_debug_uid.compare uid1 uid2 in
        if c <> 0 then c else Bool.compare flag1 flag2

  let equal t1 t2 = compare t1 t2 = 0

  let hash { param; kind; uid; may_be_in_region_ended_in_body } =
    Hashtbl.hash
      ( Variable.hash param,
        Flambda_kind.With_subkind.hash kind,
        Flambda_debug_uid.hash uid,
        may_be_in_region_ended_in_body )

  let print_debug_uid ppf duid =
    if !Clflags.dump_debug_uids
    then Format.fprintf ppf "%@{%a}" Flambda_debug_uid.print duid

  let print_may_be_in_region_ended_in_body ppf flag =
    if flag then Format.pp_print_string ppf "[may_be_in_region_ended_in_body]"

  let [@ocamlformat "disable"] print ppf
      { param; kind; uid; may_be_in_region_ended_in_body } =
    Format.fprintf ppf "@[(%t%a%a%a%t @<1>\u{2237} %a)@]"
      Flambda_colours.parameter
      Variable.print param
      print_debug_uid uid
      print_may_be_in_region_ended_in_body may_be_in_region_ended_in_body
      Flambda_colours.pop
      Flambda_kind.With_subkind.print kind
end)

let create param kind uid =
  if
    not
      (Flambda_kind.equal
         (Flambda_kind.With_subkind.kind kind)
         (Variable.kind param))
  then
    Misc.fatal_errorf
      "Cannot create parameter %a with kind %a: it must have a subkind of %a"
      Variable.print param Flambda_kind.With_subkind.print kind
      Flambda_kind.print (Variable.kind param);
  { param; kind; uid; may_be_in_region_ended_in_body = false }

let var t = t.param

let var_and_uid t = t.param, t.uid

let name t = Name.var (var t)

let simple t = Simple.var (var t)

let kind t = t.kind

let with_kind t kind = { t with kind }

let may_be_in_region_ended_in_body t = t.may_be_in_region_ended_in_body

let mark_may_be_in_region_ended_in_body t =
  { t with may_be_in_region_ended_in_body = true }

let rename t = { t with param = Variable.rename t.param }

let is_renamed_version_of t t' =
  Flambda_kind.With_subkind.equal t.kind t'.kind
  && Variable.is_renamed_version_of t.param t'.param

let free_names { param; kind = _; uid = _; may_be_in_region_ended_in_body = _ }
    =
  Name_occurrences.singleton_variable param Name_mode.normal

let apply_renaming
    ({ param; kind = _; uid = _; may_be_in_region_ended_in_body = _ } as t)
    renaming =
  let param' = Renaming.apply_variable renaming param in
  if param == param' then t else { t with param = param' }

let import_and_rename
    ({ param; kind = _; uid = _; may_be_in_region_ended_in_body = _ } as t)
    renaming =
  let renaming, param = Renaming.bind_variable renaming param in
  renaming, { t with param }

let ids_for_export
    { param; kind = _; uid = _; may_be_in_region_ended_in_body = _ } =
  Ids_for_export.add_variable Ids_for_export.empty param
