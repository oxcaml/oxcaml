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

module TG = Type_grammar

type t = TG.Env_extension.t

let fold ~equation ({ equations } : t) acc =
  Name.Map.fold equation equations acc

let invariant ({ equations } : t) =
  if Flambda_features.check_invariants ()
  then Name.Map.iter More_type_creators.check_equation equations

let empty = TG.Env_extension.empty

let is_empty ({ equations } : t) = Name.Map.is_empty equations

let from_map equations =
  let t = TG.Env_extension.create ~equations in
  invariant t;
  t

let to_map ({ equations } : t) = equations

let has_equation name ({ equations } : t) = Name.Map.mem name equations

let one_equation name ty =
  More_type_creators.check_equation name ty;
  TG.Env_extension.create ~equations:(Name.Map.singleton name ty)

let add_or_replace_equation ({ equations } : t) name ty =
  More_type_creators.check_equation name ty;
  if Flambda_features.check_invariants () && Name.Map.mem name equations
  then
    Format.eprintf
      "Warning: Overriding equation for name %a@\n\
       Old equation is@ @[%a@]@\n\
       New equation is@ @[%a@]@."
      Name.print name TG.print
      (Name.Map.find name equations)
      TG.print ty;
  TG.Env_extension.create ~equations:(Name.Map.add name ty equations)

let replace_equation ({ equations } : t) name ty =
  TG.Env_extension.create
    ~equations:(Name.Map.add (* replace *) name ty equations)

let disjoint_union ({ equations = equations1 } : t)
    ({ equations = equations2 } : t) =
  TG.Env_extension.create
    ~equations:(Name.Map.disjoint_union equations1 equations2)

let ids_for_export = TG.Env_extension.ids_for_export

let apply_renaming = TG.Env_extension.apply_renaming

let free_names = TG.Env_extension.free_names

let print = TG.Env_extension.print

module With_extra_variables = struct
  type t =
    { existential_vars : Variable.t list;
      equations : TG.t Name.Map.t
    }

  let print ppf { existential_vars; equations } =
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(variables@ @[<hov 1>%a@])@]@ @[<hov 1>%a@])@ @]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Variable.print)
      existential_vars TG.Env_extension.print
      (TG.Env_extension.create ~equations)

  let fold ~variable ~equation t acc =
    let acc =
      List.fold_left (fun acc var -> variable var acc) acc t.existential_vars
    in
    Name.Map.fold equation t.equations acc

  let empty = { existential_vars = []; equations = Name.Map.empty }

  let add_definition t var kind =
    if
      Flambda_features.check_light_invariants ()
      && not (Flambda_kind.equal kind (Variable.kind var))
    then
      Misc.fatal_errorf "Incorrect kind for variable (expected %a): %a"
        Flambda_kind.print (Variable.kind var) Flambda_kind.print kind;
    { existential_vars = var :: t.existential_vars; equations = t.equations }

  let add_or_replace_equation t name ty =
    More_type_creators.check_equation name ty;
    { existential_vars = t.existential_vars;
      equations = Name.Map.add name ty t.equations
    }

  let free_names { existential_vars; equations } =
    let variables = Variable.Set.of_list existential_vars in
    let free_names =
      Name_occurrences.create_variables variables Name_mode.in_types
    in
    Name.Map.fold
      (fun name ty free_names ->
        let free_names =
          Name_occurrences.add_name free_names name Name_mode.in_types
        in
        Name_occurrences.union free_names (TG.free_names ty))
      equations free_names

  let apply_renaming { existential_vars; equations } renaming =
    (* Make sure to preserve order here! *)
    let existential_vars =
      List.map
        (fun var -> Renaming.apply_variable renaming var)
        existential_vars
    in
    let equations =
      Name.Map.fold
        (fun name ty result ->
          let name' = Renaming.apply_name renaming name in
          let ty' = TG.apply_renaming ty renaming in
          Name.Map.add name' ty' result)
        equations Name.Map.empty
    in
    { existential_vars; equations }

  let ids_for_export { existential_vars; equations } =
    let variables = Variable.Set.of_list existential_vars in
    let ids = Ids_for_export.create ~variables () in
    Name.Map.fold
      (fun name ty ids ->
        let ids = Ids_for_export.add_name ids name in
        Ids_for_export.union ids (TG.ids_for_export ty))
      equations ids

  let existential_vars { existential_vars; _ } = existential_vars

  let map_types ({ existential_vars; equations } as t) ~f =
    let equations' = Name.Map.map_sharing f equations in
    if equations == equations'
    then t
    else { existential_vars; equations = equations' }
end
