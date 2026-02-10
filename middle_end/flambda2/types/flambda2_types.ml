(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Typing_env = struct
  include Typing_env
  open Meet_env

  let add_equation t name ty =
    add_equation t name ty ~meet_type:(Meet.meet_type ())

  let add_equation_on_simple t simple ty =
    add_equation_on_simple t simple ty ~meet_type:(Meet.meet_type ())

  let add_is_null_relation t name ~scrutinee =
    use_meet_env t ~f:(fun t ->
        let t = add_equation t name (Type_grammar.is_null ~scrutinee) in
        Name.pattern_match name
          ~symbol:(fun _ -> t)
          ~var:(fun var ->
            let scrutinee_ty =
              Type_grammar.create_from_head_value
                { is_null = Maybe_null { is_null = Some var };
                  non_null = Unknown
                }
            in
            add_equation_on_simple t scrutinee scrutinee_ty))

  let add_is_int_relation t name ~scrutinee =
    use_meet_env t ~f:(fun t ->
        let t =
          add_equation t name (Type_grammar.is_int_for_scrutinee ~scrutinee)
        in
        Name.pattern_match name
          ~symbol:(fun _ -> t)
          ~var:(fun var ->
            let scrutinee_head_of_kind_value_non_null =
              Type_grammar.Head_of_kind_value_non_null.create_variant
                ~is_unique:false ~blocks:Unknown ~immediates:Unknown
                ~extensions:No_extensions ~is_int:(Some var) ~get_tag:None
            in
            let scrutinee_ty =
              Type_grammar.create_from_head_value
                { is_null = Not_null;
                  non_null = Ok scrutinee_head_of_kind_value_non_null
                }
            in
            add_equation_on_simple t scrutinee scrutinee_ty))

  let add_get_tag_relation t name ~scrutinee =
    use_meet_env t ~f:(fun t ->
        let t =
          add_equation t name (Type_grammar.get_tag_for_block ~block:scrutinee)
        in
        Name.pattern_match name
          ~symbol:(fun _ -> t)
          ~var:(fun var ->
            (* CR bclement: We would like to set [immediates] to [Known bottom],
               but it turns out that [add_get_tag_relation] is called on
               arguments that might not be blocks during unboxing. *)
            let scrutinee_head_of_kind_value_non_null =
              Type_grammar.Head_of_kind_value_non_null.create_variant
                ~is_unique:false ~blocks:Unknown ~immediates:Unknown
                ~extensions:No_extensions ~is_int:None ~get_tag:(Some var)
            in
            let scrutinee_ty =
              Type_grammar.create_from_head_value
                { is_null = Not_null;
                  non_null = Ok scrutinee_head_of_kind_value_non_null
                }
            in
            add_equation_on_simple t scrutinee scrutinee_ty))

  let add_equation t name ty =
    use_meet_env t ~f:(fun t -> add_equation t name ty)

  let add_equations_on_params t ~params ~param_types =
    use_meet_env t ~f:(fun t ->
        add_equations_on_params t ~params ~param_types
          ~meet_type:(Meet.meet_type ()))

  let add_env_extension t extension =
    use_meet_env t ~f:(fun t ->
        add_env_extension t extension ~meet_type:(Meet.meet_type ()))

  let add_env_extension_with_extra_variables t extension =
    use_meet_env t ~f:(fun t ->
        add_env_extension_with_extra_variables t extension
          ~meet_type:(Meet.meet_type ()))

  module Alias_set = Aliases.Alias_set
end

module Typing_env_extension = struct
  include Typing_env_extension

  let add_is_null_relation t name ~scrutinee =
    add_or_replace_equation t name (Type_grammar.is_null ~scrutinee)

  let add_is_int_relation t name ~scrutinee =
    add_or_replace_equation t name
      (Type_grammar.is_int_for_scrutinee ~scrutinee)

  let add_get_tag_relation t name ~scrutinee =
    add_or_replace_equation t name
      (Type_grammar.get_tag_for_block ~block:scrutinee)
end

type typing_env = Typing_env.t

type typing_env_extension = Typing_env_extension.t

include Type_grammar
include More_type_creators
include Expand_head
include Meet
include Provers
include Reify
include Join_levels
module Code_age_relation = Code_age_relation
module Join_analysis = Join_env.Analysis

module Closures_entry = struct
  include Closures_entry

  let find_function_type t function_slot : _ Or_unknown.t =
    match find_function_type t ~exact:false function_slot with
    | Unknown | Bottom -> Unknown
    | Ok function_type -> Known function_type
end

let remove_outermost_alias env ty =
  Expand_head.expand_head env ty |> Expand_head.Expanded_type.to_type

module Equal_types_for_debug = struct
  let equal_type env t1 t2 =
    Equal_types_for_debug.equal_type ~meet_type:(Meet.meet_type ()) env t1 t2

  let equal_env_extension env ext1 ext2 =
    Equal_types_for_debug.equal_env_extension ~meet_type:(Meet.meet_type ()) env
      ext1 ext2
end

module Rewriter = Traversals
