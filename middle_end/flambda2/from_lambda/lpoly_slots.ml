(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Jack Rickard, Jane Street Europe                 *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Derivation of flambda2 slot identities for layout-polymorphism environment
   sets (see [Lambda.Pset_of_closures]).

   The environment block of a template is allocated in the unit that evaluates
   the template's definition, but is projected from in any unit that
   instantiates the template (the projections live inside the slambda template
   marshalled into the .cmx). Slot identity in flambda2 is (compilation unit,
   stamp), so every unit must reconstruct exactly the same slots; we achieve
   this by deriving the (negative) stamps injectively from the template's stamp
   and the capture/leaf indices. *)

module K = Flambda_kind

let template_id (template : Lambda.template_ref) =
  match template with
  | Template_id id -> id
  | Template_var _ ->
    Misc.fatal_error
      "Lpoly_slots: unresolved template reference (should have been resolved \
       by slambda evaluation)"

let compilation_unit template =
  match Template_id.owner (template_id template) with
  | Some cu -> cu
  | None -> Current_unit.get_cu_exn ()

let stamp template =
  let stamp = Template_id.stamp (template_id template) in
  if stamp < 0
  then Misc.fatal_errorf "Lpoly_slots: negative template stamp %d" stamp;
  stamp

(* Injective pairing of non-negative integers. Overflow would produce a
   non-negative derived stamp below, which [create_deterministic] rejects. *)
let cantor_pair a b = ((a + b) * (a + b + 1) / 2) + b

let function_slot template =
  Function_slot.create_deterministic
    (compilation_unit template)
    ~name:"lpoly_env" ~is_always_immediate:false K.value
    ~stamp:(-stamp template - 1)

let value_slots template ~(layouts : Lambda.layout list) ~machine_width =
  let cu = compilation_unit template in
  let stamp = stamp template in
  List.mapi
    (fun i layout ->
      let component =
        Flambda_arity.Component_for_creation.from_lambda layout ~machine_width
      in
      let kinds = Flambda_arity.unarize (Flambda_arity.create [component]) in
      List.mapi
        (fun j kind ->
          let is_always_immediate =
            match[@ocaml.warning "-4"]
              K.With_subkind.non_null_value_subkind kind
            with
            | Tagged_immediate -> true
            | _ -> false
          in
          let slot =
            Value_slot.create_deterministic cu
              ~name:(Printf.sprintf "lpoly_env_%d_%d" i j)
              ~is_always_immediate (K.With_subkind.kind kind)
              ~stamp:(-cantor_pair (cantor_pair stamp i) j - 1)
          in
          slot, kind)
        kinds)
    layouts
