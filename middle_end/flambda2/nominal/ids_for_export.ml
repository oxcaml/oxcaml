(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Coercion = Int_ids.Coercion
module Simple = Int_ids.Simple

type t =
  { symbols : Symbol.Set.t;
    variables : Variable.Set.t;
    simples : Simple.Set.t;
    consts : Reg_width_const.Set.t;
    code_ids : Code_id.Set.t;
    continuations : Continuation.Set.t;
    fields : Field.Set.t
  }

let empty =
  { symbols = Symbol.Set.empty;
    variables = Variable.Set.empty;
    simples = Simple.Set.empty;
    consts = Reg_width_const.Set.empty;
    code_ids = Code_id.Set.empty;
    continuations = Continuation.Set.empty;
    fields = Field.Set.empty
  }

let create ?(symbols = Symbol.Set.empty) ?(variables = Variable.Set.empty)
    ?(simples = Simple.Set.empty) ?(consts = Reg_width_const.Set.empty)
    ?(code_ids = Code_id.Set.empty) ?(continuations = Continuation.Set.empty)
    ?(fields = Field.Set.empty) () =
  { symbols; variables; simples; consts; code_ids; continuations; fields }

let singleton_variable var = create ~variables:(Variable.Set.singleton var) ()

let singleton_code_id code_id =
  create ~code_ids:(Code_id.Set.singleton code_id) ()

let singleton_continuation cont =
  create ~continuations:(Continuation.Set.singleton cont) ()

let singleton_symbol symbol = create ~symbols:(Symbol.Set.singleton symbol) ()

let singleton_field field = create ~fields:(Field.Set.singleton field) ()

let add_const t const =
  { t with consts = Reg_width_const.Set.add const t.consts }

let add_variable t var = { t with variables = Variable.Set.add var t.variables }

let add_symbol t sym = { t with symbols = Symbol.Set.add sym t.symbols }

let add_name t name =
  Name.pattern_match name ~var:(add_variable t) ~symbol:(add_symbol t)

let add_simple t simple =
  let simples =
    if Coercion.is_id (Simple.coercion simple)
    then t.simples
    else Simple.Set.add simple t.simples
  in
  let t = { t with simples } in
  Simple.pattern_match simple ~const:(add_const t)
    ~name:(fun name ~coercion:_ -> add_name t name)

let add_code_id t code_id =
  { t with code_ids = Code_id.Set.add code_id t.code_ids }

let add_code_id_or_name t code_id_or_name =
  Code_id_or_name.pattern_match' code_id_or_name ~code_id:(add_code_id t)
    ~name:(add_name t)

let add_continuation t continuation =
  { t with continuations = Continuation.Set.add continuation t.continuations }

let add_field t field = { t with fields = Field.Set.add field t.fields }

let from_simple simple =
  let simples =
    if Coercion.is_id (Simple.coercion simple)
    then
      (* This simple will not be in the grand_table_of_simples *)
      Simple.Set.empty
    else Simple.Set.singleton simple
  in
  Simple.pattern_match simple
    ~const:(fun const ->
      create ~simples ~consts:(Reg_width_const.Set.singleton const) ())
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun var ~coercion:_ ->
          create ~simples ~variables:(Variable.Set.singleton var) ())
        ~symbol:(fun sym ~coercion:_ ->
          create ~simples ~symbols:(Symbol.Set.singleton sym) ()))

let union t1 t2 =
  { symbols = Symbol.Set.union t1.symbols t2.symbols;
    variables = Variable.Set.union t1.variables t2.variables;
    simples = Simple.Set.union t1.simples t2.simples;
    consts = Reg_width_const.Set.union t1.consts t2.consts;
    code_ids = Code_id.Set.union t1.code_ids t2.code_ids;
    continuations = Continuation.Set.union t1.continuations t2.continuations;
    fields = Field.Set.union t1.fields t2.fields
  }

let rec union_list ts =
  match ts with [] -> empty | t :: ts -> union t (union_list ts)

let no_ids _ = empty

let ids_for_export_set ~fold ~add_ids_key set =
  fold (fun key ids -> add_ids_key ids key) set empty

let ids_for_export_map ~fold ~add_ids_key ~ids_for_export_value map =
  fold
    (fun key value ids ->
      union ids (add_ids_key (ids_for_export_value value) key))
    map empty

let ids_for_export_name_set names =
  ids_for_export_set ~fold:Name.Set.fold ~add_ids_key:add_name names

let ids_for_export_code_id_set code_ids = create ~code_ids ()

let ids_for_export_continuation_set continuations = create ~continuations ()

let ids_for_export_variable_map ids_for_export_value map =
  ids_for_export_map ~fold:Variable.Map.fold ~add_ids_key:add_variable
    ~ids_for_export_value map

let ids_for_export_name_map ids_for_export_value map =
  ids_for_export_map ~fold:Name.Map.fold ~add_ids_key:add_name
    ~ids_for_export_value map

let ids_for_export_code_id_map ids_for_export_value map =
  ids_for_export_map ~fold:Code_id.Map.fold ~add_ids_key:add_code_id
    ~ids_for_export_value map

let ids_for_export_continuation_map ids_for_export_value map =
  ids_for_export_map ~fold:Continuation.Map.fold ~add_ids_key:add_continuation
    ~ids_for_export_value map

let ids_for_export_code_id_or_name_map ids_for_export_value map =
  ids_for_export_map ~fold:Code_id_or_name.Map.fold
    ~add_ids_key:add_code_id_or_name ~ids_for_export_value map

module Table_data = struct
  type ids = t

  type t =
    { symbols : Symbol.importer;
      variables : Variable.importer;
      simples : Simple.importer;
      consts : Reg_width_const.importer;
      code_ids : Code_id.importer;
      continuations : Continuation.importer;
      fields : Field.importer
    }

  let create (exported_ids : ids) =
    { symbols = Symbol.export exported_ids.symbols;
      variables = Variable.export exported_ids.variables;
      simples = Simple.export exported_ids.simples;
      consts = Reg_width_const.export exported_ids.consts;
      code_ids = Code_id.export exported_ids.code_ids;
      continuations = Continuation.export exported_ids.continuations;
      fields = Field.export exported_ids.fields
    }

  let create_import_map
      { symbols; variables; simples; consts; code_ids; continuations; fields }
      ~used_value_slots ~original_compilation_unit =
    Renaming.create_import_map ~symbols ~variables ~simples ~consts ~code_ids
      ~continuations ~fields ~used_value_slots ~original_compilation_unit
end

module Stamp_state = struct
  type t =
    { variables : Variable.stamp_state;
      code_ids : Code_id.stamp_state;
      continuations : Continuation.stamp_state;
      function_slots : Function_slot.stamp_state;
      value_slots : Value_slot.stamp_state;
      symbols : Symbol.stamp_state
    }

  let save () =
    { variables = Variable.export_stamp_state ();
      code_ids = Code_id.export_stamp_state ();
      continuations = Continuation.export_stamp_state ();
      function_slots = Function_slot.export_stamp_state ();
      value_slots = Value_slot.export_stamp_state ();
      symbols = Symbol.export_stamp_state ()
    }

  let restore
      { variables;
        code_ids;
        continuations;
        function_slots;
        value_slots;
        symbols
      } =
    Variable.restore_stamp_state variables;
    Code_id.restore_stamp_state code_ids;
    Continuation.restore_stamp_state continuations;
    Function_slot.restore_stamp_state function_slots;
    Value_slot.restore_stamp_state value_slots;
    Symbol.restore_stamp_state symbols

  let merge
      { variables = variables1;
        code_ids = code_ids1;
        continuations = continuations1;
        function_slots = function_slots1;
        value_slots = value_slots1;
        symbols = symbols1
      }
      { variables = variables2;
        code_ids = code_ids2;
        continuations = continuations2;
        function_slots = function_slots2;
        value_slots = value_slots2;
        symbols = symbols2
      } =
    { variables = Variable.merge_stamp_state variables1 variables2;
      code_ids = Code_id.merge_stamp_state code_ids1 code_ids2;
      continuations =
        Continuation.merge_stamp_state continuations1 continuations2;
      function_slots =
        Function_slot.merge_stamp_state function_slots1 function_slots2;
      value_slots = Value_slot.merge_stamp_state value_slots1 value_slots2;
      symbols = Symbol.merge_stamp_state symbols1 symbols2
    }
end
