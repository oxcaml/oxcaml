type conv_result =
  { unit : Flambda_unit.t;
    code_slot_offsets : Slot_offsets.t Code_id.Map.t
  }

val conv : Compilation_unit.t -> Fexpr.flambda_unit -> conv_result
