(* Semantic mode failures.  Conversion from compiler exceptions belongs in
   [Diagnostics]. *)

type mismatch_step =
  { mode : Mode.Reported_mode.t;
    pinpoint : Mode.Hint.pinpoint;
    kind : Mode.Reported_hint.t
  }

type expression_error =
  | Submode_failed of
      { error : Mode.Value.error;
        context : Typecore.submode_reason
      }
  | Curried_application_complete of
      { label : Typedtree.arg_label;
        error : Mode.Alloc.error;
        part : [`Prefix | `Single_arg | `Entire_apply]
      }
  | Function_mode_mismatch of
      { part : Typecore.mode_mismatch_kind;
        direction : Mode.equate_step;
        error : Mode.Alloc.error
      }
  | Uncurried_function_escapes_comonadic of Mode.Alloc.Comonadic.error
  | Overwrite_of_invalid_term
  | Block_index_modality_mismatch of
      { mutable_elements : bool;
        error : Mode.Modality.equate_error
      }
  | Exclave_in_nontail_position
  | Exclave_returns_not_local
  | Tail_call_local_returning
  | Always_heap_allocation of Typecore.always_heap_allocation
  | Always_static_allocation of Typecore.always_static_allocation
  | Not_allocation

type error =
  | Expression_error of
      { loc : Location.t;
        error : expression_error
      }
  | Constructor_submode_failed of
      { loc : Location.t;
        error : Mode.Value.error
      }
  | Local_value_used_in_exclave of
      { loc : Location.t;
        description : Mode.Hint.pinpoint_desc
      }
  | Mutable_value_used_in_closure of
      { loc : Location.t;
        pinpoint : Mode.Hint.pinpoint
      }
  | Unique_use_during_borrowing of
      Uniqueness_analysis.Usage.unique_use_during_borrowing_error
  | Uniqueness_error of Uniqueness_analysis.error
  | Folded_mismatch of mismatch_step list Mode.folded_axis list

type modality_subject =
  | Modality_item of string
  | Modality_field of string
  | Modality_constructor_arg of
      { constructor : string;
        index : int
      }

type modality_side =
  { atom : Mode.Modality.atom option;
    loc : Location.t option
  }

type modality_requirement =
  | Exact_match
  | At_least_as_strong

type modality_input =
  { axis : Mode.Value.Axis.packed;
    subject : modality_subject;
    expected : modality_side;
    actual : modality_side;
    requirement : modality_requirement
  }

val modality_story :
  sides:Diagnostic_term.sides ->
  modality_input ->
  Diagnostic_term.t Diagnostic_nlg.story

val mode_error_stories :
  error_loc:Location.t ->
  ?expected_declaration:Types.value_description ->
  Mode.Hint.pinpoint ->
  Mode.Value.error ->
  Diagnostic_term.t Diagnostic_nlg.story list

val diagnose : loc:Location.t -> error -> Structured_diagnostic.t option
