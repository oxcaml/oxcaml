type error =
  | Atomic_field_must_be_mutable of
      { loc : Location.t;
        name : string
      }
  | Non_value_atomic_field of Location.t
  | Mutable_field_in_unboxed_record of Location.t
  | Atomic_field_in_pattern of
      { loc : Location.t;
        field : Longident.t
      }
  | Non_atomic_field_access of
      { loc : Location.t;
        field : Longident.t
      }
  | Modalities_on_atomic_field of
      { loc : Location.t;
        field : Longident.t
      }
  | Invalid_atomic_access of Location.t
  | Bad_tail_annotation of
      { loc : Location.t;
        kind : [ `Conflict | `Not_a_tailcall ]
      }

val diagnose : error -> Diagnostic_plan.t list
