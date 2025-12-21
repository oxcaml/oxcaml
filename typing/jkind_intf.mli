module type Sort =
  sig
    type t
    type base =
        Void
      | Scannable
      | Untagged_immediate
      | Float64
      | Float32
      | Word
      | Bits8
      | Bits16
      | Bits32
      | Bits64
      | Vec128
      | Vec256
      | Vec512
    type var
    module Const :
      sig
        type t = Base of base | Product of t list
        val equal : t -> t -> bool
        val format : Format.formatter -> t -> unit
        val all_void : t -> bool
        val scannable : t
        val void : t
        val float64 : t
        val float32 : t
        val word : t
        val untagged_immediate : t
        val bits8 : t
        val bits16 : t
        val bits32 : t
        val bits64 : t
        val vec128 : t
        val vec256 : t
        val vec512 : t
        module Debug_printers : sig val t : Format.formatter -> t -> unit end
        val for_class_arg : t
        val for_instance_var : t
        val for_lazy_body : t
        val for_tuple_element : t
        val for_variant_arg : t
        val for_boxed_record : t
        val for_block_element : t
        val for_array_get_result : t
        val for_array_comprehension_element : t
        val for_list_element : t
        val for_function : t
        val for_probe_body : t
        val for_poly_variant : t
        val for_object : t
        val for_initializer : t
        val for_method : t
        val for_module : t
        val for_predef_scannable : t
        val for_tuple : t
        val for_idx : t
        val for_loop_index : t
        val for_constructor : t
        val for_boxed_variant : t
        val for_exception : t
        val for_type_extension : t
        val for_class : t
      end
    module Var :
      sig
        type id = private int
        val get_id : var -> id
        val get_print_number : id -> int
        val name : var -> string
      end
    val void : t
    val scannable : t
    val float64 : t
    val float32 : t
    val word : t
    val bits32 : t
    val bits64 : t
    val new_var : level:int -> t
    val of_base : base -> t
    val of_const : Const.t -> t
    val of_var : var -> t
    val equate : t -> t -> bool
    val format : Format.formatter -> t -> unit
    val is_void_defaulting : t -> bool
    val default_to_scannable_and_get : t -> Const.t
    val default_for_transl_and_get : t -> Const.t
    type change
    val undo_change : change -> unit
    module Debug_printers :
      sig
        val base : Format.formatter -> base -> unit
        val t : Format.formatter -> t -> unit
        val var : Format.formatter -> var -> unit
      end
  end
module History :
  sig
    type concrete_creation_reason =
        Match
      | Constructor_declaration of int
      | Label_declaration of Ident.t
      | Record_projection
      | Record_assignment
      | Record_functional_update
      | Let_binding
      | Function_argument
      | Function_result
      | Structure_item_expression
      | External_argument
      | External_result
      | Statement
      | Optional_arg_default
      | Layout_poly_in_external
      | Unboxed_tuple_element
      | Peek_or_poke
      | Old_style_unboxed_type
      | Array_element
      | Idx_element
      | Structure_item
      | Signature_item
    type concrete_legacy_creation_reason =
        Unannotated_type_parameter of Path.t
      | Wildcard
      | Unification_var
    type 'd annotation_context =
        Type_declaration :
          Path.t -> (Allowance.allowed * 'r) annotation_context
      | Type_parameter : Path.t *
          string option -> (Allowance.allowed * Allowance.allowed)
                           annotation_context
      | Newtype_declaration :
          string -> (Allowance.allowed * Allowance.allowed)
                    annotation_context
      | Constructor_type_parameter : Path.t *
          string -> (Allowance.allowed * Allowance.allowed)
                    annotation_context
      | Existential_unpack :
          string -> (Allowance.allowed * Allowance.allowed)
                    annotation_context
      | Univar :
          string -> (Allowance.allowed * Allowance.allowed)
                    annotation_context
      | Type_variable :
          string -> (Allowance.allowed * Allowance.allowed)
                    annotation_context
      | Type_wildcard :
          Warnings.loc -> (Allowance.allowed * Allowance.allowed)
                          annotation_context
      | Type_of_kind :
          Warnings.loc -> (Allowance.allowed * Allowance.allowed)
                          annotation_context
      | With_error_message : string *
          'd annotation_context -> 'd annotation_context
    and annotation_context_l =
        (Allowance.allowed * Allowance.disallowed) annotation_context
    and annotation_context_r =
        (Allowance.disallowed * Allowance.allowed) annotation_context
    and annotation_context_lr =
        (Allowance.allowed * Allowance.allowed) annotation_context
    type value_or_null_creation_reason =
        Primitive of Ident.t
      | Tuple_element
      | Separability_check
      | Polymorphic_variant_field
      | V1_safety_check
      | Probe
      | Captured_in_object
      | Let_rec_variable of Ident.t
      | Type_argument of { parent_path : Path.t; position : int; arity : int;
        }
    type value_creation_reason =
        Class_let_binding
      | Object
      | Instance_variable
      | Object_field
      | Class_field
      | Boxed_record
      | Boxed_variant
      | Extensible_variant
      | Primitive of Ident.t
      | Type_argument of { parent_path : Path.t; position : int; arity : int;
        }
      | Tuple
      | Row_variable
      | Polymorphic_variant
      | Polymorphic_variant_too_big
      | Arrow
      | Tfield
      | Tnil
      | First_class_module
      | Univar
      | Default_type_jkind
      | Existential_type_variable
      | Idx_base
      | Array_comprehension_element
      | List_comprehension_iterator_element
      | Array_comprehension_iterator_element
      | Lazy_expression
      | Class_type_argument
      | Class_term_argument
      | Debug_printer_argument
      | Recmod_fun_arg
      | Quotation_result
      | Antiquotation_result
      | Tquote
      | Tsplice
      | Array_type_kind
      | Unknown of string
    type immediate_creation_reason =
        Empty_record
      | Enumeration
      | Primitive of Ident.t
      | Immediate_polymorphic_variant
    type immediate_or_null_creation_reason = Primitive of Ident.t
    type scannable_creation_reason = Dummy_jkind
    type void_creation_reason = |
    type any_creation_reason =
        Missing_cmi of Path.t
      | Initial_typedecl_env
      | Dummy_jkind
      | Type_expression_call
      | Inside_of_Tarrow
      | Wildcard
      | Unification_var
      | Array_type_argument
      | Type_argument of { parent_path : Path.t; position : int; arity : int;
        }
    type product_creation_reason = Unboxed_tuple | Unboxed_record
    type creation_reason =
        Annotated : ('l * 'r) annotation_context *
          Warnings.loc -> creation_reason
      | Missing_cmi of Path.t
      | Value_or_null_creation of value_or_null_creation_reason
      | Value_creation of value_creation_reason
      | Immediate_creation of immediate_creation_reason
      | Immediate_or_null_creation of immediate_or_null_creation_reason
      | Scannable_creation of scannable_creation_reason
      | Void_creation of void_creation_reason
      | Any_creation of any_creation_reason
      | Product_creation of product_creation_reason
      | Concrete_creation of concrete_creation_reason
      | Concrete_legacy_creation of concrete_legacy_creation_reason
      | Primitive of Ident.t
      | Unboxed_primitive of Ident.t
      | Imported
      | Imported_type_argument of { parent_path : Path.t; position : int;
          arity : int;
        }
      | Generalized of Ident.t option * Warnings.loc
      | Abbreviation
    type interact_reason =
        Gadt_equation of Path.t
      | Tyvar_refinement_intersection
      | Subjkind
  end
