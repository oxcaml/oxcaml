(* Merlin only needs the Lambda types and helpers used by the typechecker. *)

type immediate_or_pointer =
  | Immediate
  | Pointer

type boxed_float = Primitive.boxed_float =
  | Boxed_float64
  | Boxed_float32

type boxed_integer = Primitive.boxed_integer =
  | Boxed_int64
  | Boxed_nativeint
  | Boxed_int32

type boxed_vector = Primitive.boxed_vector =
  | Boxed_vec128
  | Boxed_vec256
  | Boxed_vec512

type unboxed_float = Primitive.unboxed_float =
  | Unboxed_float64
  | Unboxed_float32

type unboxed_or_untagged_integer = Primitive.unboxed_or_untagged_integer =
  | Unboxed_int64
  | Unboxed_nativeint
  | Unboxed_int32
  | Untagged_int16
  | Untagged_int8
  | Untagged_int

type unboxed_vector = Primitive.unboxed_vector =
  | Unboxed_vec128
  | Unboxed_vec256
  | Unboxed_vec512

type scannable_product_element_kind =
  | Pint_scannable
  | Paddr_scannable
  | Pproduct_scannable of scannable_product_element_kind list

type ignorable_product_element_kind =
  | Pint_ignorable
  | Punboxedfloat_ignorable of unboxed_float
  | Punboxedvector_ignorable of unboxed_vector
  | Punboxedoruntaggedint_ignorable of unboxed_or_untagged_integer
  | Pproduct_ignorable of ignorable_product_element_kind list

type array_kind =
    Pgenarray | Paddrarray | Pgcignorableaddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray of unboxed_float
  | Punboxedoruntaggedintarray of unboxed_or_untagged_integer
  | Punboxedvectorarray of unboxed_vector
  | Punboxedmaskarray
  | Pgcscannableproductarray of scannable_product_element_kind list
  | Pgcignorableproductarray of ignorable_product_element_kind list
  | Punspecializedarray

type nullable =
  | Nullable
  | Non_nullable

let split_vectors =
  (* The compiler toggles this value based on the target architecture. Since we don't have
     a target architecture, we arbitrarily choose the x86 case. *)
  false

type 'a mixed_block_element =
  | Value of Jkind_types.Scannable_axes.t
  | Float_boxed of 'a
  | Float64
  | Float32
  | Bits8
  | Bits16
  | Bits32
  | Bits64
  | Vec128
  | Vec256
  | Vec512
  | Mask
  | Word
  | Untagged_immediate
  | Product of 'a mixed_block_element array
  | Splice_variable of Jkind_types.Sort.var

type mixed_block_shape = unit mixed_block_element array

type constructor_representation =
  | Constructor_uniform_value
  | Constructor_mixed of mixed_block_shape
  | Constructor_immediate_all_void

type variant_representation =
  | Variant_unboxed
  | Variant_boxed
  | Variant_extensible
  | Variant_with_null

type record_representation =
  | Record_unboxed
  | Record_inlined of
      Types.tag * constructor_representation * variant_representation
  | Record_boxed
  | Record_float
  | Record_ufloat
  | Record_mixed of mixed_block_shape

let rec mixed_block_element_of_types (elt : Types.mixed_block_element) =
  match elt with
  | Scannable axes -> Value axes
  | Float_boxed -> Float_boxed ()
  | Float64 -> Float64
  | Float32 -> Float32
  | Bits8 -> Bits8
  | Bits16 -> Bits16
  | Bits32 -> Bits32
  | Bits64 -> Bits64
  | Vec128 -> Vec128
  | Vec256 -> Vec256
  | Vec512 -> Vec512
  | Mask -> Mask
  | Word -> Word
  | Untagged_immediate -> Untagged_immediate
  | Product shapes -> Product (mixed_block_shape_of_types shapes)
  | Void -> Product [||]
  | Addressable elt -> mixed_block_element_of_types elt

and mixed_block_shape_of_types shape =
  Array.map mixed_block_element_of_types shape

let mixed_block_shape_has_splices shape =
  let rec has_splices : 'a mixed_block_element -> bool = function
    | Splice_variable _ -> true
    | Product shape -> Array.exists has_splices shape
    | Value _ | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16
    | Bits32 | Bits64 | Vec128 | Vec256 | Vec512 | Mask | Word
    | Untagged_immediate -> false
  in
  Array.exists has_splices shape
