let static_const_not_supported () =
  Misc.fatal_error "This static_const is not yet supported."

let block_like ~env ~res (const : Flambda2_terms.Static_const.t) :
    Jsir.expr * To_jsir_env.t * To_jsir_result.t =
  match const with
  | Set_of_closures _closures ->
    Misc.fatal_errorf
      "Cannot translate %a: expected a block-like static const, instead found \
       Set_of_closures"
      Flambda2_terms.Static_const.print const
  | Block (tag, mut, _shape, fields) ->
    (* CR selee: is it ok to ignore shape? *)
    let tag = Flambda2_kinds.Tag.Scannable.to_int tag in
    let mutability =
      match mut with
      | Mutable -> Jsir.Maybe_mutable
      | Immutable -> Jsir.Immutable
      | Immutable_unique ->
        (* CR selee: check *)
        Jsir.Immutable
    in
    let fields, res =
      To_jsir_shared.simples ~env ~res
        (List.map Flambda2_term_basics.Simple.With_debuginfo.simple fields)
    in
    Block (tag, Array.of_list fields, NotArray, mutability), env, res
  | Boxed_float32 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_float value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_int32 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_int64 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_nativeint value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_vec128 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_vec256 value ->
    ignore value;
    static_const_not_supported ()
  | Boxed_vec512 value ->
    ignore value;
    static_const_not_supported ()
  | Immutable_float_block values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_float_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_float32_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_int32_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_int64_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_nativeint_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec128_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec256_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_vec512_array values ->
    ignore values;
    static_const_not_supported ()
  | Immutable_value_array values ->
    ignore values;
    static_const_not_supported ()
  | Empty_array kind ->
    ignore kind;
    static_const_not_supported ()
  | Mutable_string { initial_value } ->
    ignore initial_value;
    static_const_not_supported ()
  | Immutable_string value ->
    ignore value;
    static_const_not_supported ()
