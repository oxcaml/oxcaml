module Ext_name = struct
  let ppat_effect = "ppxlib.migration.ppat_effect_503"
end

let invalid_encoding ~loc name =
  Location.raise_errorf ~loc "Invalid %s encoding" name

module To_502 = struct
  let encode_ppat_effect ~loc ~effect_ ~k =
    let open Ast_502.Parsetree in
    Ppat_extension
      ( Location.{ txt = Ext_name.ppat_effect; loc },
        PPat
          ( {
              ppat_desc = Ppat_tuple [ effect_; k ];
              ppat_attributes = [];
              ppat_loc_stack = [];
              ppat_loc = loc;
            },
            None ) )

  let decode_ppat_effect ~loc payload =
    let open Ast_502.Parsetree in
    match payload with
    | PPat ({ ppat_desc = Ppat_tuple [ effect_; cont ]; _ }, None) ->
        (effect_, cont)
    | _ -> invalid_encoding ~loc Ext_name.ppat_effect
end
