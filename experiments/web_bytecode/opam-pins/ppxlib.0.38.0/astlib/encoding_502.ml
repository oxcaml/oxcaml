module Ext_name = struct
  let ptyp_open = "ppxlib.migration.ptyp_open_502"
end

let invalid_encoding ~loc name =
  Location.raise_errorf ~loc "Invalid %s encoding" name

module To_501 = struct
  open Ast_501.Asttypes
  open Ast_501.Parsetree

  let encode_ptyp_open ~loc ((name, typ) : Longident.t Location.loc * core_type)
      : extension =
    let typ = Ptyp_constr (name, [ typ ]) in
    let ctyp =
      {
        ptyp_desc = typ;
        ptyp_loc = loc;
        ptyp_attributes = [];
        ptyp_loc_stack = [];
      }
    in
    let payload = PTyp ctyp in
    let ext = { txt = Ext_name.ptyp_open; loc } in
    (ext, payload)

  let decode_ptyp_open ~loc = function
    | PTyp { ptyp_desc = Ptyp_constr (name, [ typ ]); _ } -> (name, typ)
    | _ -> invalid_encoding ~loc Ext_name.ptyp_open
end
