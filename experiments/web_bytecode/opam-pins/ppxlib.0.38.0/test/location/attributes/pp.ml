open Ppxlib

let pp_attr str =
  let iter =
    object
      inherit Ast_traverse.iter as super

      method! attribute v =
        let loc = loc_of_attribute v in
        Format.printf "%a %s" Location.print loc v.attr_name.txt;
        super#attribute v
    end
  in
  iter#structure str;
  str

let () = Driver.register_transformation ~impl:pp_attr "print-attributes"
let () = Ppxlib.Driver.standalone ()
