(* TEST
 include ocamlcommon;
 expect;
*)

let () =
  let payload =
    Btype.newgenvar
      (Btype.Jkind0.Builtin.value ~why:Jkind_intf.History.Row_variable)
  in
  let result =
    Btype.Jkind0.for_variant_with_null_result
      (Path.Pident (Ident.create_local "custom_or_null"))
      payload
  in
  Format.printf "%a@."
    (Format_doc.compat
       (Jkind.format_history
          ~intro:(fun ppf ->
            Format_doc.fprintf ppf "The kind of custom_or_null is")
          Env.empty))
    result
;;

[%%expect{|
The kind of custom_or_null is
  because the payload of custom_or_null determines its kind value_or_null.
|}]
