let x = ref 0

let () =
  try
    if Dynlink.is_native
    then Dynlink.loadfile "test6_second_plugin.cmxs"
    else Dynlink.loadfile "test6_second_plugin.cmo";
    assert false
  with
  | Dynlink.Error
      (Dynlink.Linking_error (_, Dynlink.Uninitialized_global "Test6_plugin"))
  ->
    ()

let () = x := 1
