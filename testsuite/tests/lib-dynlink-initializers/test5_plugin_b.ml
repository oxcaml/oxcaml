let () =
  if Dynlink.is_native
  then Dynlink.loadfile "test5_second_plugin.cmxs"
  else Dynlink.loadfile "test5_second_plugin.cmo"
