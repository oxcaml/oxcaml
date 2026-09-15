let app (type b) f x =
  let _ : b = f x in
  let _ : b = f x in
  ()
;;
