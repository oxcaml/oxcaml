
let[@inline never] foo () = 42

let[@inline always] bar () =
  if Sys.opaque_identity true then 13 else 42

let[@inline always] loop () =
  let r = ref 0 in
  for i = 0 to Sys.opaque_identity 5 do
    r := !r + i
  done;
  !r

let field1 = foo ()
let field2 = bar ()
let field3 = loop ()
