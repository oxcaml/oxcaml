(* TEST
   flags = "-O3 -extension-universe beta";
   native;
*)

let[@inline] id f x = f x

let () =
  id (fun value -> match value with Null -> () | This _ -> assert false) Null
