(* TEST
   flags = "-O3 -extension-universe beta";
   native;
*)

let[@inline] id f = fun x -> f x;;

let () =
  id
    (fun value ->
      match value with
      | Null -> ()
      | This _ -> assert false)
    Null
;;

(* This covers [Null] through generic array creation; the dynamic float-array
   check must not inspect a null header. *)
let[@inline never] local_array x = [| x |]

let () =
  let x = local_array Null in
  ignore (x : int or_null array)
;;
