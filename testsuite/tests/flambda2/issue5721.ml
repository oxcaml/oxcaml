(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -flambda2-join-points -flambda2-join-algorithm=n-way";
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte with dump-simplify;
   check-fexpr-dump;
 *)

type 'a t =
  | Left_input 
  | Right_input 
  | New_result of 'a 
let main ~f x =
  let _ =
    match x with
    | Left_input -> ()
    | Right_input -> f ()
    | New_result _ -> ()
  in
  (* We expect that the [%is_int] primitive on [x] should be eliminated here,
     because we already know its value from the previous [match x]. *)
  match x with
  | Left_input -> false
  | Right_input -> false
  | New_result _ -> true
