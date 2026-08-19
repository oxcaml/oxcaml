(* TEST
 flags = "-extension layouts_beta -flambda2-kind-checks";
 flambda2;
 {
   native;
 }
*)

type (_ : any) witness =
  | Int : int witness
  | String : string witness

let[@inline never] poly : type (a : any). a witness -> unit -> a =
  fun w () -> match w with Int -> 42 | String -> "forty-two"

let use_int () = poly Int ()

let use_string () = poly String ()

let () =
  assert (use_int () = 42);
  assert (use_string () = "forty-two");
  ()
