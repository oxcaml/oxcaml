(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

[@@@ocaml.flambda_o3]

external (+) : int -> int -> int = "%addint"

module F(X : sig val x : int end) = struct
  let f =
    let[@inline never][@local never] id f = f in
    let[@inline never][@local never] rec fail () = (id fail) () in
    let x = fail () in
    fun y -> x + X.x + y
end 