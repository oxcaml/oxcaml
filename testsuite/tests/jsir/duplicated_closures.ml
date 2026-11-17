(* TEST
 flambda2;
 {
   native;
 }{
   jsir;
 }
*)

(* Js_of_ocaml inlines [Make2] into [Make], which duplicates its body. The JSIR
   variables standing for the value slots of [map] must be bound so that each copy
   gets its own variables. *)

module type Basic = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module [@inline] Make2 (X : Basic) = struct
  include X

  let map f m = bind m (fun a -> return (f a))
end

module Make (X : Basic) = Make2 (X)

module M = Make (struct
  type 'a t = 'a

  let return x = x

  let bind x f = f x
end)

let () = Printf.printf "%d\n" (M.map (fun x -> x + 1) 41)
