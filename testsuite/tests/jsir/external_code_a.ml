(* Paired with external_code.ml: functions that other units inline, whose
   bodies create closures from the code of this unit. *)

let counter = ref 0

let[@inline never] bump n = counter := !counter + n

let[@inline never] get () = !counter

(* The code of the closure uses its environment. *)
let[@inline] make_adder n = fun x -> x + n

(* The code of the closure refers to symbols of this unit, and creates a
   closure itself. *)
let[@inline] make_counter step =
  fun () ->
    bump step;
    fun y -> y + step + get ()

(* The code of the closure does not use its environment once it is
   simplified, but the closure is not lifted. *)
let[@inline] make_first n = fun x -> fst (x, n)

module type S = sig
  type t

  val compare : t -> t -> int
end

module Make (X : S) = struct
  let max a b = if X.compare a b >= 0 then a else b

  let[@inline never] max_list l = List.fold_left max (List.hd l) l

  let id x = x
end
