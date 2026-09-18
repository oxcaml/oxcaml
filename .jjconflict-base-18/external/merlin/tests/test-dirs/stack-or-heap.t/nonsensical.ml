(* Various cases where it doesn't make much sense to report a stack-or-heap, even though
   there might be an allocating expression in sight. *)

module M = struct
    (* ^ *)
  fun x = x


   (* ^ *)
end

type t_rec = { x : int }

let f y = { y }
         (* ^ *)

let f () = { x = "OK" }
                (* ^ *)

type t_var = A of int | B of { b : int }

let f x = A { a = x }
               (* ^ *)

let f () = A "OK"
            (* ^ *)

let f () = C "OK"
            (* ^ *)

let f () = D
        (* ^ *)

let f x = x.maybe_a_float_field
          (* ^ *)
