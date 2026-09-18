module Make (Key : sig
    type t
  end) =
struct
  let find (k : Key.t) = k
  let consume (_ : Key.t) = ()
end

(* The functor's body is itself an application, the way core/map.ml builds [Make] from
   [Make_using_comparator]. *)
module Make_applied (Key : sig
    type t
  end) =
  Make (Key)
