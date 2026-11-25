(* [make] is made to return at mode local. Its result types, which are exported
   with this unit, must not keep saying that the pair it returns is allocated
   on the heap. [run] is inlined in the other unit, where the call to [make]
   then appears. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val run : int -> int
end = struct
  let[@inline never] make x = x, x + 1

  let run x =
    let p = make x in
    (sum_pair p [@nontail])
end

let run = M.run
