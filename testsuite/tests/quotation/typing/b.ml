(* Example from the manual *)

type 'a nested = List of 'a list | Nested of 'a list nested;;

let len nested =
    let map_and_sum f = List.fold_left (fun acc x -> acc + f x) 0 in
    let rec len: 'a. ('a list -> int ) -> 'a nested -> int =
    fun nested_len n ->
      match n with
      | List l -> nested_len l
      | Nested n -> len (map_and_sum nested_len) n
    in
  len List.length nested
