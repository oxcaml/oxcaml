(* TEST *)

(* a simple test with mutually recursive functions *)
let test =
  let rec even = function
    | 0 -> true
    | n -> odd (n - 1)
  and odd = function
    | 0 -> false
    | n -> even (n - 1)
  in
  List.iter (fun i -> assert (even i <> odd i && even i = (i mod 2 = 0)))
    [0;1;2;3;4;5;6]

(* toplevel recursive functions that build immediate objects *)
let rec countdown n = object
  method value = n
  method next = if n = 0 then None else Some (countdown_twice (n - 1))
end
and countdown_twice n = object
  method value = 2 * n
  method next = if n = 0 then None else Some (countdown (n - 1))
end

let test =
  let rec values o =
    o#value :: (match o#next with None -> [] | Some o -> values o)
  in
  assert (values (countdown 4) = [4; 6; 2; 2; 0])
