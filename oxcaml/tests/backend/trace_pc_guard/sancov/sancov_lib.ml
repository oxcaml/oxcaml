(* The functions named [hit_*] are executed and those named [missed_*] are not.
   Most are markers called from one path of a larger function, so the report of
   [sancov] shows which paths the coverage data says were taken. *)

let count = ref 0

let[@inline never] hit_even () = incr count

let[@inline never] missed_huge () = incr count

let[@inline never] hit_found () = incr count

let[@inline never] missed_absent () = incr count

let[@inline never] hit_zero () = incr count

let[@inline never] missed_one () = incr count

let[@inline never] hit_two () = incr count

let[@inline never] missed_many () = incr count

let[@inline never] hit_below () = incr count

let[@inline never] missed_above () = incr count

let[@inline never] hit_small () = incr count

let[@inline never] missed_large () = incr count

let[@inline never] hit_base () = incr count

let[@inline never] hit_step () = incr count

let[@inline never] hit_closure () = incr count

let[@inline never] missed_closure () = incr count

let[@inline never] hit_loop n =
  for i = 1 to n do
    if i land 1 = 0 then hit_even () else if i > 1000 then missed_huge ()
  done

exception Found of int

let[@inline never] hit_search a x =
  match Array.iteri (fun i y -> if y = x then raise (Found i)) a with
  | () -> missed_absent ()
  | exception Found _ -> hit_found ()

let[@inline never] hit_classify n =
  match n with
  | 0 -> hit_zero ()
  | 1 -> missed_one ()
  | 2 -> hit_two ()
  | _ -> missed_many ()

module Make (X : sig
  val limit : int
end) =
struct
  let[@inline never] hit_functor n =
    if n < X.limit then hit_below () else missed_above ()
end
[@@inline never]

let[@inline always] inlined_choice n =
  if n > 10 then missed_large () else hit_small ()

let[@inline never] hit_caller n =
  inlined_choice n;
  inlined_choice (n - 1)

let rec hit_rec n =
  if n <= 0
  then hit_base ()
  else (
    hit_step ();
    hit_rec (n - 1))

let[@inline never] missed_iter f = List.iter f [1; 2; 3]
