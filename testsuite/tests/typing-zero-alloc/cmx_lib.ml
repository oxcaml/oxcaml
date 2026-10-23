let[@inline never] catch_and_discard_exceptions f = try f () with _ -> ()

let[@inline never] either f g x b = if b then f x else g x

(* Tail-recursive, *not* optimized into a loop: *)
let[@inline never][@loop never] rec iter_rec ~f = function
  | [] -> ()
  | head :: tail ->
    f head;
    iter_rec ~f tail

(* Tail-recursive, optimized into a loop: *)
let[@inline never][@loop always] rec iter_loop ~f = function
  | [] -> ()
  | head :: tail ->
    f head;
    iter_loop ~f tail

(* Call sites, flipping argument order: *)
let[@inline never] apply f x = f x
let[@inline never] ylppa x f = apply f x

(* Across compilation units: *)
let[@inline never] iter_flipped xs f = (List.iter [@inlined never]) f xs

(* Mutual recursion: *)
let[@inline never] rec ping f = f (); pong f
and[@inline never] pong f = ping f

(* [caml_apply]: *)
let[@inline never] apply2 f x y = f x y
let[@inline never] ylppa2 y x f = apply2 f x y

(* Passing constant symbols to conditionally zero-alloc functions: *)
let[@inline never] fail () = raise Exit
let[@inline never] diverge () = while true do () done
let[@inline never] iter_fail xs = iter_rec ~f:fail xs
let[@inline never] iter_diverge xs = iter_rec ~f:diverge xs
