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

(* Mutual recursion through [apply]: *)
let[@inline never] rec aping f = f (); apply apong f
and[@inline never] apong f = apply aping f

(* [caml_apply]: *)
let[@inline never] apply2 f x y = f x y
let[@inline never] ylppa2 y x f = apply2 f x y
let[@inline never] over f x y = apply f x y
let[@inline never] over2 f x y z = apply2 f x y z
let[@inline never] partial f x = apply2 f x

(* arity: *)
let[@inline never] arity_1_expected_1 i = apply Int.neg i
let[@inline never] arity_2_expected_1 i = apply Int.add i
let[@inline never] arity_2_expected_2 i j = apply2 Int.add i j
let[@inline never] arity_2_expected_2_partial i = apply2 Int.add i

(* Passing constant symbols to conditionally zero-alloc functions: *)
let[@inline never] fail () = raise Exit
let[@inline never] diverge () = while true do () done
let[@inline never] iter_fail xs = iter_rec ~f:fail xs
let[@inline never] iter_diverge xs = iter_rec ~f:diverge xs

(* Closure symbols, code symbols, and builtins are all distinct: *)
let[@inline never] noop _ = ()
let[@inline never] alloc x = ignore (Sys.opaque_identity (ref x))
let[@inline never] iter_ignore xs = (List.iter [@inlined never]) ignore xs
let[@inline never] iter_noop xs = (List.iter [@inlined never]) noop xs
let[@inline never] iter_alloc xs = (List.iter [@inlined never]) alloc xs

(* Closure symbols to code symbols across compilation units: *)
let[@inline never] exists_some xs =
  (List.exists [@inlined never]) Option.is_some xs

(* Closure symbols to code symbols across compilation units in recursive
   functions: *)
let[@inline never][@loop never] rec self_cross xs =
  ignore ((List.exists [@inlined never]) Option.is_some xs);
  self_cross xs

(* Closure symbols to code symbols across compilation units in mutually
   recursive functions: *)
let[@inline never] rec cycle_a xs =
  ignore ((List.exists [@inlined never]) Option.is_some xs);
  cycle_b xs
and[@inline never] cycle_b xs = cycle_a xs
