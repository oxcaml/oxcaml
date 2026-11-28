(* TEST
 stack-checks;
 native;
*)

(* A function whose frame does not fit in its stack asks the runtime for a
   larger one, giving the space it needs in words. The arm64 emitter used to
   give the size of the frame in bytes: about eight times too much for a
   large frame, which then got [Stack_overflow] under a stack limit that it
   fits well within.

   Fibers start with small stacks, so [run] below, whose frame is about 10k
   bytes on both arm64 and amd64, has to grow a new fiber's stack at once. A
   stack can be doubled as long as it is smaller than [stack_limit] words:
   that allows for [run] and its callees, which need about 2000 words, and
   not for a request of 10k words. *)

type t0 =
  #{ a : int; b : int; c : int; d : int; e : int; f : int; g : int; h : int }

type t1 =
  #{ a : t0; b : t0; c : t0; d : t0; e : t0; f : t0; g : t0; h : t0 }

type t2 =
  #{ a : t1; b : t1; c : t1; d : t1; e : t1; f : t1; g : t1; h : t1 }

let[@inline never] mk0 x : t0 =
  #{ a = x; b = x + 1; c = x + 2; d = x + 3; e = x + 4; f = x + 5; g = x + 6;
     h = x + 7 }

let[@inline never] mk1 x : t1 =
  let y = mk0 x in
  #{ a = y; b = y; c = y; d = y; e = y; f = y; g = y; h = y }

let[@inline never] mk2 x : t2 =
  let y = mk1 x in
  #{ a = y; b = y; c = y; d = y; e = y; f = y; g = y; h = y }

let[@inline never] sum0 (x : t0) =
  x.#a + x.#b + x.#c + x.#d + x.#e + x.#f + x.#g + x.#h

let[@inline never] sum1 (x : t1) =
  sum0 x.#a + sum0 x.#b + sum0 x.#c + sum0 x.#d + sum0 x.#e + sum0 x.#f
  + sum0 x.#g + sum0 x.#h

let[@inline never] sum2 (x : t2) =
  sum1 x.#a + sum1 x.#b + sum1 x.#c + sum1 x.#d + sum1 x.#e + sum1 x.#f
  + sum1 x.#g + sum1 x.#h

let[@inline never] ping () = ()

(* The 512 words of [x] are returned by [mk2], live across the call to [ping]
   and passed to [sum2]. *)
let[@inline never] run () =
  let x = mk2 (Sys.opaque_identity 0) in
  ping ();
  sum2 x

let stack_limit = 4096

let () =
  let limit = (Gc.get ()).stack_limit in
  Gc.set { (Gc.get ()) with stack_limit };
  let sum = Effect.Deep.try_with run () { effc = (fun _ -> None) } in
  Gc.set { (Gc.get ()) with stack_limit = limit };
  Printf.printf "%d\n" sum
