(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* More functions made to return at mode local (see [escape_exclave.ml]):
   where their own region is closed when the control flow is not linear, and
   cases where they cannot be given one. *)
external local_stack_offset : unit -> int = "caml_local_stack_offset"

type triple =
  { a : int;
    b : int;
    c : int
  }

let[@inline never] sum3 t = t.a + t.b + t.c

let[@inline never] sum_pair (p : int * int) = fst p + snd p

exception Negative

let[@inline never] sum3_exn t = if t.a < 0 then raise Negative else sum3 t

let count = ref 0

let base = ref 0

let[@inline never] used () = local_stack_offset () - !base

module M : sig
  val run : int -> unit
end = struct
  (* The result is allocated on one path only, and both paths then go through
     the same code: the region is closed at the start of the path allocating
     the result, and before the jump ending the other path. *)
  let[@inline never] join x y =
    let t = { a = x; b = y; c = x } in
    let s = sum3 t in
    let p =
      if s > 10
      then s, 1
      else (
        incr count;
        0, 0)
    in
    let u = sum_pair p [@nontail] in
    Some (p, u)

  (* No region: here the other path is a [Switch] arm jumping to the common
     code, before which nothing can be inserted. *)
  let[@inline never] join_from_switch x y =
    let t = { a = x; b = y; c = x } in
    let s = sum3 t in
    let p = if s > 10 then s, 1 else 0, 0 in
    let u = sum_pair p [@nontail] in
    Some (p, u)

  (* The temporary is used in a loop, which is over before the result is
     allocated. *)
  let[@inline never] with_loop x n =
    let t = { a = x; b = n; c = 1 } in
    let acc = ref 0 in
    for i = 1 to n do
      acc := !acc + sum3 t + i
    done;
    Some !acc

  (* The same when the loop has a region of its own, for the temporary
     allocated at each iteration. *)
  let[@inline never] with_allocating_loop x n =
    let t = { a = x; b = n; c = 1 } in
    let acc = ref 0 in
    for i = 1 to n do
      let u = { a = i; b = !acc; c = sum3 t } in
      acc := sum3 u land 0xffff
    done;
    Some !acc

  (* No region: the function has an exception handler. *)
  let[@inline never] with_try x y =
    let t = { a = x; b = y; c = x } in
    let s = try sum3_exn t with Negative -> 0 in
    Some (s, y)

  (* No region: the temporary is given to a function whose result, which is
     allocated in the region current at the call, is returned. *)
  let[@inline never] make t = Some (t.a + t.b, t.c)

  let[@inline never] temporary_to_callee x y =
    let t = { a = x; b = y; c = x } in
    let s = sum3 t in
    let p = make t in
    Some (p, s)

  let[@inline never] value o = match o with None -> -1 | Some v -> v

  let[@inline never] value2 o =
    match o with None -> -1 | Some (p, u) -> sum_pair p + u

  let[@inline never] value3 o =
    match o with
    | None | Some (None, _) -> -1
    | Some (Some p, s) -> sum_pair p + s

  let[@inline never] run n =
    base := local_stack_offset ();
    let j1 = join n 1 in
    let j2 = join 1 1 in
    let u1 = used () in
    let j3 = join_from_switch n 1 in
    let j4 = join_from_switch 1 1 in
    let l = with_loop n 5 in
    let l2 = with_allocating_loop n 5 in
    let u2 = used () in
    let t1 = with_try n 2 in
    let t2 = with_try (-1) 2 in
    let c = temporary_to_callee n 3 in
    (Printf.printf [@inlined never])
      "%d %d %d %d %d %d %d %d %d; count %d; used %d %d\n" (value2 j1)
      (value2 j2) (value2 j3) (value2 j4) (value l) (value l2)
      (value (Option.map sum_pair t1))
      (value (Option.map sum_pair t2))
      (value3 c) !count u1 (u2 - u1)
end

let () = M.run 10
