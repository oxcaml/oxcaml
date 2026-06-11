(* TEST
 flags = "-extension-universe alpha";
 include stdlib_alpha;
 runtime5;
 {
   bytecode;
 }{
   native;
 }
*)

(* Runnable examples for [Stdlib_alpha.Effect_reflection], ported from
   https://github.com/lpw25/effect-reflection examples.ml. Examples that
   need [reify_local] (handled computations closing over other local
   handlers) are omitted: the runtime does not yet support local
   closures running on fibers. *)

module R = Stdlib_alpha.Effect_reflection

(* An integer state effect, handled with a term-interpreting loop. *)

module State = struct
  type 'a t =
    | Get : int t
    | Set : int -> unit t
end

let get h = R.perform (module State) State.Get h

let set h i = R.perform (module State) (State.Set i) h

let handle_state (f : R.Handler(State).t @ local -> 'a) (i : int) : 'a =
  let rec loop (s : int) (t : _ R.Term(State).t) =
    match t with
    | Return x -> x
    | Op (Get, k) -> loop s (k s)
    | Op (Set s', k) -> loop s' (k ())
  in
  loop i (R.reify (module State) f)

let rec iter (f @ local) = function
  | [] -> ()
  | x :: xs ->
    f x;
    iter f xs

let total l =
  handle_state
    (fun h ->
      iter (fun x -> set h (get h + x)) l;
      get h)
    0

(* Reflecting a hand-written term back into a handled computation. *)

let doubled =
  handle_state
    (fun h ->
      let module T = R.Term(State) in
      R.reflect (module State)
        (T.Op (State.Get, fun s ->
           T.Op (State.Set (s * 2), fun () -> T.Return ())))
        h;
      get h)
    21

(* A generator effect, reified to a lazy stream of ints. *)

module Gen = struct
  type 'a t = Gen : int -> unit t
end

let gen h i = R.perform (module Gen) (Gen.Gen i) h

type ints =
  | Finished
  | More of int * (unit -> ints)

let handle_gen (f : R.Handler(Gen).t @ local -> unit) : ints =
  let rec loop (t : _ R.Term(Gen).t) =
    match t with
    | Return () -> Finished
    | Op (Gen i, k) -> More (i, fun () -> loop (k ()))
  in
  loop (R.reify (module Gen) f)

let rec ints_to_list = function
  | Finished -> []
  | More (i, k) -> i :: ints_to_list (k ())

(* An await effect over a toy scheduler. *)

module Deferred = struct
  type 'a t = 'a

  let return x = x

  let bind x f = f x
end

module File = struct
  let read _ = "hello"

  let write _ _ = ()
end

module Await = struct
  type 'a t = Await : 'a Deferred.t -> 'a t
end

let handle_await (f : R.Handler(Await).t @ local -> 'a) : 'a Deferred.t =
  let rec loop (t : _ R.Term(Await).t) =
    match t with
    | Return x -> Deferred.return x
    | Op (Await d, k) -> Deferred.bind d (fun x -> loop (k x))
  in
  loop (R.reify (module Await) f)

let await h d = R.perform (module Await) (Await.Await d) h

let copy_file h src dst =
  let s = await h (File.read src) in
  await h (File.write dst s)

(* Half reflection: handle the right summand of [Sum(L)(Rt)], forwarding
   the left summand's operations to an enclosing handler. *)

module Half_term (L : R.Op) (Rt : R.Op) = struct
  type 'a t =
    | Return : 'a -> 'a t
    | Op : 'r Rt.t * ('r -> R.Handler(L).t @ local -> 'a t) -> 'a t
end

let half_reify :
    (module L : R.Op) -> (module Rt : R.Op) ->
    (R.Handler(L).t @ local -> R.Handler(Rt).t @ local -> 'a) ->
    R.Handler(L).t @ local -> 'a Half_term(L)(Rt).t =
  fun (module L : R.Op) (module Rt : R.Op) f (lh @ local) ->
    let module HT = Half_term(L)(Rt) in
    let rec loop (lh @ local) (t : _ R.Term(R.Sum(L)(Rt)).t) =
      match t with
      | Return v -> HT.Return v
      | Op (Left op, k) -> loop lh (k (R.perform (module L) op lh))
      | Op (Right op, k) ->
        HT.Op (op, fun res lh -> loop lh (k res))
    in
    loop lh
      (R.reify (module R.Sum(L)(Rt)) (fun h ->
         f (R.outl (module L) (module Rt) h)
           (R.outr (module L) (module Rt) h) [@nontail]))

let rec half_reflect :
    (module L : R.Op) -> (module Rt : R.Op) ->
    (R.Handler(L).t @ local -> 'a Half_term(L)(Rt).t) ->
    R.Handler(L).t @ local -> R.Handler(Rt).t @ local -> 'a =
  fun (module L : R.Op) (module Rt : R.Op) f (lh @ local) (rh @ local) ->
    let module HT = Half_term(L)(Rt) in
    match (f lh : _ HT.t) with
    | Return x -> x
    | Op (op, k) ->
      half_reflect (module L) (module Rt)
        (k (R.perform (module Rt) op rh))
        lh rh

(* A generator handled inside an await computation: producing the
   stream requires the enclosing await handler at each step. *)

type aints =
  | AFinished
  | AMore of int * (R.Handler(Await).t @ local -> aints)

let handle_gen_in_await
    (f : R.Handler(Await).t @ local -> R.Handler(Gen).t @ local -> unit)
    (ah : R.Handler(Await).t @ local) : aints =
  let module HT = Half_term(Await)(Gen) in
  let rec loop (t : unit HT.t) =
    match t with
    | Return () -> AFinished
    | Op (Gen i, k) -> AMore (i, fun ah -> loop (k () ah))
  in
  loop (half_reify (module Await) (module Gen) f ah)

(* Polymorphic state via the parameterized [Op1] family. *)

module PState = struct
  type ('a, 's) t =
    | Get : ('s, 's) t
    | Set : 's -> (unit, 's) t
end

let pget h = R.perform1 (module PState) PState.Get h

let pset h s = R.perform1 (module PState) (PState.Set s) h

let phandle_state (type s) (i : s)
    (f : s R.Handler1(PState).t @ local -> 'a) : 'a =
  let rec loop (s : s) (t : (_, s) R.Term1(PState).t) =
    match t with
    | Return x -> x
    | Op (Get, k) -> loop s (k s)
    | Op (Set s', k) -> loop s' (k ())
  in
  loop i (R.reify1 (module PState) f)

let () =
  Printf.printf "total: %d\n" (total [1; 2; 3]);
  Printf.printf "doubled: %d\n" doubled;
  let l = ints_to_list (handle_gen (fun h -> gen h 1; gen h 2; gen h 3)) in
  Printf.printf "gen:%s\n"
    (String.concat "" (List.map (fun i -> " " ^ string_of_int i) l));
  Printf.printf "read: %s\n"
    (handle_await (fun h -> await h (File.read "f")));
  Printf.printf "copy: %b\n"
    (handle_await (fun h -> copy_file h "src" "dst"; true));
  let interleaved =
    handle_await (fun ah ->
      let rec drain (a : aints) acc =
        match a with
        | AFinished -> List.rev acc
        | AMore (i, k) -> drain (k ah) (i :: acc)
      in
      drain
        (handle_gen_in_await
           (fun ah gh ->
             gen gh 1;
             let s = await ah (File.read "f") in
             gen gh (String.length s))
           ah)
        [] [@nontail])
  in
  Printf.printf "gen-in-await:%s\n"
    (String.concat ""
       (List.map (fun i -> " " ^ string_of_int i) interleaved));
  Printf.printf "pstate: %s\n"
    (phandle_state "foo" (fun h ->
       let s = pget h in
       pset h (s ^ "!");
       pget h))
