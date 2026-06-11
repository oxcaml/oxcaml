(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Leo White, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Op = sig
  type 'a t
end

module Term (O : Op) = struct
  type 'a t =
    | Return : 'a @@ global -> 'a t
    | Op : 'r O.t @@ global * ('r -> 'a t) -> 'a t
end

module Handler (O : Op) = struct
  type t = { perform : 'r. 'r O.t -> 'r }
end

let reify : (module O : Op) -> (Handler(O).t @ local -> 'a) -> 'a Term(O).t =
  fun (module O : Op) f ->
    let open Handler(O) in
    let open Term(O) in
    let module Eff = struct
      type 'a Effect.t += C : 'a O.t -> 'a Effect.t
    end in
    let handler = { perform = (fun o -> Effect.perform (Eff.C o)) } in
    Effect.Deep.match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc =
          (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Eff.C o ->
              Some
                (fun (k : (c, _) Effect.Deep.continuation) ->
                  Op (o, fun x -> Effect.Deep.continue k x))
            | _ -> None) }

let perform : (module O : Op) -> 'a O.t -> Handler(O).t @ local -> 'a =
  fun (module O : Op) op (h @ local) ->
    let open Handler(O) in
    h.perform op

let rec reflect :
    (module O : Op) -> 'a Term(O).t -> Handler(O).t @ local -> 'a =
  fun (module O : Op) t (h @ local) ->
    let open Term(O) in
    match t with
    | Return x -> x
    | Op (op, k) -> reflect (module O) (k (perform (module O) op h)) h

module Sum (L : Op) (R : Op) = struct
  type 'a t =
    | Left : 'a L.t -> 'a t
    | Right : 'a R.t -> 'a t
end

let outl :
    (module L : Op) -> (module R : Op) ->
    Handler(Sum(L)(R)).t @ local -> Handler(L).t @ local =
  fun (module L : Op) (module R : Op) (h @ local) -> exclave_
    let open Sum(L)(R) in
    let ({ perform = p } : Handler(Sum(L)(R)).t) = h in
    let open Handler(L) in
    { perform = (fun op -> p (Left op)) }

let outr :
    (module L : Op) -> (module R : Op) ->
    Handler(Sum(L)(R)).t @ local -> Handler(R).t @ local =
  fun (module L : Op) (module R : Op) (h @ local) -> exclave_
    let open Sum(L)(R) in
    let ({ perform = p } : Handler(Sum(L)(R)).t) = h in
    let open Handler(R) in
    { perform = (fun op -> p (Right op)) }

module type Op1 = sig
  type ('a, 'e) t
end

module Term1 (O : Op1) = struct
  type ('a, 'e) t =
    | Return : 'a @@ global -> ('a, 'e) t
    | Op : ('r, 'e) O.t @@ global * ('r -> ('a, 'e) t) -> ('a, 'e) t
end

module Handler1 (O : Op1) = struct
  type 'e t = { perform : 'r. ('r, 'e) O.t -> 'r }
end

let reify1 :
    (module O : Op1) ->
    ('e Handler1(O).t @ local -> 'a) -> ('a, 'e) Term1(O).t =
  fun (module O : Op1) (type e) (f : e Handler1(O).t @ local -> _) ->
    let open Handler1(O) in
    let open Term1(O) in
    let module Eff = struct
      type 'a Effect.t += C : ('a, e) O.t -> 'a Effect.t
    end in
    let handler = { perform = (fun o -> Effect.perform (Eff.C o)) } in
    Effect.Deep.match_with f handler
      { retc = (fun v -> Return v);
        exnc = raise;
        effc =
          (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Eff.C o ->
              Some
                (fun (k : (c, _) Effect.Deep.continuation) ->
                  Op (o, fun x -> Effect.Deep.continue k x))
            | _ -> None) }

let perform1 :
    (module O : Op1) -> ('a, 'e) O.t -> 'e Handler1(O).t @ local -> 'a =
  fun (module O : Op1) op (h @ local) ->
    let open Handler1(O) in
    h.perform op

let rec reflect1 :
    (module O : Op1) ->
    ('a, 'e) Term1(O).t -> 'e Handler1(O).t @ local -> 'a =
  fun (module O : Op1) t (h @ local) ->
    let open Term1(O) in
    match t with
    | Return x -> x
    | Op (op, k) -> reflect1 (module O) (k (perform1 (module O) op h)) h
