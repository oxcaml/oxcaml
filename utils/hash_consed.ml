(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Simon Spies, Jane Street Europe                       *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   Permission is hereby granted, free of charge, to any person          *)
(*   obtaining a copy of this software and associated documentation       *)
(*   files (the "Software"), to deal in the Software without              *)
(*   restriction, including without limitation the rights to use, copy,   *)
(*   modify, merge, publish, distribute, sublicense, and/or sell copies   *)
(*   of the Software, and to permit persons to whom the Software is       *)
(*   furnished to do so, subject to the following conditions:             *)
(*                                                                        *)
(*   The above copyright notice and this permission notice shall be       *)
(*   included in all copies or substantial portions of the Software.      *)
(*                                                                        *)
(*   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,      *)
(*   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF   *)
(*   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                *)
(*   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS  *)
(*   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN   *)
(*   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN    *)
(*   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE     *)
(*   SOFTWARE.                                                            *)
(*                                                                        *)
(**************************************************************************)

(* The 'tbl is a phantom argument that we use below to associate deduplicated
   elements with their table to ensure we don't get confused between different
   memoization tables. *)
type ('a, 'tbl) t =
  { hash : int;
    value : 'a
  }

let equal e1 e2 = Int.equal e1.hash e2.hash && e1.value == e2.value

let hash e = e.hash

let value e = e.value

module Table (T : sig
  type t

  val initial_size : int

  val hash : t -> int

  val equal : t -> t -> bool
end) : sig
  type tbl

  val create : T.t -> (T.t, tbl) t
end = struct
  type tbl = unit

  module Tbl = Hashtbl.Make (struct
    type nonrec t = (T.t, tbl) t

    let equal t1 t2 =
      (* It is crucial that we use [T.equal] here such that the hash table
         lookup below is up to structural equality. *)
      Int.equal t1.hash t2.hash && T.equal t1.value t2.value

    (* We hash at creation time, so there is no need to recompute the hash using
       [T.hash]. *)
    let hash = hash
  end)

  let table = Tbl.create T.initial_size

  let create value =
    let hash = T.hash value in
    let elem = { hash; value } in
    match Tbl.find_opt table elem with
    | Some existing -> existing
    | None ->
      Tbl.add table elem elem;
      elem
end

module Dedup (T : sig
  type t

  val initial_size : int

  val hash : t -> int

  val equal : t -> t -> bool
end) =
struct
  module D = Table (T)

  type nonrec t = (T.t, D.tbl) t

  let create = D.create

  let equal = equal

  let hash = hash

  let value = value
end
