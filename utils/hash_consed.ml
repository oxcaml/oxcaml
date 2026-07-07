(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Simon Spies, Jane Street Europe                       *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
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

module type S = sig
  type elt

  type t

  type table

  val create_table : initial_size:int -> table

  val create : table -> elt -> t

  val value : t -> elt

  val hash : t -> int

  val equal : t -> t -> bool

  val canonical : table -> elt -> elt

  val modify : table -> t -> (elt -> elt) -> t
end

module Dedup (H : Hashtbl.HashedType) () = struct
  type elt = H.t

  type t =
    { hash : int;
      value : H.t
    }

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal t1 t2 =
      (* It is crucial that we use [H.equal] here such that the hash table
         lookup below is up to structural equality. *)
      Int.equal t1.hash t2.hash && H.equal t1.value t2.value

    (* We hash at creation time, so there is no need to recompute the hash using
       [H.hash]. *)
    let hash t = t.hash
  end)

  type table = t Tbl.t

  let create_table ~initial_size = Tbl.create initial_size

  let create table value =
    let hash = H.hash value in
    let elem = { hash; value } in
    match Tbl.find_opt table elem with
    | Some existing -> existing
    | None ->
      Tbl.add table elem elem;
      elem

  let value e = e.value

  let hash e = e.hash

  let equal e1 e2 = Int.equal e1.hash e2.hash && e1.value == e2.value

  let canonical table value = (create table value).value

  let modify table e f = create table (f e.value)
end
