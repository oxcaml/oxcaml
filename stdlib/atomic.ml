(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

<<<<<<< HEAD
type (!'a : value_or_null) t : sync_data with 'a =
  { mutable contents : 'a [@atomic] }

external make
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "%makemutable"
||||||| 23e84b8c4d
type !'a t

external make : 'a -> 'a t = "%makemutable"
external make_contended : 'a -> 'a t = "caml_atomic_make_contended"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
external ignore : 'a -> unit = "%ignore"
=======
external ignore : 'a -> unit = "%ignore"
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

<<<<<<< HEAD
external make_contended
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "caml_atomic_make_contended"

external get
  : ('a : value_or_null).
  'a t @ local -> 'a
  @@ portable
  = "%atomic_load"

external set
  : ('a : value_or_null).
  'a t @ local -> 'a -> unit
  @@ portable
  = "%atomic_set"

external exchange
  : ('a : value_or_null).
  'a t @ local -> 'a -> 'a
  @@ portable
  = "%atomic_exchange"

external compare_and_set
  : ('a : value_or_null).
  'a t @ local -> 'a -> 'a -> bool
  @@ portable
  = "%atomic_cas"

external compare_exchange
  : ('a : value_or_null).
  'a t @ local -> 'a -> 'a -> 'a
  @@ portable
  = "%atomic_compare_exchange"

external fetch_and_add
  :  int t @ local
  -> int
  -> int
  @@ portable
  = "%atomic_fetch_add"

external add
  :  int t @ local
  -> int
  -> unit
  @@ portable
  = "%atomic_add"

external sub
  :  int t @ local
  -> int
  -> unit
  @@ portable
  = "%atomic_sub"

external logand
  :  int t @ local
  -> int
  -> unit
  @@ portable
  = "%atomic_land"

external logor
  :  int t @ local
  -> int
  -> unit
  @@ portable
  = "%atomic_lor"

external logxor
  :  int t @ local
  -> int
  -> unit
  @@ portable
  = "%atomic_lxor"

let incr r = add r 1
let decr r = sub r 1

external get_contended
  : ('a : value_or_null).
  'a t @ contended local -> 'a @ contended
  @@ portable
  = "%atomic_load"

module Loc = struct
  type ('a : value_or_null) t : sync_data with 'a = 'a atomic_loc
  external get : ('a : value_or_null).
    'a t @ local -> 'a @@ portable = "%atomic_load_loc"
  external set : ('a : value_or_null).
    'a t @ local -> 'a -> unit @@ portable = "%atomic_set_loc"
  external exchange : ('a : value_or_null).
    'a t @ local -> 'a -> 'a @@ portable = "%atomic_exchange_loc"
  external compare_and_set : ('a : value_or_null).
    'a t @ local -> 'a -> 'a -> bool @@ portable = "%atomic_cas_loc"
  external compare_exchange : ('a : value_or_null).
    'a t @ local -> 'a -> 'a -> 'a @@ portable = "%atomic_compare_exchange_loc"

  external fetch_and_add
    : int t @ local -> int -> int @@ portable
    = "%atomic_fetch_add_loc"

  external add
    : int t @ local -> int -> unit @@ portable = "%atomic_add_loc"

  external sub
    : int t @ local -> int -> unit @@ portable = "%atomic_sub_loc"

  external logand
    : int t @ local -> int -> unit @@ portable = "%atomic_land_loc"

  external logor
    : int t @ local -> int -> unit @@ portable = "%atomic_lor_loc"

  external logxor
    : int t @ local -> int -> unit @@ portable = "%atomic_lxor_loc"

  let incr t = add t 1
  let decr t = sub t 1

  external get_contended : ('a : value_or_null).
    'a t @ contended local -> 'a @ contended @@ portable = "%atomic_load_loc"
end
||||||| 23e84b8c4d
let set r x = ignore (exchange r x)
let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
=======
module Loc = struct
  type 'a t = 'a atomic_loc

  external get : 'a t -> 'a = "%atomic_load_loc"
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange_loc"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas_loc"
  external fetch_and_add : int t -> int -> int = "%atomic_fetch_add_loc"

  let set t v =
    ignore (exchange t v)
  let incr t =
    ignore (fetch_and_add t 1)
  let decr t =
    ignore (fetch_and_add t (-1))
end

type !'a t =
  { mutable contents: 'a [@atomic];
  }

let make v =
  { contents = v }

external make_contended : 'a -> 'a t = "caml_atomic_make_contended"

let get t =
  t.contents
let set t v =
  t.contents <- v

let exchange t v =
  Loc.exchange [%atomic.loc t.contents] v
let compare_and_set t old new_ =
  Loc.compare_and_set [%atomic.loc t.contents] old new_
let fetch_and_add t incr =
  Loc.fetch_and_add [%atomic.loc t.contents] incr
let incr t =
  Loc.incr [%atomic.loc t.contents]
let decr t =
  Loc.decr [%atomic.loc t.contents]
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
