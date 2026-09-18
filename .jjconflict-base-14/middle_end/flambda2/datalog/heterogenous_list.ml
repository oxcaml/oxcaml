(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type nil = Nil

module type S = sig
  type 'a t

  type _ hlist =
    | [] : nil hlist
    | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist

  type t_ = Any : 'a t -> t_

  val hlist_to_list : 'a hlist -> t_ list

  val iter_hlist : (t_ -> unit) -> 'a hlist -> unit
end

module Make (X : sig
  type 'a t
end) : S with type 'a t := 'a X.t = struct
  type 'a t = 'a X.t

  type _ hlist =
    | [] : nil hlist
    | ( :: ) : 'a t * 'b hlist -> ('a -> 'b) hlist

  type t_ = Any : 'a t -> t_

  let[@tail_mod_cons] rec hlist_to_list : type t. t hlist -> t_ list = function
    | [] -> []
    | x :: xs -> Any x :: hlist_to_list xs

  let iter_hlist f =
    let rec loop : type t. t hlist -> unit = function
      | [] -> ()
      | x :: xs ->
        (f [@inlined hint]) (Any x);
        loop xs
    in
    loop
end

module Constant = Make (struct
  type 'a t = 'a
end)
