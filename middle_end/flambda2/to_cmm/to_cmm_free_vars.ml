(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2022 OCamlPro SAS                                    *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Name modes for occurrences *)
module Mode = struct
  type t =
    | Normal
    | Phantom

  let print ppf = function
    | Normal -> Format.fprintf ppf "Normal"
    | Phantom -> Format.fprintf ppf "Phantom"

  let union t1 t2 =
    match t1, t2 with
    | Phantom, Phantom -> Phantom
    | _, Normal | Normal, _ -> Normal
end

module M = Backend_var.Map

type t = Mode.t M.t

let print ppf t = M.print Mode.print ppf t

(* Creation/modification *)

let empty = M.empty

let add ~mode v t =
  M.update v
    (function None -> Some mode | Some mode' -> Some (Mode.union mode mode'))
    t

let singleton ~mode v = M.singleton v mode

let union t1 t2 = M.union_merge Mode.union t1 t2

let remove v t = M.remove v t

(* Inspection *)

let is_empty = M.is_empty

let mode v t = M.find_opt v t

let mem ~mode v (t : t) =
  match M.find_opt v t with
  | None -> false
  | Some Normal -> true
  | Some Phantom -> (
    match (mode : Mode.t) with Phantom -> true | Normal -> false)
