(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Sizes are always represented as in [Code_size_v2] (estimates for both
   architectures plus the frame flag); the v1 model only fills in the same
   number for both architectures. Which model computes the sizes of terms is
   chosen by [-flambda2-code-size-model]. *)

type t = Code_size_v2.t

let zero = Code_size_v2.zero

let equal = Code_size_v2.equal

let ( + ) = Code_size_v2.( + )

(* Sizes from the v1 model never record allocations, so for them [seq] is the
   same as [( + )]. *)
let seq = Code_size_v2.seq

let with_out_of_line = Code_size_v2.with_out_of_line

let ( - ) = Code_size_v2.( - )

let ( <= ) = Code_size_v2.( <= )

let print = Code_size_v2.print

let of_int = Code_size_v2.of_int

let to_int = Code_size_v2.to_int

let create = Code_size_v2.create

let x86_64 = Code_size_v2.x86_64

let arm64 = Code_size_v2.arm64

let evaluate = Code_size_v2.evaluate

(* Sizes produced by the v1 model never request a frame, so this is a no-op for
   them. *)
let add_function_frame = Code_size_v2.add_function_frame

let select ~v1 ~v2 =
  match Flambda_features.code_size_model () with
  | V1 -> Code_size_v2.of_int (v1 ())
  | V2 -> v2 ()

let prim ~machine_width prim =
  select
    ~v1:(fun () -> Code_size_v1.prim ~machine_width prim)
    ~v2:(fun () -> Code_size_v2.prim ~machine_width prim)

let simple simple =
  select
    ~v1:(fun () -> Code_size_v1.simple simple)
    ~v2:(fun () -> Code_size_v2.simple simple)

let static_consts () =
  select
    ~v1:(fun () -> Code_size_v1.static_consts ())
    ~v2:(fun () -> Code_size_v2.static_consts ())

let apply ~is_tail apply =
  select
    ~v1:(fun () -> Code_size_v1.apply apply)
    ~v2:(fun () -> Code_size_v2.apply ~is_tail apply)

let apply_cont apply_cont =
  select
    ~v1:(fun () -> Code_size_v1.apply_cont apply_cont)
    ~v2:(fun () -> Code_size_v2.apply_cont apply_cont)

let switch switch =
  select
    ~v1:(fun () -> Code_size_v1.switch switch)
    ~v2:(fun () -> Code_size_v2.switch switch)

(* Zero in both models. *)
let invalid = Code_size_v2.invalid

let box_number ~machine_width kind =
  select
    ~v1:(fun () -> Code_size_v1.box_number ~machine_width kind)
    ~v2:(fun () -> Code_size_v2.box_number ~machine_width kind)

let block num_fields =
  select
    ~v1:(fun () -> Code_size_v1.block num_fields)
    ~v2:(fun () -> Code_size_v2.block num_fields)

let array num_fields =
  select
    ~v1:(fun () -> Code_size_v1.array num_fields)
    ~v2:(fun () -> Code_size_v2.array num_fields)

let set_of_closures_allocation ~num_stores =
  select
    ~v1:(fun () -> Int.sub (Int.add Code_size_v1.alloc_size num_stores) 1)
    ~v2:(fun () -> Code_size_v2.set_of_closures_allocation ~num_stores)
