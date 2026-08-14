(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int = Numbers.Int

module Id = struct
  include Int

  let flags_size_in_bits = 3

  let flags_shift = Sys.int_size - flags_size_in_bits

  let mask_selecting_bottom_bits = -1 lsr flags_size_in_bits

  let create t flags =
    if flags < 0 || flags >= 1 lsl (flags_size_in_bits + 1)
    then Misc.fatal_errorf "Flags value 0x%x out of range" flags;
    t land mask_selecting_bottom_bits lor (flags lsl flags_shift)

  let flags t = t lsr flags_shift

  let next t = t + 1
end

module Make (E : sig
  type t

  val flags : int

  val print : Format.formatter -> t -> unit

  val hash : t -> int

  val equal : t -> t -> bool
end) =
struct
  module HT = Hashtbl.Make (struct
    type t = int

    (* CR mshinwell: maybe this should be a proper hash function *)
    let hash (t : t) = Hashtbl.hash t

    let equal t1 t2 = t1 == t2
  end)

  let () = assert (E.flags lsr Id.flags_size_in_bits = 0)

  type t = E.t HT.t

  let create () = HT.create 20_000

  exception Can_add of int

  exception Already_added of int

  let add t elt =
    let id = Id.create (E.hash elt) E.flags in
    match HT.find t id with
    | exception Not_found ->
      HT.add t id elt;
      id
    | existing_elt -> (
      if E.equal elt existing_elt
      then id
      else
        try
          let starting_id = id in
          let id = ref (Id.next starting_id) in
          (* If there is a collision, we search for another slot, but take care
             not to alter the flags bits. *)
          while !id <> starting_id do
            assert (Id.flags !id = E.flags);
            match HT.find t !id with
            | exception Not_found -> raise (Can_add !id)
            | existing_elt ->
              if E.equal elt existing_elt
              then raise (Already_added !id)
              else id := Id.next !id
          done;
          Misc.fatal_errorf "No hash values left for@ %a" E.print elt
        with
        | Can_add id ->
          HT.add t id elt;
          assert (Id.flags id = E.flags);
          id
        | Already_added id ->
          assert (Id.flags id = E.flags);
          id)

  let find t id =
    assert (Id.flags id = E.flags);
    HT.find t id

  (* We serialize a table using the exact same format we use in memory, except
     that we only store the data for the exported elements.

     We also record the largest difference between a hash and its final
     identifier so that we do not loop forever when calling [import_backwards]
     on data that was not exported. *)
  type serializable =
    { exported : E.t HT.t;
      max_diff : int
    }

  let export t ~iter =
    let exported = HT.create 0 in
    let max_diff = ref 0 in
    iter (fun id ->
        let elt = find t id in
        let starting_id = Id.create (E.hash elt) E.flags in
        max_diff := max !max_diff (id - starting_id);
        HT.replace exported id elt);
    { exported; max_diff = !max_diff }

  exception Not_exported

  let import t id =
    assert (Id.flags id = E.flags);
    try HT.find t.exported id
    with Not_found ->
      Misc.fatal_error "Id was not exported from this compilation unit."

  exception Found_id

  let import_backwards_exn { exported; max_diff } elt =
    let id = Id.create (E.hash elt) E.flags in
    match HT.find exported id with
    | existing_elt when E.equal elt existing_elt -> id
    | _ | (exception Not_found) -> (
      (* Replicate the search for another id that was performed in the original
         map to compute the actual id for [elt], skipping over empty slots that
         have not been exported.

         We recorded the maximum difference between a [starting_id] and its
         actual id in the exported data, so we can stop looking after [max_diff]
         steps (which might well be immediately). *)
      let id = ref (Id.next id) in
      let stopping_id = !id + max_diff in
      try
        while !id <> stopping_id do
          match HT.find exported !id with
          | existing_elt when E.equal elt existing_elt -> raise_notrace Found_id
          | _ | (exception Not_found) -> id := Id.next !id
        done;
        raise Not_exported
      with Found_id -> !id)
end
