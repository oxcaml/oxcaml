(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a with_name =
  { value : 'a;
    name : string
  }

type 'a with_names =
  { values : 'a;
    names : string list
  }

include Heterogenous_list

module Or_null_sender = struct
  module T0 = struct
    type 'a t = 'a Channel.or_null_sender
  end

  include T0
  include Make (T0)

  let[@inline] send s v = Channel.send_or_null s (Or_null.this v)

  let rec send_hlist : type s. s hlist -> s Constant.hlist -> unit =
   fun refs values ->
    match refs, values with
    | [], [] -> ()
    | r :: rs, v :: vs ->
      send r v;
      send_hlist rs vs
end

module Or_null_receiver = struct
  module T0 = struct
    type 'a t = 'a Channel.or_null_receiver
  end

  include T0
  include Make (T0)

  let[@inline] recv r =
    match Channel.recv_or_null r with
    | Null -> Misc.fatal_error "Or_null_receiver: empty receiver"
    | This r -> r

  let rec recv_hlist : type s. s hlist -> s Constant.hlist = function
    | [] -> []
    | r :: rs -> recv r :: recv_hlist rs
end
