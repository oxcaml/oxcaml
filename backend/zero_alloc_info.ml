[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module String = Misc.Stdlib.String

module Encoded = struct
  module Component = struct
    type t = int

    let encode : unit Zero_alloc_utils.Component.t -> int = function
      | Bot -> 0
      | Safe -> 1
      | Top () -> 2

    let decode : int -> unit Zero_alloc_utils.Component.t = function
      | 0 -> Bot
      | 1 -> Safe
      | 2 -> Top ()
      | _ ->
        Misc.fatal_error
          "[Zero_alloc_info.Component_value.of_int]: value outside range 0-3"
  end

  module Components = struct
    type t = int

    let encode (components : Component.t Zero_alloc_utils.Components.t) =
      components.nor lor (components.exn lsl 2) lor (components.div lsl 4)

    let all_top =
      Zero_alloc_utils.Components.create (fun _ignore_tag ->
          Component.encode (Top ()))

    let all_top_encoding = encode all_top

    let encode_or_elide ~(encode_component : 'a -> Component.t)
        (components : 'a Zero_alloc_utils.Components.t) : t option =
      let encoded =
        encode
          (Zero_alloc_utils.Components.map components ~f:(fun _tag ->
               encode_component))
      in
      (* Artifact-size optimization: if this summary is all [Top] (the most
         common case), then leave it out, and let readers infer that a missing
         summary is all [Top]. *)
      if encoded = all_top_encoding then None else Some encoded

    let decode ~(decode_component : Component.t -> 'a) (i : t) :
        'a Zero_alloc_utils.Components.t =
      { nor = decode_component (i land 3);
        exn = decode_component ((i lsr 2) land 3);
        div = decode_component ((i lsr 4) land 3)
      }

    let decode_maybe_elided ~(decode_component : Component.t -> 'a) i :
        'a Zero_alloc_utils.Components.t =
      if i = all_top_encoding
      then Misc.fatal_error "Zero_alloc_checker unexpected all-Top encoding"
      else decode ~decode_component i
  end
end

type t = { mutable zero_alloc : Encoded.Components.t String.Map.t }

let create () = { zero_alloc = String.Map.empty }

let reset t = t.zero_alloc <- String.Map.empty

let merge src ~into:dst =
  let join key b1 b2 =
    Misc.fatal_errorf "Unexpected merge %s %d %d" key b1 b2
  in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc

let get (t : t) s = String.Map.find_opt s t.zero_alloc

let set (t : t) s (v : Encoded.Components.t) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    Some new_
  in
  t.zero_alloc <- String.Map.update s (f v) t.zero_alloc

let iter ~f t = String.Map.iter f t.zero_alloc

module Raw = struct
  type entries = (string * int) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let print (name, v) = Printf.printf "\t\t%s = %#x\n" name v in
    (* CR gyorsh: move encode/decode here somehow for noalloc *)
    Printf.printf "Function summaries for static checks:\n";
    List.iter print t

  let print = function None -> () | Some t -> print t
end

let to_raw (t : t) : Raw.t =
  if String.Map.is_empty t.zero_alloc
  then None
  else Some (String.Map.bindings t.zero_alloc)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t -> { zero_alloc = Raw.entries_to_map t }
