(* Addressability does not affect DWARF layouts, as it does not affect boxed
   representations. *)

module Int8_u = Stdlib_stable.Int8_u

type t8 : bits8 addressable

type tp : (bits64 & bits8) addressable

(* Kinds like [bits8 addressable] aren't yet inhabited, so we use magic. *)
external magic_make_addressable : ('a : any) ('b : any addressable). 'a -> 'b
  = "%identity"
  [@@layout_poly]

let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

let[@inline never] [@local never] f_addressable_bits8 (x : t8) = x

let _ = f_addressable_bits8 (magic_make_addressable (Int8_u.of_int 42))

let[@inline never] [@local never] f_addressable_product (x : tp) = x

let _ = f_addressable_product (magic_make_addressable #(#7L, Int8_u.of_int 8))

type r_unboxed = { x : t8 } [@@unboxed]

let[@inline never] [@local never] f_unboxed_record (r : r_unboxed) = r.x

let _ = f_unboxed_record { x = magic_make_addressable (Int8_u.of_int 3) }

type rec_record =
  { hd : t8;
    tl : rec_record option
  }

let[@inline never] [@local never] f_rec_record (r : rec_record) =
  match r.tl with None -> r.hd | Some r' -> r'.hd

let _ =
  f_rec_record
    { hd = magic_make_addressable (Int8_u.of_int 1);
      tl = Some { hd = magic_make_addressable (Int8_u.of_int 2); tl = None }
    }
