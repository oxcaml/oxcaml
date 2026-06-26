type t =
  { value : int;
    flat : int
  }

let zero = { value = 0; flat = 0 }

let add { value; flat } { value = value'; flat = flat' } =
  { value = value + value'; flat = flat + flat' }

let rec count_types_element (elt : Types.mixed_block_element) : t =
  match elt with
  | Scannable _ -> { value = 8; flat = 0 }
  | Float_boxed | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 | Word
  | Untagged_immediate ->
    { value = 0; flat = 8 }
  | Vec128 -> { value = 0; flat = 16 }
  | Vec256 -> { value = 0; flat = 32 }
  | Vec512 -> { value = 0; flat = 64 }
  | Product elts ->
    Array.fold_left (fun acc elt -> add acc (count_types_element elt)) zero elts
  | Void -> zero

let count_types_shape shape =
  Array.fold_left (fun acc elt -> add acc (count_types_element elt)) zero shape

let all_value { flat; _ } = Int.equal flat 0

let value_prefix_len { value; _ } = value / 8
