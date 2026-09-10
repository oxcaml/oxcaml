type t =
  { value : int;
    flat : int
  }

let zero = { value = 0; flat = 0 }

let add { value; flat } { value = value'; flat = flat' } =
  { value = value + value'; flat = flat + flat' }

let rec count (el : _ Lambda.mixed_block_element) : t =
  match el with
  | Value _ -> { value = 8; flat = 0 }
  | Float_boxed _ | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 | Word
  | Untagged_immediate ->
    (* In a record, bits8/bits16/bits32/float32 aren't packed tightly *)
    { value = 0; flat = 8 }
  | Vec128 -> { value = 0; flat = 16 }
  | Vec256 -> { value = 0; flat = 32 }
  | Vec512 -> { value = 0; flat = 64 }
  | Mask -> { value = 0; flat = 8 }
  | Product layouts ->
    Array.fold_left (fun cts l -> add cts (count l)) zero layouts
  | Splice_variable _ ->
    Misc.fatal_error "Mixed_product_bytes_count: layout poly not supported"

let rec count_types_element (elt : Types.mixed_block_element) : t =
  match elt with
  | Scannable _ -> { value = 8; flat = 0 }
  | Float_boxed | Float64 | Float32 | Bits8 | Bits16 | Bits32 | Bits64 | Word
  | Untagged_immediate ->
    { value = 0; flat = 8 }
  | Vec128 -> { value = 0; flat = 16 }
  | Vec256 -> { value = 0; flat = 32 }
  | Vec512 -> { value = 0; flat = 64 }
  | Mask -> { value = 0; flat = 8 }
  | Product elts ->
    Array.fold_left (fun acc e -> add acc (count_types_element e)) zero elts
  | Void -> zero
  | Addressable elt ->
    (* CR box: This may have to be updated once addressability affects boxed
       representations *)
    count_types_element elt

let count_types_shape shape =
  Array.fold_left (fun acc elt -> add acc (count_types_element elt)) zero shape

let all_value { flat; _ } = Int.equal flat 0

let value_prefix_len { value; _ } = value / 8
