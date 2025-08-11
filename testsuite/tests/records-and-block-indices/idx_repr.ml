(* See [jane/doc/extensions/_03-unboxed-types/03-block-indices.md] *)
type t =
  | Bytecode of { path : int list }
  | Native of { offset : int; gap : int }

external magic_box_bits64 : ('a : bits64) 'b . 'a -> 'b =
  "%box_int64"

let of_idx idx =
  match Sys.backend_type with
  | Bytecode ->
    let r = Obj.repr (magic_box_bits64 idx) in
    let nth_idx n : int = Obj.magic (Obj.field r n) in
    let path = List.init (Obj.size r) nth_idx in
    Bytecode { path }
  | Native ->
    let i : int64 = magic_box_bits64 idx in
    let offset =
      Int64.(logand (sub (shift_left one 48) one)) i
      |> Int64.to_int
    in
    let gap =
      Int64.shift_right i 48
      |> Int64.to_int
    in
    Native { offset; gap }
  | Other _ ->
    failwith "unsupported backend"

let of_idx_imm = of_idx
let of_idx_mut = of_idx

let equal t1 t2 =
  match t1, t2 with
  | Bytecode { path = path1 }, Bytecode { path = path2 } ->
    List.equal Int.equal path1 path2
  | Native { gap = gap1; offset = offset1 },
    Native { gap = gap2; offset = offset2 } ->
    Int.equal gap1 gap2 && Int.equal offset1 offset2
  | Bytecode _, Native _ | Native _, Bytecode _ -> assert false

let debug_string = function
  | Bytecode { path } ->
    Printf.sprintf "{ %s }"
      (String.concat "; " (List.map Int.to_string path))
  | Native { offset; gap } ->
    Printf.sprintf "offset %d; gap %d" offset gap
