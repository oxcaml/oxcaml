type t = string

let of_ident = Ident.unique_name

let of_string = Fun.id

let equal = String.equal

let hash = Hashtbl.hash

let compare = String.compare

let output = output_string

let print = Format.pp_print_string

include Identifiable.Make (struct
  type nonrec t = t

  let equal = equal

  let hash = hash

  let compare = compare

  let output = output

  let print = print
end)
