type t =
  { prefix : string;
    mutable count : int
  }

let create ~prefix = { prefix; count = 0 }

let next t =
  let name = Ir.Name.of_string (Format.sprintf "%s_%d" t.prefix t.count) in
  t.count <- t.count + 1;
  name

let count t = t.count
