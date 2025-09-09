let tbl : (string, int) Hashtbl.t = Hashtbl.create 64

let inc (k : string) : unit =
  let v = match Hashtbl.find_opt tbl k with Some n -> n | None -> 0 in
  Hashtbl.replace tbl k (v + 1)

let counters () : (string * int) list = Hashtbl.to_seq tbl |> List.of_seq

