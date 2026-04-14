type t = None | Some of int

let f x = match match x with None -> Some 2 | Some _ -> None with None -> 3 | Some x -> x