let print s =
  let rec to_string : Ppxlib.Longident.t -> string = function
    | Lident s -> Printf.sprintf "Lident %S" s
    | Ldot (l, s) -> Printf.sprintf "Ldot (%s, %S)" (to_string l) s
    | Lapply (l, r) -> Printf.sprintf "Lapply (%s, %s)" (to_string l) (to_string r)
  in
  Printf.printf "%s: %s\n" s (to_string (Ppxlib.Longident.parse s))

let () = List.iter print [ "É.x"; "x.É"; "M.É"; "+."; "M.(+.)" ]
