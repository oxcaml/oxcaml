external format32 : string -> float32 -> string = "caml_format_float32"

let pi : float = 3.141592653589793

let e : float32 = 2.7182818284s

let rec f y = if y <= 1 then 1 else g (y - 1) (y - 2) [@nontail]

and g i j = (f i + f j) [@nontail]

let () =
  print_endline "one of the strings\nof all time";
  Format.printf "%.20f %s\n" pi (format32 "%.10g" e);
  Format.printf "mutually_recursive: %d\n" (f 5)
