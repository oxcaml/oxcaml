open Exn_part1

exception Exn3

let[@inline never] [@local never] catch_exn1_from_llvm () =
  let r = ref1 in
  try
    r := 2;
    raise_exn1_catch_exn2_from_ocaml ()
  with Exn1 n ->
    my_print_int !r;
    my_print_int n [@nontail]

let[@inline never] [@local never] raise_exn1_catch_exn2_from_llvm () =
  let r = ref1 in
  r := 3;
  try raise_exn1_from_ocaml 30 with Exn2 -> ()

let[@inline never] [@local never] catch_exn1_nested_from_llvm () =
  let r = ref1 in
  try
    r := 4;
    raise_exn1_catch_exn2_from_llvm ()
  with Exn1 n ->
    my_print_int !r;
    my_print_int n [@nontail]

let[@inline never] [@local never] raise_1 n () : unit =
  (raise_exn1_from_ocaml n [@nontail])

let[@inline never] [@local never] raise_2 () : unit = raise Exn2

let[@inline never] [@local never] raise_3 () : unit = raise Exn3

let[@inline never] [@local never] complicated do_raise =
  let outer = secret 20 in
  try
    let x = secret 30 in
    try
      let y = secret 40 in
      try
        do_raise ();
        123123
      with Exn2 -> x + y
    with Exn1 n -> n + x
  with Exn3 -> outer

let[@inline never] [@local never] raise_in_loop () =
  for i = 0 to 1000 do
    try raise_exn1_from_ocaml i with Exn1 n -> assert (n = i)
  done
