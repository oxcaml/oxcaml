let () =
  Array_rev.rev ();
  Array.iter
    (fun el ->
      print_int el;
      print_newline ())
    Array_rev_data.arr
