let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

let[@inline never] [@local never] test_anon_module () =
  let module _ = struct
    let _ = 42
  end in
  print_endline "anonymous module"

let _ = test_anon_module ()

let[@inline never] [@local never] test_anon_module_with_function () =
  let module _ = struct
    let[@inline never] [@local never] inner_func x = x + 1

    let _ = inner_func 5
  end in
  print_endline "anonymous module with function"

let _ = test_anon_module_with_function ()

let[@inline never] [@local never] test_named_vs_anon () =
  (* Named module for comparison *)
  let module Named = struct
    let[@inline never] [@local never] named_func x = x * 2
  end in
  (* Anonymous module with function *)
  let module _ = struct
    let[@inline never] [@local never] anon_func x = x + 1

    let _ = anon_func 10
  end in
  Named.named_func 5

let _ = test_named_vs_anon ()

let[@inline never] [@local never] test_nested_anon_module () =
  let module _ = struct
    let[@inline never] [@local never] outer_func () =
      let module _ = struct
        let[@inline never] [@local never] inner_func y = y + 10

        let _ = inner_func 5
      end in
      42

    let () = ignore (outer_func ())
  end in
  print_endline "nested anonymous module"

let _ = test_nested_anon_module ()
