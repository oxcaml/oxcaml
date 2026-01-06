open Jkind_axis

let axis_equal (Axis.Pack a) (Axis.Pack b) =
  match Per_axis.eq_obj a b with
  | Some Refl -> true
  | None -> false

let axis_name (Axis.Pack axis) = Axis.name axis

let pp_axis_list axes =
  axes |> List.map axis_name |> String.concat ", "

let check_order () =
  let expected = Axis.all in
  let count = List.length expected in
  let actual =
    let rec loop i acc =
      if i = count
      then List.rev acc
      else loop (i + 1) (Axis_lattice.axis_number_to_axis_packed i :: acc)
    in
    loop 0 []
  in
  if not (List.for_all2 axis_equal expected actual)
  then
    failwith
      (Format.asprintf "Axis order mismatch: all=[%s] index=[%s]"
         (pp_axis_list expected) (pp_axis_list actual))

let () = check_order ()
