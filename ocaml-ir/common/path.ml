open Std
let split filename =
  ((Filename.dirname filename), (Filename.basename filename))
let parts filename =
  let rec loop acc filename =
    match split filename with
    | (("." as base), ".") -> base :: acc
    | (("/" as base), "/") -> base :: acc
    | (rest, dir) -> loop (dir :: acc) rest in
  loop [] filename
let of_parts =
  function
  | [] -> failwith "Filename.of_parts: empty parts list"
  | root::rest -> List.fold_left rest ~init:root ~f:Filename.concat
let (^/) = Filename.concat
let canonicalize path =
  let parts = parts path in
  let parts_canonical =
    (List.fold_left parts ~init:[]
       ~f:(fun acc part ->
             match acc with
             | _::parent::rest when
                 String.equal Filename.parent_dir_name part -> parent :: rest
             | _::_rest when String.equal Filename.current_dir_name part ->
                 acc
             | acc -> part :: acc))
      |> List.rev in
  of_parts parts_canonical
let chop_path_prefix ~prefix path =
  let rec chop prefix_ path_ =
    match (prefix_, path_) with
    | (x::prefix_, y::path_) when String.equal x y -> chop prefix_ path_
    | ([], path_) -> Some path_
    | (_, _) -> None in
  Option.Let_syntax.Let_syntax.map (chop (parts prefix) (parts path))
    ~f:(function | [] -> Filename.current_dir_name | parts -> of_parts parts)
let chop_path_prefix_exn ~prefix path =
  (chop_path_prefix ~prefix path) |>
    (Option.value_exn
       ~here:{
               Lexing.pos_fname = "lib/ocaml_ir/common/path.ml";
               pos_lnum = 52;
               pos_cnum = 1438;
               pos_bol = 1425
             }
       ~message:(Printf.sprintf "Path %s is not a prefix of %s" prefix path))
let change_relative_root ~current_root ~target_root path =
  let rec chop_common_root xs ys =
    match (xs, ys) with
    | (x::xs, y::ys) when String.equal x y -> chop_common_root xs ys
    | _ -> (xs, ys) in
  let (dirname, basename) =
    let absolute =
      canonicalize
        (if Filename.is_relative path then current_root ^/ path else path) in
    split absolute in
  let target_root = parts (canonicalize target_root) in
  let (from_common_root, rest) = chop_common_root (parts dirname) target_root in
  let to_common_root =
    match rest with
    | [] -> [Filename.current_dir_name]
    | l -> List.map l ~f:(fun _ -> Filename.parent_dir_name) in
  let target_relative = of_parts (to_common_root @ from_common_root) in
  target_relative ^/ basename
