open Std
let rec split_path path acc =
  match ((Filename.dirname path), (Filename.basename path)) with
  | (dir, _) when dir = path -> dir :: acc
  | (dir, base) -> split_path dir (base :: acc)
let canonicalize_filename ?cwd path =
  let parts =
    match split_path path [] with
    | dot::rest when dot = Filename.current_dir_name ->
        split_path (match cwd with | None -> Sys.getcwd () | Some c -> c)
          rest
    | parts -> parts in
  let goup path =
    function
    | dir when dir = Filename.parent_dir_name ->
        (match path with | _::t -> t | [] -> [])
    | dir when dir = Filename.current_dir_name -> path
    | dir -> dir :: path in
  let parts = List.rev (List.fold_left ~f:goup ~init:[] parts) in
  let filename_concats =
    function
    | [] -> ""
    | root::subs -> List.fold_left ~f:Filename.concat ~init:root subs in
  filename_concats parts
let rec expand_glob ~filter acc root =
  function
  | [] -> root :: acc
  | (Glob.Wildwild)::_tl ->
      let rec append acc root =
        let items = try Sys.readdir root with | Sys_error _ -> [||] in
        let process acc dir =
          let filename = Filename.concat root dir in
          if filter filename then append (filename :: acc) filename else acc in
        Array.fold_left process (root :: acc) items in
      append acc root
  | (Glob.Exact component)::tl ->
      let filename = Filename.concat root component in
      expand_glob ~filter acc filename tl
  | pattern::tl ->
      let items = try Sys.readdir root with | Sys_error _ -> [||] in
      let process acc dir =
        if Glob.match_pattern pattern dir
        then
          let root' = Filename.concat root dir in
          (if filter root' then expand_glob ~filter acc root' tl else acc)
        else acc in
      Array.fold_left process acc items
let expand_glob ?(filter= fun _ -> true) path acc =
  match split_path path [] with
  | [] -> acc
  | root::subs ->
      let patterns = List.map ~f:Glob.compile_pattern subs in
      expand_glob ~filter acc root patterns
let expand_directory alt s =
  if ((String.length s) > 0) && ((s.[0]) = '+')
  then Filename.concat alt (String.sub s ~pos:1 ~len:((String.length s) - 1))
  else s
let rev_split_string cond s =
  let rec split1 res i =
    if i >= (String.length s)
    then res
    else if cond (s.[i]) then split1 res (i + 1) else split2 res i (i + 1)
  and split2 res i j =
    if j >= (String.length s)
    then (String.sub s ~pos:i ~len:(j - i)) :: res
    else
      if cond (s.[j])
      then split1 ((String.sub s ~pos:i ~len:(j - i)) :: res) (j + 1)
      else split2 res i (j + 1) in
  split1 [] 0
let rev_split_words s =
  let helper = function | ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
  rev_split_string helper s
