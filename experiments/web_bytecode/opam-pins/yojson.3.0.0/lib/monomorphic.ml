let rec pp fmt =
  function
  | `Null -> Format.pp_print_string fmt "`Null"
  | `Bool x ->
    Format.fprintf fmt "`Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
#ifdef INT
  | `Int x ->
    Format.fprintf fmt "`Int (@[<hov>";
    Format.fprintf fmt "%d" x;
    Format.fprintf fmt "@])"
#endif
#ifdef INTLIT
  | `Intlit x ->
    Format.fprintf fmt "`Intlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
#endif
#ifdef FLOAT
  | `Float x ->
    Format.fprintf fmt "`Float (@[<hov>";
    Format.fprintf fmt "%F" x;
    Format.fprintf fmt "@])"
#endif
#ifdef FLOATLIT
  | `Floatlit x ->
    Format.fprintf fmt "`Floatlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
#endif
#ifdef STRING
  | `String x ->
    Format.fprintf fmt "`String (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
#endif
#ifdef STRINGLIT
  | `Stringlit x ->
    Format.fprintf fmt "`Stringlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
#endif
  | `Assoc xs ->
    Format.fprintf fmt "`Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | `List xs ->
    Format.fprintf fmt "`List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"

let show x =
  Format.asprintf "%a" pp x

let rec equal a b =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
#ifdef INT
  | `Int a, `Int b -> a = b
#endif
#ifdef INTLIT
    | `Intlit a, `Intlit b -> a = b
#endif
#ifdef FLOAT
    | `Float a, `Float b -> a = b
#endif
#ifdef FLOATLIT
    | `Floatlit a, `Floatlit b -> a = b
#endif
#ifdef STRING
    | `String a, `String b -> a = b
#endif
#ifdef STRINGLIT
    | `Stringlit a, `Stringlit b -> a = b
#endif
    | `Assoc xs, `Assoc ys ->
      let compare_keys = fun (key, _) (key', _) -> String.compare key key' in
      let xs = List.stable_sort compare_keys xs in
      let ys = List.stable_sort compare_keys ys in
      (match List.for_all2 (fun (key, value) (key', value') ->
        match key = key' with
        | false -> false
        | true -> equal value value') xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    | `List xs, `List ys ->
      (match List.for_all2 equal xs ys with
      | result -> result
      | exception Invalid_argument _ ->
        (* the lists were of different lengths, thus unequal *)
        false)
    | _ -> false
