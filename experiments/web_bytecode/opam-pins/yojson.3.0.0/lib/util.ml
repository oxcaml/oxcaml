exception Type_error of string * t

let typeof = function
  | `Assoc _ -> "object"
  | `Bool _ -> "bool"
  | `Float _ -> "float"
#ifdef INT
  | `Int _ -> "int"
#endif
  | `List _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
  | `Intlit _ -> "intlit"
  | `Floatlit _ -> "floatlit"
#ifdef STRINGLIT
  | `Stringlit _ -> "stringlit"
#endif

let typerr msg js = raise (Type_error (msg ^ typeof js, js))

exception Undefined of string * t

let assoc name obj = try List.assoc name obj with Not_found -> `Null

let member name = function
  | `Assoc obj -> assoc name obj
  | js -> typerr ("Can't get member '" ^ name ^ "' of non-object type ") js

let rec path l obj =
  match l with
  | [] -> Some obj
  | key :: l -> (
      match obj with
      | `Assoc assoc -> (
          match List.assoc key assoc with
          | obj -> path l obj
          | exception Not_found -> None)
      | _ -> None)

let index i = function
  | `List l as js ->
      let len = List.length l in
      let wrapped_index = if i < 0 then len + i else i in
      if wrapped_index < 0 || wrapped_index >= len then
        raise (Undefined ("Index " ^ string_of_int i ^ " out of bounds", js))
      else List.nth l wrapped_index
  | js ->
      typerr ("Can't get index " ^ string_of_int i ^ " of non-array type ") js

let map f = function
  | `List l -> `List (List.map f l)
  | js -> typerr "Can't map function over non-array type " js

let to_assoc = function
  | `Assoc obj -> obj
  | js -> typerr "Expected object, got " js

let to_option f = function `Null -> None | x -> Some (f x)
let to_bool = function `Bool b -> b | js -> typerr "Expected bool, got " js

let to_bool_option = function
  | `Bool b -> Some b
  | `Null -> None
  | js -> typerr "Expected bool or null, got " js

let to_number = function
#ifdef INT
  | `Int i -> float i
#endif
#ifdef FLOAT
  | `Float f -> f
#endif
  | js -> typerr "Expected number, got " js

let to_number_option = function
#ifdef INT
  | `Int i -> Some (float i)
#endif
#ifdef FLOAT
  | `Float f -> Some f
#endif
  | `Null -> None
  | js -> typerr "Expected number or null, got " js

let to_float = function
#ifdef FLOAT
  | `Float f -> f
#endif
  | js -> typerr "Expected float, got " js

let to_float_option = function
#ifdef FLOAT
  | `Float f -> Some f
#endif
  | `Null -> None
  | js -> typerr "Expected float or null, got " js

let to_int = function
#ifdef INT
  | `Int i -> i
#endif
  | js -> typerr "Expected int, got " js

let to_int_option = function
#ifdef INT
  | `Int i -> Some i
#endif
  | `Null -> None
  | js -> typerr "Expected int or null, got " js

let to_list = function `List l -> l | js -> typerr "Expected array, got " js

let to_string = function
#ifdef STRING
  | `String s -> s
#endif
  | js -> typerr "Expected string, got " js

let to_string_option = function
#ifdef STRING
  | `String s -> Some s
#endif
  | `Null -> None
  | js -> typerr "Expected string or null, got " js

let convert_each f = function
  | `List l -> List.map f l
  | js -> typerr "Can't convert each element of non-array type " js

let rec rev_filter_map f acc l =
  match l with
  | [] -> acc
  | x :: tl -> (
      match f x with
      | None -> rev_filter_map f acc tl
      | Some y -> rev_filter_map f (y :: acc) tl)

let filter_map f l = List.rev (rev_filter_map f [] l)

let rec rev_flatten acc l =
  match l with
  | [] -> acc
  | x :: tl -> (
      match x with
      | `List l2 -> rev_flatten (List.rev_append l2 acc) tl
      | _ -> rev_flatten acc tl)

let flatten l = List.rev (rev_flatten [] l)

let filter_index i l =
  filter_map
    (function
      | `List l -> ( try Some (List.nth l i) with _ -> None) | _ -> None)
    l

let filter_list l = filter_map (function `List l -> Some l | _ -> None) l

let filter_member k l =
  filter_map
    (function
      | `Assoc l -> ( try Some (List.assoc k l) with _ -> None) | _ -> None)
    l

let filter_assoc l = filter_map (function `Assoc l -> Some l | _ -> None) l
let filter_bool l = filter_map (function `Bool x -> Some x | _ -> None) l
let filter_int l =
  filter_map (
      function
        #ifdef INT
      |  `Int x -> Some x
                     #endif
      | _ -> None
    ) l

let filter_float l =
  filter_map (
    function
#ifdef FLOAT
      `Float x -> Some x
#endif
      | _ -> None
  ) l

let filter_number l =
  filter_map (
    function
#ifdef INT
        `Int x -> Some (float x)
#endif
#ifdef FLOAT
      | `Float x -> Some x
#endif
      | _ -> None
  ) l

let filter_string l =
  filter_map (
    function
#ifdef STRING
        `String x -> Some x
#endif
      | _ -> None
  ) l

let keys o =
  to_assoc o |> List.map (fun (key, _) -> key)

let values o =
  to_assoc o |> List.map (fun (_, value) -> value)

let combine (first : t) (second : t) =
  match (first, second) with
  | `Assoc a, `Assoc b -> (`Assoc (a @ b) : t)
  | a, b -> raise (Invalid_argument "Expected two objects, check inputs")
