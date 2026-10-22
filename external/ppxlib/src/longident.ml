open! Import

module T = struct
  type t = longident = Lident of string | Ldot of t * string | Lapply of t * t

  let compare : t -> t -> int = Poly.compare

  let is_normal_ident_char = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false

  let is_normal_ident = function
    | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "or" -> false
    | string -> String.for_all string ~f:is_normal_ident_char

  let short_name string =
    if is_normal_ident string then string else "( " ^ string ^ " )"

  let rec name = function
    | Lident s -> short_name s
    | Ldot (a, b) -> name a ^ "." ^ short_name b
    | Lapply (a, b) -> Printf.sprintf "%s(%s)" (name a) (name b)

  let sexp_of_t t = Sexp.Atom (name t)
end

include T

let rec flat accu = function
  | Lident s -> s :: accu
  | Ldot (lid, s) -> flat (s :: accu) lid
  | Lapply (_, _) -> invalid_arg "Ppxlib.Longident.flatten"

let flatten_exn lid = flat [] lid

let last_exn = function
  | Lident s -> s
  | Ldot (_, s) -> s
  | Lapply (_, _) -> invalid_arg "Ppxlib.Longident.flatten"

let unflatten ~init l = List.fold_left l ~init ~f:(fun acc s -> Ldot (acc, s))

(* for cases without dotted operators (e.g. [parse "A.B.C"]) *)
let parse_simple s =
  match String.split_on_char s ~sep:'.' with
  | [] -> assert false
  | s :: l -> unflatten ~init:(Lident s) l

(* find the first matching pair of parentheses *)
let rec parentheses lpos opened pos len s =
  if pos >= len then if opened > 0 then Error () else Ok None
  else
    match s.[pos] with
    | '(' ->
        let lpos = if opened = 0 then pos else lpos in
        parentheses lpos (opened + 1) (pos + 1) len s
    | ')' ->
        let opened = opened - 1 in
        if opened = 0 then Ok (Some (lpos, pos))
        else if opened < 0 then Error ()
        else parentheses lpos opened (pos + 1) len s
    | _ -> parentheses lpos opened (pos + 1) len s

(* handle ["A.B.(+.+)"] or ["Vec.(.%.()<-)"] *)
let parse s =
  let invalid variant =
    invalid_arg (Printf.sprintf "Ppxlib.Longident.parse(%s): %S" variant s)
  in
  if String.length s < 1 then invalid "empty string";
  let par = parentheses (-1) 0 0 (String.length s) s in
  match (s.[0], par) with
  | ('A' .. 'Z' | 'a' .. 'z' | '_'), Ok None -> parse_simple s
  | _, Ok None -> Lident s (* This is a raw operator, no module path *)
  | _, Error _ -> invalid "unbalanced parenthesis"
  | _, Ok (Some (l, r)) -> (
      if Int.(r <> String.length s - 1) then
        invalid "right parenthesis misplaced";
      let group =
        let inside = String.trim (String.sub s ~pos:(l + 1) ~len:(r - l - 1)) in
        if String.(inside = "") then "()" else inside
      in
      if Int.(l = 0) then Lident group
      else if Char.(s.[l - 1] <> '.') then invalid "application in path"
      else
        let before = String.sub s ~pos:0 ~len:(l - 1) in
        match String.split_on_char before ~sep:'.' with
        | [] -> assert false
        | s :: l -> Ldot (unflatten ~init:(Lident s) l, group))

module Map = Map.Make (T)
module Set = Set.Make (T)
