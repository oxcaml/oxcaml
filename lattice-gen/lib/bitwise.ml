type var = string

type t =
  { node : node;
    mask : int
  }

and node =
  | Var of var
  | Const of int
  | Xor of t * int
  | And of t list
  | Or of t list
  | ShiftL of t * int
  | ShiftR of t * int
  | Masked of t * int

type binding =
  { name : string;
    expr : t
  }

let mask_of (t : t) = t.mask

let const n =
  let n = max 0 n in
  { node = Const n; mask = n }

let is_const_zero t =
  match t.node with
  | Const 0 -> true
  | _ -> false

let var ~mask name =
  let mask = max 0 mask in
  if mask = 0 then const 0 else { node = Var name; mask }

let rec key (t : t) =
  match t.node with
  | Var name -> Printf.sprintf "V(%s):%d" name t.mask
  | Const n -> Printf.sprintf "C(%d):%d" n t.mask
  | Xor (child, m) -> Printf.sprintf "X(%s,%d):%d" (key child) m t.mask
  | And children ->
    Printf.sprintf
      "A(%s):%d"
      (String.concat "," (List.map key children))
      t.mask
  | Or children ->
    Printf.sprintf
      "O(%s):%d"
      (String.concat "," (List.map key children))
      t.mask
  | ShiftL (child, amount) ->
    Printf.sprintf "SL(%s,%d):%d" (key child) amount t.mask
  | ShiftR (child, amount) ->
    Printf.sprintf "SR(%s,%d):%d" (key child) amount t.mask
  | Masked (child, mask) ->
    Printf.sprintf "M(%s,%d):%d" (key child) mask t.mask

let compare_expr left right = String.compare (key left) (key right)

let stable_uniq exprs =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun expr ->
      let k = key expr in
      if Hashtbl.mem seen k
      then false
      else (
        Hashtbl.add seen k ();
        true))
    exprs

let rec size (t : t) =
  1
  +
  match t.node with
  | Var _ | Const _ -> 0
  | Xor (child, _) | ShiftL (child, _) | ShiftR (child, _) | Masked (child, _) ->
    size child
  | And children | Or children ->
    List.fold_left (fun acc child -> acc + size child) 0 children

let rec mask t requested =
  let requested = max 0 requested in
  let requested = requested land t.mask in
  if requested = 0
  then const 0
  else if requested = t.mask
  then t
  else
    match t.node with
    | Const n -> const (n land requested)
    | Masked (child, child_mask) -> mask child (requested land child_mask)
    | _ -> { node = Masked (t, requested); mask = requested }

and xor_mask t bits =
  let bits = max 0 bits in
  if bits = 0
  then t
  else
    match t.node with
    | Const n -> const (n lxor bits)
    | Xor (child, bits') -> xor_mask child (bits lxor bits')
    | And children
      when List.for_all (fun child -> child.mask = bits) children ->
      let children = List.map (fun child -> xor_mask child bits) children in
      let const_bits, others =
        List.fold_left
          (fun (const_bits, others) child ->
            match child.node with
            | Const n -> (const_bits lor n, others)
            | _ -> (const_bits, child :: others))
          (0, [])
          children
      in
      let others = List.rev others in
      let others =
        others
        |> List.filter (fun child -> child.mask <> 0)
        |> List.sort_uniq compare_expr
      in
      let combined_mask =
        List.fold_left (fun acc child -> acc lor child.mask) const_bits others
      in
      (match others, const_bits with
       | [], _ -> const const_bits
       | [ child ], 0 -> child
       | _ ->
         let children =
           if const_bits = 0 then others else const const_bits :: others
         in
         { node = Or children; mask = combined_mask })
    | _ -> { node = Xor (t, bits); mask = t.mask lor bits }

let rec flatten_and acc = function
  | [] -> acc
  | child :: rest -> (
    match child.node with
    | And nested -> flatten_and acc (nested @ rest)
    | _ -> flatten_and (child :: acc) rest)

let and_ children =
  let children = flatten_and [] children |> List.rev in
  let constants, others =
    List.partition
      (fun child ->
        match child.node with
        | Const _ -> true
        | _ -> false)
      children
  in
  let const_mask =
    match constants with
    | [] -> None
    | _ ->
      Some
        (List.fold_left
           (fun acc child ->
             match child.node with
             | Const n -> acc land n
             | _ -> acc)
           (-1)
           constants)
  in
  let others = List.filter (fun child -> not (is_const_zero child)) others in
  let base_mask =
    match others with
    | [] -> -1
    | child :: rest ->
      List.fold_left (fun acc elt -> acc land elt.mask) child.mask rest
  in
  let final_mask =
    match const_mask with
    | None -> base_mask
    | Some const_mask -> base_mask land const_mask
  in
  if final_mask = 0
  then const 0
  else
    match others, const_mask with
    | [], Some const_mask -> const const_mask
    | [ child ], None -> child
    | [ child ], Some const_mask -> mask child const_mask
    | _ ->
      let children =
        match const_mask with
        | None -> others
        | Some const_mask ->
          let masked = const const_mask in
          if masked.mask = final_mask then others else masked :: others
      in
      let children = stable_uniq children in
      if List.length children = 1
      then mask (List.hd children) final_mask
      else { node = And children; mask = final_mask }

let rec flatten_or acc = function
  | [] -> acc
  | child :: rest -> (
    match child.node with
    | Or nested -> flatten_or acc (nested @ rest)
    | _ -> flatten_or (child :: acc) rest)

let or_ children =
  let children = flatten_or [] children |> List.rev in
  let const_bits, others =
    List.fold_left
      (fun (const_bits, others) child ->
        match child.node with
        | Const n -> (const_bits lor n, others)
        | _ -> (const_bits, child :: others))
      (0, [])
      children
  in
  let others = List.rev others in
  let others =
    others
    |> List.filter (fun child -> child.mask <> 0)
    |> stable_uniq
  in
  let combined_mask =
    List.fold_left (fun acc child -> acc lor child.mask) const_bits others
  in
  match others, const_bits with
  | [], _ -> const const_bits
  | [ child ], 0 -> child
  | _ ->
    let children =
      if const_bits = 0 then others else const const_bits :: others
    in
    { node = Or children; mask = combined_mask }

let shift_l t amount =
  if amount = 0
  then t
  else if t.mask = 0
  then const 0
  else
    match t.node with
    | Const n -> const (n lsl amount)
    | _ -> { node = ShiftL (t, amount); mask = t.mask lsl amount }

let dropped_low_bits amount =
  if amount <= 0 then 0 else (1 lsl amount) - 1

let shift_r t amount =
  if amount = 0
  then t
  else if t.mask = 0
  then const 0
  else
    match t.node with
    | Const n -> const (n lsr amount)
    | _ ->
      { node = ShiftR (t, amount);
        mask = (t.mask land lnot (dropped_low_bits amount)) lsr amount
      }

let is_atomic (t : t) =
  match t.node with
  | Var _ | Const _ -> true
  | _ -> false

let rec render (t : t) =
  let render_operand child =
    let rendered = render child in
    if is_atomic child then rendered else "(" ^ rendered ^ ")"
  in
  match t.node with
  | Var name -> name
  | Const n -> string_of_int n
  | Xor (child, bits) ->
    Printf.sprintf "%s lxor %d" (render_operand child) bits
  | ShiftL (child, amount) ->
    Printf.sprintf "%s lsl %d" (render_operand child) amount
  | ShiftR (child, amount) ->
    Printf.sprintf "%s lsr %d" (render_operand child) amount
  | Masked (child, requested_mask) ->
    Printf.sprintf "%s land %d" (render_operand child) requested_mask
  | And children ->
    String.concat " land " (List.map render_operand children)
  | Or children ->
    String.concat " lor " (List.map render_operand children)

module Pretty = struct
  type doc =
    | Nil
    | Text of string
    | Line
    | Cat of doc * doc
    | Nest of int * doc
    | Union of doc * doc

  let nil = Nil

  let text s = if s = "" then Nil else Text s

  let line = Line

  let ( ^^ ) left right =
    match left, right with
    | Nil, doc | doc, Nil -> doc
    | _ -> Cat (left, right)

  let nest indent doc =
    if indent = 0 then doc else Nest (indent, doc)

  let rec flatten = function
    | Nil -> Nil
    | Text _ as doc -> doc
    | Line -> Text " "
    | Cat (left, right) -> Cat (flatten left, flatten right)
    | Nest (_, doc) -> flatten doc
    | Union (left, _) -> flatten left

  let group doc = Union (flatten doc, doc)

  let rec fits remaining docs =
    if remaining < 0
    then false
    else
      match docs with
      | [] -> true
      | (_, Nil) :: rest -> fits remaining rest
      | (_, Text s) :: rest -> fits (remaining - String.length s) rest
      | (_, Line) :: _ -> true
      | (indent, Cat (left, right)) :: rest ->
        fits remaining ((indent, left) :: (indent, right) :: rest)
      | (_, Nest (indent, doc)) :: rest -> fits remaining ((indent, doc) :: rest)
      | (indent, Union (left, right)) :: rest ->
        fits remaining ((indent, left) :: rest)
        ||
        fits remaining ((indent, right) :: rest)

  let render ?(width = 88) doc =
    let buffer = Buffer.create 256 in
    let rec best col docs =
      match docs with
      | [] -> ()
      | (_, Nil) :: rest -> best col rest
      | (_, Text s) :: rest ->
        Buffer.add_string buffer s;
        best (col + String.length s) rest
      | (indent, Line) :: rest ->
        Buffer.add_char buffer '\n';
        Buffer.add_string buffer (String.make indent ' ');
        best indent rest
      | (indent, Cat (left, right)) :: rest ->
        best col ((indent, left) :: (indent, right) :: rest)
      | (indent, Nest (indent', doc)) :: rest ->
        best col ((indent' + indent, doc) :: rest)
      | (indent, Union (left, right)) :: rest ->
        if fits (width - col) [ (indent, left) ]
        then best col ((indent, left) :: rest)
        else best col ((indent, right) :: rest)
    in
    best 0 [ (0, doc) ];
    Buffer.contents buffer
end

let parens doc = Pretty.(text "(" ^^ doc ^^ text ")")

let infix_chain_doc op docs =
  match docs with
  | [] -> Pretty.text "0"
  | first :: rest ->
    Pretty.group
      Pretty.(first ^^ nest 2
        (List.fold_left
           (fun acc doc -> acc ^^ line ^^ text op ^^ text " " ^^ doc)
           nil
           rest))

let rec doc_of_expr (t : t) =
  let operand_doc child =
    if is_atomic child then Pretty.text (render child) else parens (doc_of_expr child)
  in
  match t.node with
  | Var name -> Pretty.text name
  | Const n -> Pretty.text (string_of_int n)
  | Xor (child, bits) ->
    Pretty.(operand_doc child ^^ text " lxor " ^^ text (string_of_int bits))
  | ShiftL (child, amount) ->
    Pretty.(operand_doc child ^^ text " lsl " ^^ text (string_of_int amount))
  | ShiftR (child, amount) ->
    Pretty.(operand_doc child ^^ text " lsr " ^^ text (string_of_int amount))
  | Masked (child, requested_mask) ->
    Pretty.(operand_doc child ^^ text " land " ^^ text (string_of_int requested_mask))
  | And children -> infix_chain_doc "land" (List.map operand_doc children)
  | Or children -> infix_chain_doc "lor" (List.map operand_doc children)

let render_pretty ?width t = Pretty.render ?width (doc_of_expr t)

let cse ?(avoid = []) root =
  let counts = Hashtbl.create 64 in
  let rec count (t : t) =
    let k = key t in
    Hashtbl.replace counts k (Option.value (Hashtbl.find_opt counts k) ~default:0 + 1);
    match t.node with
    | Var _ | Const _ -> ()
    | Xor (child, _) | ShiftL (child, _) | ShiftR (child, _) | Masked (child, _) ->
      count child
    | And children | Or children -> List.iter count children
  in
  count root;
  let avoid = Hashtbl.of_seq (List.to_seq (List.map (fun name -> (name, ())) avoid)) in
  let extracted = Hashtbl.create 32 in
  let bindings = ref [] in
  let next_id = ref 1 in
  let fresh_name () =
    let rec loop () =
      let name = Printf.sprintf "tmp%d" !next_id in
      incr next_id;
      if Hashtbl.mem avoid name then loop () else name
    in
    loop ()
  in
  let rec rewrite ~is_root (t : t) =
    let t =
      match t.node with
      | Var _ | Const _ -> t
      | Xor (child, bits) -> xor_mask (rewrite ~is_root:false child) bits
      | ShiftL (child, amount) -> shift_l (rewrite ~is_root:false child) amount
      | ShiftR (child, amount) -> shift_r (rewrite ~is_root:false child) amount
      | Masked (child, requested_mask) -> mask (rewrite ~is_root:false child) requested_mask
      | And children -> and_ (List.map (rewrite ~is_root:false) children)
      | Or children -> or_ (List.map (rewrite ~is_root:false) children)
    in
    let k = key t in
    let occurrences = Option.value (Hashtbl.find_opt counts k) ~default:0 in
    let should_extract =
      not is_root
      &&
      occurrences > 1
      &&
      size t > 2
      &&
      match t.node with
      | Var _ | Const _ -> false
      | _ -> true
    in
    if not should_extract
    then t
    else
      match Hashtbl.find_opt extracted k with
      | Some name -> var ~mask:t.mask name
      | None ->
        let name = fresh_name () in
        Hashtbl.add avoid name ();
        Hashtbl.add extracted k name;
        bindings := !bindings @ [ { name; expr = t } ];
        var ~mask:t.mask name
  in
  let root = rewrite ~is_root:true root in
  (!bindings, root)
