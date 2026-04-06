type var = string

type t =
  { id : int;
    node : node;
    mask : int
  }

and node =
  | Var of var
  | Const of int
  | Xor2 of t * t
  | Xor of t * int
  | And of t list
  | Or of t list
  | ShiftL of t * int
  | ShiftR of t * int

type binding =
  { name : string;
    expr : t
  }

type known_bits =
  { known0 : int;
    known1 : int
  }

let next_id = ref 1

let make node mask =
  let id = !next_id in
  incr next_id;
  { id; node; mask }

let known_bits_cache : (int, known_bits) Hashtbl.t = Hashtbl.create 256
let project_cache : ((int * int), t) Hashtbl.t = Hashtbl.create 512
let simplify_cache : ((int * int), t) Hashtbl.t = Hashtbl.create 512

let mask_of (t : t) = t.mask

let all_bits = -1

let lowmask bits =
  if bits <= 0 then 0 else (1 lsl bits) - 1

let bits_literal n =
  let n = max 0 n in
  if n = 0
  then "0"
  else
    let rec loop acc n =
      if n = 0
      then acc
      else loop ((if n land 1 = 0 then "0" else "1") :: acc) (n lsr 1)
    in
    "0b" ^ String.concat "" (loop [] n)

let const n =
  let n = max 0 n in
  make (Const n) n

let is_const_zero t =
  match t.node with
  | Const 0 -> true
  | _ -> false

let var ~mask name =
  let mask = max 0 mask in
  if mask = 0 then const 0 else make (Var name) mask

let rec key (t : t) =
  match t.node with
  | Var name -> Printf.sprintf "V(%s):%d" name t.mask
  | Const n -> Printf.sprintf "C(%d):%d" n t.mask
  | Xor2 (left, right) ->
    Printf.sprintf "XX(%s,%s):%d" (key left) (key right) t.mask
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
  | Xor (child, _) | ShiftL (child, _) | ShiftR (child, _) -> size child
  | Xor2 (left, right) -> size left + size right
  | And children | Or children ->
    List.fold_left (fun acc child -> acc + size child) 0 children

let rec flatten_and acc = function
  | [] -> acc
  | child :: rest -> (
    match child.node with
    | And nested -> flatten_and acc (nested @ rest)
    | _ -> flatten_and (child :: acc) rest)

let rec flatten_or acc = function
  | [] -> acc
  | child :: rest -> (
    match child.node with
    | Or nested -> flatten_or acc (nested @ rest)
    | _ -> flatten_or (child :: acc) rest)

let rec normalize_xor child bits =
  let bits = max 0 bits in
  if bits = 0
  then child
  else
    match child.node with
    | Const n -> const (n lxor bits)
    | Xor (grandchild, bits') -> normalize_xor grandchild (bits lxor bits')
    | And children ->
      let constants, others =
        List.partition
          (fun child ->
            match child.node with
            | Const _ -> true
            | _ -> false)
          children
      in
      let const_mask =
        List.fold_left
          (fun acc child ->
            match child.node with
            | Const n -> acc land n
            | _ -> acc)
          all_bits
          constants
      in
      if constants <> [] && bits land const_mask = bits
      then (
        let base =
          match others with
          | [] -> const const_mask
          | [ child ] -> child
          | _ -> normalize_and others
        in
        normalize_and [ normalize_xor base bits; const const_mask ])
      else if List.for_all (fun child -> child.mask land bits = child.mask) children
      then
        let children = List.map (fun child -> normalize_xor child bits) children in
        normalize_or children
      else make (Xor (child, bits)) (child.mask lor bits)
    | _ -> make (Xor (child, bits)) (child.mask lor bits)

and normalize_xor2 left right =
  if left.mask = 0
  then right
  else if right.mask = 0
  then left
  else if key left = key right
  then const 0
  else
    match left.node, right.node with
    | Const a, Const b -> const (a lxor b)
    | Const bits, _ -> normalize_xor right bits
    | _, Const bits -> normalize_xor left bits
    | _ -> make (Xor2 (left, right)) (left.mask lor right.mask)

and normalize_and children =
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
    List.fold_left
      (fun acc child ->
        match child.node with
        | Const n -> acc land n
        | _ -> acc)
      all_bits
      constants
  in
  let others = List.filter (fun child -> not (is_const_zero child)) others in
  let others = stable_uniq others in
  let final_mask =
    List.fold_left (fun acc child -> acc land child.mask) const_mask others
  in
  if final_mask = 0
  then const 0
  else
    let children =
      if const_mask = all_bits
      then others
      else others @ [ const const_mask ]
    in
    match children with
    | [] -> const const_mask
    | [ child ] ->
      if child.mask = final_mask
      then child
      else make (And [ child; const final_mask ]) final_mask
    | _ -> make (And children) final_mask

and projection_of expr =
  match expr.node with
  | And children ->
    let constants, others =
      List.partition
        (fun child ->
          match child.node with
          | Const _ -> true
          | _ -> false)
        children
    in
    if constants = []
    then None
    else
      let projected =
        List.fold_left
          (fun acc child ->
            match child.node with
            | Const n -> acc land n
            | _ -> acc)
          all_bits
          constants
      in
      let base =
        match others with
        | [] -> const all_bits
        | [ child ] -> child
        | _ ->
          let mask =
            List.fold_left (fun acc child -> acc land child.mask) all_bits others
          in
          make (And others) mask
      in
      Some (base, projected)
  | _ -> None

and project_expr expr projected =
  if projected = all_bits
  then expr
  else mask expr projected

and normalize_or children =
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
  let grouped = Hashtbl.create 8 in
  let passthrough = ref [] in
  List.iter
    (fun child ->
      match projection_of child with
      | Some (base, projected) when projected <> all_bits ->
        let prev = Option.value (Hashtbl.find_opt grouped projected) ~default:[] in
        Hashtbl.replace grouped projected (base :: prev)
      | _ -> passthrough := child :: !passthrough)
    others;
  let grouped =
    Hashtbl.to_seq grouped
    |> List.of_seq
    |> List.map (fun (projected, bases) ->
         project_expr (normalize_or (List.rev bases)) projected)
  in
  let others = List.rev !passthrough @ grouped |> stable_uniq in
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
    make (Or children) combined_mask

and shift_l t amount =
  if amount = 0
  then t
  else if t.mask = 0
  then const 0
  else
    match t.node with
    | Const n -> const (n lsl amount)
    | _ -> make (ShiftL (t, amount)) (t.mask lsl amount)

and dropped_low_bits amount =
  if amount <= 0 then 0 else (1 lsl amount) - 1

and shift_r t amount =
  if amount = 0
  then t
  else if t.mask = 0
  then const 0
  else
    match t.node with
    | Const n -> const (n lsr amount)
    | _ ->
      make
        (ShiftR (t, amount))
        ((t.mask land lnot (dropped_low_bits amount)) lsr amount)

and xor_ left right = normalize_xor2 left right

and xor_mask t bits = normalize_xor t bits

and and_ children = normalize_and children

and or_ children = normalize_or children

and project t projected =
  let projected = max 0 projected land t.mask in
  match Hashtbl.find_opt project_cache (t.id, projected) with
  | Some projected -> projected
  | None ->
    let projected_expr =
      if projected = 0
      then const 0
      else if projected = t.mask
      then t
      else
        match t.node with
        | Const n -> const (n land projected)
        | Var _ -> make (And [ t; const projected ]) projected
        | Xor2 (left, right) ->
          normalize_xor2 (project left projected) (project right projected)
        | Xor (child, bits) ->
          normalize_xor (project child projected) (bits land projected)
        | ShiftL (child, amount) ->
          shift_l (project child (projected lsr amount)) amount
        | ShiftR (child, amount) ->
          shift_r (project child (projected lsl amount)) amount
        | And children ->
          normalize_and (List.map (fun child -> project child projected) children)
        | Or children ->
          normalize_or (List.map (fun child -> project child projected) children)
    in
    Hashtbl.replace project_cache (t.id, projected) projected_expr;
    projected_expr

and known_bits (t : t) =
  match Hashtbl.find_opt known_bits_cache t.id with
  | Some facts -> facts
  | None ->
    let facts =
      match t.node with
      | Const n -> { known0 = lnot n; known1 = n }
      | Var _ -> { known0 = lnot t.mask; known1 = 0 }
      | Xor2 (left, right) ->
        let left = known_bits left in
        let right = known_bits right in
        { known0 =
            (left.known0 land right.known0) lor (left.known1 land right.known1);
          known1 =
            (left.known0 land right.known1) lor (left.known1 land right.known0)
        }
      | Xor (child, bits) ->
        let child = known_bits child in
        { known0 = (child.known0 land lnot bits) lor (child.known1 land bits);
          known1 = (child.known1 land lnot bits) lor (child.known0 land bits)
        }
      | And children ->
        { known0 =
            List.fold_left
              (fun acc child -> acc lor (known_bits child).known0)
              0
              children;
          known1 =
            List.fold_left
              (fun acc child -> acc land (known_bits child).known1)
              all_bits
              children
        }
      | Or children ->
        { known0 =
            List.fold_left
              (fun acc child -> acc land (known_bits child).known0)
              all_bits
              children;
          known1 =
            List.fold_left
              (fun acc child -> acc lor (known_bits child).known1)
              0
              children
        }
      | ShiftL (child, amount) ->
        let child = known_bits child in
        { known0 =
            (lnot t.mask)
            lor (((child.known0 lsl amount) land t.mask) lor lowmask amount);
          known1 = (child.known1 lsl amount) land t.mask
        }
      | ShiftR (child, amount) ->
        let child = known_bits child in
        let highmask =
          if amount <= 0
          then 0
          else lnot (lowmask (Sys.int_size - 1 - amount))
        in
        { known0 = (lnot t.mask) lor (child.known0 lsr amount) lor highmask;
          known1 = child.known1 lsr amount
        }
    in
    Hashtbl.replace known_bits_cache t.id facts;
    facts

and simplify_known_demand t demand =
  let demand = max 0 demand in
  let demand = demand land (t.mask lor 0) in
  match Hashtbl.find_opt simplify_cache (t.id, demand) with
  | Some simplified -> simplified
  | None ->
    let simplified =
      if demand = 0
      then const 0
      else
        let bits = known_bits t in
        let unknown = demand land lnot (bits.known0 lor bits.known1) in
        if unknown = 0
        then const (bits.known1 land demand)
        else
          match t.node with
          | Const n -> const (n land demand)
          | Var _ ->
            project t demand
          | Xor2 (left, right) ->
            normalize_xor2
              (simplify_known_demand left demand)
              (simplify_known_demand right demand)
          | Xor (child, bits) ->
            normalize_xor (simplify_known_demand child demand) (bits land demand)
          | ShiftL (child, amount) ->
            shift_l (simplify_known_demand child (demand lsr amount)) amount
          | ShiftR (child, amount) ->
            shift_r (simplify_known_demand child (demand lsl amount)) amount
          | And children ->
            let kb = Array.of_list (List.map known_bits children) in
            let may1 = Array.map (fun kb -> lnot kb.known0) kb in
            let n = Array.length may1 in
            let prefix = Array.make (n + 1) all_bits in
            let suffix = Array.make (n + 1) all_bits in
            for i = 0 to n - 1 do
              prefix.(i + 1) <- prefix.(i) land may1.(i)
            done;
            for i = n - 1 downto 0 do
              suffix.(i) <- suffix.(i + 1) land may1.(i)
            done;
            let children =
              List.mapi
                (fun i child ->
                  simplify_known_demand child (demand land prefix.(i) land suffix.(i + 1)))
                children
            in
            normalize_and children
          | Or children ->
            let kb = Array.of_list (List.map known_bits children) in
            let may0 = Array.map (fun kb -> lnot kb.known1) kb in
            let n = Array.length may0 in
            let prefix = Array.make (n + 1) all_bits in
            let suffix = Array.make (n + 1) all_bits in
            for i = 0 to n - 1 do
              prefix.(i + 1) <- prefix.(i) land may0.(i)
            done;
            for i = n - 1 downto 0 do
              suffix.(i) <- suffix.(i + 1) land may0.(i)
            done;
            let children =
              List.mapi
                (fun i child ->
                  simplify_known_demand child (demand land prefix.(i) land suffix.(i + 1)))
                children
            in
            normalize_or children
    in
    Hashtbl.replace simplify_cache (t.id, demand) simplified;
    simplified

and simplify t demand = simplify_known_demand t demand

and mask t requested =
  let requested = max 0 requested land t.mask in
  if requested = 0
  then const 0
  else if requested = t.mask
  then t
  else
    match t.node with
    | Const n -> const (n land requested)
    | Xor2 (left, right) ->
      normalize_xor2 (mask left requested) (mask right requested)
    | Xor (child, bits) -> normalize_xor (mask child requested) (bits land requested)
    | ShiftL (child, amount) -> shift_l (mask child (requested lsr amount)) amount
    | ShiftR (child, amount) -> shift_r (mask child (requested lsl amount)) amount
    | Var _ | And _ | Or _ -> make (And [ t; const requested ]) requested

let is_atomic (t : t) =
  match t.node with
  | Var _ | Const _ -> true
  | _ -> false

let merge_projection projected_by mask =
  match projected_by with
  | None -> Some mask
  | Some projected -> Some (projected land mask)

let projection_through_shift projected_by amount dir =
  Option.map
    (fun projected ->
      match dir with
      | `Left -> projected lsr amount
      | `Right -> projected lsl amount)
    projected_by

let render_xor_as_lnot projected_by bits =
  match projected_by with
  | Some projected when projected <> 0 && projected land lnot bits = 0 -> true
  | _ -> false

let child_renders_as_lnot projected_by child =
  match child.node with
  | Xor (_, bits) -> render_xor_as_lnot projected_by bits
  | _ -> false

let rec render_with_projection projected_by (t : t) =
  let render_unary_operand child =
    let rendered = render_with_projection projected_by child in
    if is_atomic child then rendered else "(" ^ rendered ^ ")"
  in
  let render_operand child =
    let rendered = render_with_projection projected_by child in
    if is_atomic child || child_renders_as_lnot projected_by child
    then rendered
    else "(" ^ rendered ^ ")"
  in
  match t.node with
  | Var name -> name
  | Const n -> bits_literal n
  | Xor2 (left, right) ->
    Printf.sprintf "%s lxor %s" (render_operand left) (render_operand right)
  | Xor (child, bits) ->
    if render_xor_as_lnot projected_by bits
    then Printf.sprintf "lnot %s" (render_unary_operand child)
    else Printf.sprintf "%s lxor %s" (render_operand child) (bits_literal bits)
  | ShiftL (child, amount) ->
    let projected_by = projection_through_shift projected_by amount `Left in
    let rendered = render_with_projection projected_by child in
    let rendered =
      if is_atomic child then rendered else "(" ^ rendered ^ ")"
    in
    Printf.sprintf "%s lsl %d" rendered amount
  | ShiftR (child, amount) ->
    let projected_by = projection_through_shift projected_by amount `Right in
    let rendered = render_with_projection projected_by child in
    let rendered =
      if is_atomic child then rendered else "(" ^ rendered ^ ")"
    in
    Printf.sprintf "%s lsr %d" rendered amount
  | And children ->
    let projected_by = merge_projection projected_by t.mask in
    String.concat
      " land "
      (List.map
         (fun child ->
           let rendered = render_with_projection projected_by child in
           if is_atomic child || child_renders_as_lnot projected_by child
           then rendered
           else "(" ^ rendered ^ ")")
         children)
  | Or children ->
    String.concat " lor " (List.map render_operand children)

let render (t : t) = render_with_projection None t

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
      | (indent, Nest (indent', doc)) :: rest ->
        fits remaining ((indent + indent', doc) :: rest)
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
        best col ((indent + indent', doc) :: rest)
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
      Pretty.(
        first
        ^^ nest 2
             (List.fold_left
                (fun acc doc -> acc ^^ line ^^ text op ^^ text " " ^^ doc)
                nil
                rest))

let rec doc_of_expr_with_projection projected_by (t : t) =
  let unary_operand_doc child =
    if is_atomic child
    then Pretty.text (render_with_projection projected_by child)
    else parens (doc_of_expr_with_projection projected_by child)
  in
  let operand_doc child =
    if is_atomic child || child_renders_as_lnot projected_by child
    then Pretty.text (render_with_projection projected_by child)
    else parens (doc_of_expr_with_projection projected_by child)
  in
  match t.node with
  | Var name -> Pretty.text name
  | Const n -> Pretty.text (bits_literal n)
  | Xor2 (left, right) ->
    Pretty.(operand_doc left ^^ text " lxor " ^^ operand_doc right)
  | Xor (child, bits) ->
    if render_xor_as_lnot projected_by bits
    then Pretty.(text "lnot " ^^ unary_operand_doc child)
    else Pretty.(operand_doc child ^^ text " lxor " ^^ text (bits_literal bits))
  | ShiftL (child, amount) ->
    let projected_by = projection_through_shift projected_by amount `Left in
    let child_doc =
      if is_atomic child || child_renders_as_lnot projected_by child
      then Pretty.text (render_with_projection projected_by child)
      else parens (doc_of_expr_with_projection projected_by child)
    in
    Pretty.(child_doc ^^ text " lsl " ^^ text (string_of_int amount))
  | ShiftR (child, amount) ->
    let projected_by = projection_through_shift projected_by amount `Right in
    let child_doc =
      if is_atomic child || child_renders_as_lnot projected_by child
      then Pretty.text (render_with_projection projected_by child)
      else parens (doc_of_expr_with_projection projected_by child)
    in
    Pretty.(child_doc ^^ text " lsr " ^^ text (string_of_int amount))
  | And children ->
    let projected_by = merge_projection projected_by t.mask in
    infix_chain_doc "land" (List.map (fun child ->
        if is_atomic child || child_renders_as_lnot projected_by child
        then Pretty.text (render_with_projection projected_by child)
        else parens (doc_of_expr_with_projection projected_by child)) children)
  | Or children -> infix_chain_doc "lor" (List.map operand_doc children)

let render_pretty ?width t =
  Pretty.render ?width (doc_of_expr_with_projection None t)

let cse ?(avoid = []) root =
  let counts = Hashtbl.create 64 in
  let rec count (t : t) =
    let k = key t in
    Hashtbl.replace counts k (Option.value (Hashtbl.find_opt counts k) ~default:0 + 1);
    match t.node with
    | Var _ | Const _ -> ()
    | Xor2 (left, right) ->
      count left;
      count right
    | Xor (child, _) | ShiftL (child, _) | ShiftR (child, _) ->
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
      | Xor2 (left, right) ->
        xor_ (rewrite ~is_root:false left) (rewrite ~is_root:false right)
      | Xor (child, bits) -> xor_mask (rewrite ~is_root:false child) bits
      | ShiftL (child, amount) -> shift_l (rewrite ~is_root:false child) amount
      | ShiftR (child, amount) -> shift_r (rewrite ~is_root:false child) amount
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
