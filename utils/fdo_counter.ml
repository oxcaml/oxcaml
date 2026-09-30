type edge =
  | Then
  | Else
  | Switch_case of int
  | Callsite

module Hash = struct
  type t = int32

  let of_int32 t = t

  let equal = Int32.equal

  let compare = Int32.unsigned_compare

  let is_function_entry t = Int32.equal (Int32.logand t 1l) 1l

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    (* Already mixed. *)
    let hash t = Int32.to_int t land max_int
  end)
end

module Function_body_hash = struct
  type t = int32

  let of_int32 t = t

  let equal = Int32.equal
end

(* Positions and function ids are hashed structurally (see [Fdo_prehash]) from
   their components and the prehashes of those they contain. *)
type prehash = Fdo_prehash.t

(* Each constructor's prehash starts from a tag of its own. *)
let prehash_tag tag components =
  List.fold_left Fdo_prehash.combine (Fdo_prehash.int tag) components

let prehash_edge = function
  | Then -> prehash_tag 0 []
  | Else -> prehash_tag 1 []
  | Callsite -> prehash_tag 2 []
  | Switch_case n -> prehash_tag 3 [Fdo_prehash.int n]

type function_id =
  | Function of
      { unmangled_name : string;
        discriminator : int;
        prehash : prehash
      }
  | Specialized of
      { unspecialized : function_id;
        specialization_site : position;
        prehash : prehash
      }

and position =
  | Function_entry of function_id
  | Position of
      { function_id : function_id;
        function_body_hash : Function_body_hash.t;
        ast_pos : int;
        edge : edge;
        prehash : prehash
      }
  | Instantiation_site of
      { module_initializer : function_id;
        prehash : prehash
      }

let prehash_function_id = function
  | Function { prehash; _ } | Specialized { prehash; _ } -> prehash

let prehash_position = function
  | Function_entry fn -> prehash_function_id fn
  | Position { prehash; _ } | Instantiation_site { prehash; _ } -> prehash

let function_id ~unmangled_name ~discriminator =
  let prehash =
    prehash_tag 4
      [Fdo_prehash.string unmangled_name; Fdo_prehash.int discriminator]
  in
  Function { unmangled_name; discriminator; prehash }

let specialized ~unspecialized ~specialization_site =
  let prehash =
    prehash_tag 5
      [prehash_function_id unspecialized; prehash_position specialization_site]
  in
  Specialized { unspecialized; specialization_site; prehash }

let function_entry fn = Function_entry fn

let position ~function_id ~function_body_hash ~ast_pos ~edge =
  let prehash =
    prehash_tag 6
      [ prehash_function_id function_id;
        Fdo_prehash.int (Int32.to_int function_body_hash);
        Fdo_prehash.int ast_pos;
        prehash_edge edge ]
  in
  Position { function_id; function_body_hash; ast_pos; edge; prehash }

let instantiation_site module_initializer =
  let prehash = prehash_tag 7 [prehash_function_id module_initializer] in
  Instantiation_site { module_initializer; prehash }

(* The 32-bit hash with the low bit replaced by the function-entry tag. *)
let hash_of_prehash ~is_function_entry prehash =
  Int32.logor
    (Int32.logand (Fdo_prehash.to_int32 prehash) (-2l))
    (if is_function_entry then 1l else 0l)

let hash_function_id fn =
  hash_of_prehash ~is_function_entry:true (prehash_function_id fn)

let hash_position position =
  hash_of_prehash
    ~is_function_entry:
      (match position with
      | Function_entry _ -> true
      | Position _ | Instantiation_site _ -> false)
    (prehash_position position)

(* The canonical strings, optionally embedded into the metadata for readable
   dumps. *)
let rec function_id_to_string = function
  | Function { unmangled_name; discriminator; prehash = _ } ->
    Printf.sprintf "%S:%d" unmangled_name discriminator
  | Specialized { unspecialized; specialization_site; prehash = _ } ->
    Printf.sprintf "%s@(%s)"
      (function_id_to_string unspecialized)
      (position_to_string specialization_site)

and position_to_string = function
  | Function_entry fn -> function_id_to_string fn
  | Position { function_id; function_body_hash; ast_pos; edge; prehash = _ } ->
    let edge =
      match edge with
      | Then -> "then"
      | Else -> "else"
      | Switch_case n -> Printf.sprintf "case(%d)" n
      | Callsite -> "call"
    in
    Printf.sprintf "%s:%08lx:%d:%s"
      (function_id_to_string function_id)
      function_body_hash ast_pos edge
  | Instantiation_site { module_initializer; prehash = _ } ->
    Printf.sprintf "module(%s)" (function_id_to_string module_initializer)

type t =
  { position : position;
    inlining_stack : position list
  }

let inline t ~at =
  { t with
    inlining_stack = t.inlining_stack @ (at.position :: at.inlining_stack)
  }

let is_function_entry = function
  | Function_entry _ -> true
  | Position _ | Instantiation_site _ -> false

let equal_edge a b =
  match a, b with
  | Then, Then | Else, Else | Callsite, Callsite -> true
  | Switch_case a, Switch_case b -> Int.equal a b
  | (Then | Else | Callsite | Switch_case _), _ -> false

let rec equal_function_id a b =
  match a, b with
  | Function a, Function b ->
    String.equal a.unmangled_name b.unmangled_name
    && Int.equal a.discriminator b.discriminator
  | Specialized a, Specialized b ->
    equal_function_id a.unspecialized b.unspecialized
    && equal_position a.specialization_site b.specialization_site
  | (Function _ | Specialized _), _ -> false

and equal_position a b =
  match a, b with
  | Function_entry a, Function_entry b -> equal_function_id a b
  | Instantiation_site a, Instantiation_site b ->
    equal_function_id a.module_initializer b.module_initializer
  | Position a, Position b ->
    equal_function_id a.function_id b.function_id
    && Function_body_hash.equal a.function_body_hash b.function_body_hash
    && Int.equal a.ast_pos b.ast_pos
    && equal_edge a.edge b.edge
  | (Function_entry _ | Position _ | Instantiation_site _), _ -> false

let equal a b =
  equal_position a.position b.position
  && List.equal equal_position a.inlining_stack b.inlining_stack

let specialize t ~at =
  let specialize_function_id function_id =
    List.fold_left
      (fun unspecialized specialization_site ->
        match specialization_site with
        | Position _ | Instantiation_site _ ->
          specialized ~unspecialized ~specialization_site
        | Function_entry _ ->
          invalid_arg
            "Fdo_counter.specialize: function entry used as a call site")
      function_id
      (at.position :: at.inlining_stack)
  in
  let specialize_position = function
    | Function_entry fn -> Function_entry (specialize_function_id fn)
    | Position
        { function_id = fn; function_body_hash; ast_pos; edge; prehash = _ } ->
      position
        ~function_id:(specialize_function_id fn)
        ~function_body_hash ~ast_pos ~edge
    | Instantiation_site { module_initializer; prehash = _ } ->
      instantiation_site (specialize_function_id module_initializer)
  in
  (* Only the outermost position belongs to the copied function. Inner positions
     belong to callees inlined into it and retain their own identities. *)
  let rec outermost = function
    | [] -> []
    | [p] -> [specialize_position p]
    | p :: rest -> p :: outermost rest
  in
  match t.inlining_stack with
  | [] -> { t with position = specialize_position t.position }
  | stack -> { t with inlining_stack = outermost stack }

let to_string { position; inlining_stack } =
  String.concat " <- "
    (List.map position_to_string (position :: inlining_stack))

type hashed = Hash.t list

let hash { position; inlining_stack } =
  List.map hash_position (position :: inlining_stack)

let add_all existing counters =
  existing
  @ List.filter
      (fun counter -> not (List.exists (equal counter) existing))
      counters
