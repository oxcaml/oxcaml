(* This module exists to prevent a dependency cycle with Types. *)

let rec compare_longidents ?(compare_strings = String.compare)
    (l1 : Longident.t) (l2 : Longident.t) =
  match (l1, l2) with
  | Lident s1, Lident s2 -> compare_strings s1 s2
  | Lident _, _ -> -1
  | _, Lident _ -> 1
  | Ldot (l1, s1), Ldot (l2, s2) ->
    let c = compare_longidents l1 l2 in
    if c = 0 then compare_strings s1 s2 else c
  | Lapply (l1, l'1), Lapply (l2, l'2) ->
    let c = compare_longidents l1 l2 in
    if c = 0 then compare_longidents l'1 l'2 else c
  | Ldot _, Lapply _ -> -1
  | Lapply _, Ldot _ -> 1

module Lid_set = Set.Make (struct
  type t = Longident.t
  let compare a b = compare_longidents a b
end)

module Paths = struct
  module T = struct
    type t = Shape.Sig_component_kind.t * Path.t

    (* Since we are versing these paths in a different structure (the
        priority queue) before shortening, it does not seems useful tu use a
        custom path comparison function here. *)

    let compare (_, p1) (_, p2) = Path.compare p1 p2
  end

  include Set.Make (T)
end

let pp_paths ppf t =
  let pp_sep ppf () = Format.fprintf ppf ";@;" in
  let paths = Paths.elements t |> List.map (fun (_, p) -> p) in
  Format.pp_print_list ~pp_sep Path.print ppf paths

module String_map = Map.Make (String)

module Lid_trie = struct
  type t = Trie of Paths.t * t String_map.t

  let pp_paths fmt paths =
    let open Format in
    fprintf fmt "[%a]" pp_paths paths

  let rec pp fmt (Trie (paths, tries)) =
    let open Format in
    let pp_map fmt (id, trie) =
      Format.fprintf fmt "@[<v 2>%s: %a@]" id pp trie
    in
    Format.fprintf fmt "%a :> %a" pp_paths paths (pp_print_seq pp_map)
      (String_map.to_seq tries)

  let empty = Trie (Paths.empty, String_map.empty)

  let is_empty (Trie (_, children)) = String_map.is_empty children

  let node ?(children = String_map.empty) paths = Trie (paths, children)

  let trie_of_lid ?children lid paths =
    let rec aux acc lid =
      match (lid : Longident.t) with
      | Lident id ->
        let map = String_map.singleton id acc in
        Trie (Paths.empty, map)
      | Ldot (lid, id) ->
        let acc = Trie (Paths.empty, String_map.singleton id acc) in
        aux acc lid
      | Lapply (lid, arg_lid) ->
        let arg =
          let acc = Trie (Paths.empty, String_map.singleton ")" acc) in
          aux acc arg_lid
        in
        aux (Trie (Paths.empty, String_map.singleton "(" arg)) lid
    in
    aux (node ?children paths) lid

  let rec union (Trie (p1, m1)) (Trie (p2, m2)) =
    Trie
      ( Paths.union p1 p2,
        String_map.union (fun _key t1 t2 -> Some (union t1 t2)) m1 m2 )

  let add lid path t =
    let t' = trie_of_lid lid (Paths.singleton path) in
    union t t'

  let take name (Trie (paths, tries)) =
    let l, t, r = String_map.split name tries in
    let t = Option.map (fun t -> Trie (paths, String_map.singleton name t)) t in
    (t, Trie (paths, String_map.union (fun _ _ _ -> assert false) l r))

  let rec reach (Trie (_, tries) as t) lid =
    match (lid : Longident.t) with
    | Lident name -> String_map.find_opt name tries
    | Ldot (lid, name) ->
      let parent = reach t lid in
      Option.bind parent (fun (Trie (_, tries)) ->
          String_map.find_opt name tries)
    | Lapply (lid, arg_lid) ->
      let parent = reach t lid in
      Option.bind parent (fun (Trie (_, tries)) ->
          let arg_trie = String_map.find_opt "(" tries in
          let arg_enc = Option.bind arg_trie (fun t -> reach t arg_lid) in
          Option.bind arg_enc (fun (Trie (_, tries)) ->
              String_map.find_opt ")" tries))

  let to_seq t =
    let rec aux lid_acc (Trie (paths, tries)) seq =
      let seq () =
        String_map.fold
          (fun name t acc ->
            let lid =
              match (lid_acc, name) with
              | None :: tl, _ -> Some (Longident.Lident name) :: tl
              | l, "(" -> None :: l
              | Some arg :: Some lid :: tl, ")" ->
                Some (Longident.Lapply (lid, arg)) :: tl
              | Some lid :: tl, _ -> Some (Longident.Ldot (lid, name)) :: tl
              | _ -> assert false
            in
            aux lid t acc)
          tries seq
      in
      if not (Paths.is_empty paths) then
        Seq.Cons ((Option.get (List.hd lid_acc), paths), seq)
      else seq ()
    in
    fun () -> aux [ None ] t Seq.Nil

  let size t =
    let rec aux acc (Trie (paths, tries)) =
      String_map.fold
        (fun _ t acc -> aux (1 + acc) t)
        tries
        (Paths.cardinal paths + acc)
    in
    aux 0 t

  let pp_lid_paths ppf (lid, paths) =
    Format.fprintf ppf "%a %a" Pprintast.longident lid pp_paths paths
  let pp_seq fmt t =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.fprintf fmt "%a"
      (Format.pp_print_seq ~pp_sep pp_lid_paths)
      (to_seq t)
end

type t = Lid_trie.t
let empty = Lid_trie.empty
let add = Lid_trie.add
let union = Lid_trie.union
let singleton lid path = Lid_trie.trie_of_lid lid (Paths.singleton path)

let pp = Lid_trie.pp_seq
