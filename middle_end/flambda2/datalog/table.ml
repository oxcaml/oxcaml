(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2024--2025 OCamlPro SAS                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Datalog_imports

module Int = struct
  include Numbers.Int
  module Tree = Patricia_tree.Make (Numbers.Int)
  module Map = Tree.Map
end

type _ result_repr =
  | Unit_repr : unit result_repr
  | Lattice_repr :
      { print : Format.formatter -> 'a -> unit;
        meet : 'a -> 'a -> 'a;
        diff_or_null : 'a -> 'a -> 'a Or_null.t
      }
      -> 'a result_repr

let unit_repr = Unit_repr

let lattice_repr ~print ~meet ~diff_or_null =
  Lattice_repr { print; meet; diff_or_null }

let result_repr_print (type t) (repr : t result_repr) :
    Format.formatter -> t -> unit =
  match repr with
  | Unit_repr -> fun ppf () -> Format.fprintf ppf "()"
  | Lattice_repr { print; _ } -> print

let result_repr_union (type t) (repr : t result_repr) : t -> t -> t =
  match repr with
  | Unit_repr -> fun () () -> ()
  | Lattice_repr { meet; _ } -> meet

let result_repr_diff_or_null (type t) (repr : t result_repr) :
    t -> t -> t Or_null.t =
  match repr with
  | Unit_repr -> fun () () -> Or_null.null
  | Lattice_repr { diff_or_null; _ } -> diff_or_null

let result_repr_is_unit (type v) (repr : v result_repr) :
    (v, unit) Type.eq option =
  match repr with Unit_repr -> Some Equal | Lattice_repr _ -> None

let rec union : type t k v.
    (t, k, v) Column.hlist -> v result_repr -> t -> t -> t =
 fun columns repr t1 t2 ->
  match columns with
  | [] -> result_repr_union repr t1 t2
  | column :: columns ->
    Column.union_total column (fun t1 t2 -> union columns repr t1 t2) t1 t2

let rec concat : type t k v. (t, k, v) Column.hlist -> earlier:t -> later:t -> t
    =
 fun columns ~earlier ~later ->
  match columns with
  | [] -> later
  | column :: columns ->
    Column.union_total column
      (fun earlier later -> concat columns ~earlier ~later)
      earlier later

module Id = struct
  type (!'t, !'k, !'v) t =
    { id : ('t * 'k) Type.Id.t;
      name : string;
      columns : ('t, 'k, 'v) Column.hlist;
      result_repr : 'v result_repr;
      provenance : bool
    }

  let name { name; _ } = name

  type ('k, 'v) poly = Id : ('t, 'k, 'v) t -> ('k, 'v) poly

  let print ppf t = Format.fprintf ppf "%s" t.name

  let hash { id; _ } = Hashtbl.hash (Type.Id.uid id)

  let equal { id = id1; _ } { id = id2; _ } = Type.Id.uid id1 = Type.Id.uid id2

  let[@inline] provably_equal_keys_exn (type a k v a' k' v') (r1 : (a, k, v) t)
      (r2 : (a', k', v') t) : (k, k') Type.eq =
    match Type.Id.provably_equal r1.id r2.id with
    | Some Equal -> Equal
    | None -> Misc.fatal_error "Inconsistent type for uid."

  let[@inline] provably_equal_exn (type a k v a' k' v') (r1 : (a, k, v) t)
      (r2 : (a', k', v') t) : (a, a') Type.eq =
    match Type.Id.provably_equal r1.id r2.id with
    | Some Equal -> Equal
    | None -> Misc.fatal_error "Inconsistent type for uid."

  let compare { id = id1; _ } { id = id2; _ } =
    compare (Type.Id.uid id1) (Type.Id.uid id2)

  let create ~provenance ~name ~columns ~result_repr =
    { id = Type.Id.make (); name; columns; result_repr; provenance }

  let has_provenance { provenance; _ } = provenance

  let[@inline] result_repr { result_repr; _ } = result_repr

  let[@inline] columns { columns; _ } = columns

  let[@inline] uid { id; _ } = Type.Id.uid id

  let[@inline] cast_exn (type a b k v k' v') (r1 : (a, k, v) t)
      (r2 : (b, k', v') t) (t : a) : b =
    match Type.Id.provably_equal r1.id r2.id with
    | Some Equal -> t
    | None -> Misc.fatal_error "Inconsistent type for uid."

  module Map = struct
    module type S = sig
      type ('t, 'k, 'v) id = ('t, 'k, 'v) t

      type t

      type 'a elt

      type binding = B : ('t, 'k, 'v) id * 't elt -> binding

      val empty : t

      val singleton : ('t, 'k, 'v) id -> 't elt -> t

      val add : ('t, 'k, 'v) id -> 't elt -> t -> t

      val remove : ('t, 'k, 'v) id -> t -> t

      val find : ('t, 'k, 'v) id -> t -> 't elt

      val fold : (binding -> 'a -> 'a) -> t -> 'a -> 'a

      val iter : (binding -> unit) -> t -> unit
    end

    module Make (T : sig
      type 'a t
    end) =
    struct
      type ('t, 'k, 'v) id = ('t, 'k, 'v) t

      type 'a elt = 'a T.t

      type binding = B : ('t, 'k, 'v) id * 't elt -> binding

      type t = binding Int.Map.t

      let empty = Int.Map.empty

      let singleton key elt = Int.Map.singleton (uid key) (B (key, elt))

      let add key elt t = Int.Map.add (uid key) (B (key, elt)) t

      let remove key t = Int.Map.remove (uid key) t

      let find (type t k v) (key : (t, k, v) id) t : t elt =
        let (B (key', elt)) = Int.Map.find (uid key) t in
        let Equal = provably_equal_exn key key' in
        elt

      let fold f t acc = Int.Map.fold (fun _ binding acc -> f binding acc) t acc

      let iter f t = Int.Map.iter (fun _ binding -> f binding) t
    end

    module Or_null_sender = Make (Or_null_sender)
    module Or_null_receiver = Make (Or_null_receiver)
  end

  module Tbl = struct
    module type S = sig
      type ('t, 'k, 'v) id = ('t, 'k, 'v) t

      type 'a elt

      type binding = B : ('a, _, _) id * 'a elt -> binding

      type t

      val create : int -> t

      val replace : t -> ('t, 'k, 'v) id -> 't elt -> unit

      val find : t -> ('t, 'k, 'v) id -> 't elt

      val iter : (binding -> unit) -> t -> unit
    end

    module Make (T : sig
      type 'a t
    end) =
    struct
      type ('t, 'k, 'v) id = ('t, 'k, 'v) t

      type 'a elt = 'a T.t

      type binding = B : ('a, _, _) t * 'a elt -> binding

      type t = binding Int.Tbl.t

      let create n = Int.Tbl.create n

      let replace t id elt = Int.Tbl.replace t (uid id) (B (id, elt))

      let find (type t k v) t (id : (t, k, v) id) : t elt =
        let (B (id', elt)) = Int.Tbl.find t (uid id) in
        let Equal = provably_equal_exn id id' in
        elt

      let iter f t = Int.Tbl.iter (fun _ binding -> f binding) t
    end
  end
end

let iter id f table =
  let columns = Id.columns id in
  Column.iter_hlist columns f table

let print id ?(pp_sep = Format.pp_print_cut) pp_row ppf table =
  let first = ref true in
  iter id
    (fun keys value ->
      if !first then first := false else pp_sep ppf ();
      pp_row keys value)
    table

let print_table (type t k v) (id : (t, k, v) Id.t) ppf table =
  let repr = Id.result_repr id in
  let print_value ppf value =
    match result_repr_is_unit repr with
    | None -> Format.fprintf ppf " =@ %a" (result_repr_print repr) value
    | Some Equal -> ()
  in
  Format.fprintf ppf "@[<v>%a@]"
    (print id (fun keys value ->
         Format.fprintf ppf "@[%a(%a)%a.@]" Id.print id
           (Column.print_keys id.columns)
           keys print_value value))
    table

let print id ppf table =
  let header = Format.asprintf "%a" Id.print id in
  Format.fprintf ppf "@[<v>%s@ %s@ %a@]" header
    (String.make (String.length header) '=')
    (print_table id) table

module Map = struct
  type binding = Binding : ('t, 'k, 'v) Id.t * 't -> binding

  type t = binding Int.Map.t

  let print ppf tables =
    let first = ref true in
    Format.fprintf ppf "@[<v>";
    Int.Map.iter
      (fun _ (Binding (id, table)) ->
        if !first then first := false else Format.fprintf ppf "@ @ ";
        print id ppf table)
      tables;
    Format.fprintf ppf "@]"

  let get_or_null (type t k v) (id : (t, k, v) Id.t) tables : t Or_null.t =
    match Int.Map.find_opt (Id.uid id) tables with
    | Some (Binding (existing_id, table)) ->
      Or_null.this (Id.cast_exn existing_id id table)
    | None -> Or_null.null

  let set (type t k v) (id : (t, k, v) Id.t) (table : t) tables =
    if Column.is_empty_hlist (Id.columns id) table
    then Int.Map.remove (Id.uid id) tables
    else Int.Map.add (Id.uid id) (Binding (id, table)) tables

  let set_or_null id (table : _ Or_null.t) tables =
    match table with
    | Null -> Int.Map.remove (Id.uid id) tables
    | This table -> set id table tables

  let empty = Int.Map.empty

  let is_empty = Int.Map.is_empty

  let concat ~earlier:tables1 ~later:tables2 =
    Int.Map.union_total
      (fun _ (Binding (id1, table1)) (Binding (id2, table2)) ->
        let table =
          concat (Id.columns id1) ~earlier:table1
            ~later:(Id.cast_exn id2 id1 table2)
        in
        Binding (id1, table))
      tables1 tables2

  let fold ~f m ~init = Int.Map.fold (fun _ binding acc -> f binding acc) m init

  let send t senders =
    Int.Map.iter
      (fun _ (Id.Map.Or_null_sender.B (tid, sender)) ->
        Channel.send_or_null sender (get_or_null tid t))
      senders
end
