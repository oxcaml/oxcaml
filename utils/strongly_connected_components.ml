(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int = Numbers.Int

module Kosaraju : sig
  type component_graph =
    { sorted_connected_components : int list array;
      component_edges : int list array;
    }

  val component_graph : int list array -> component_graph
end = struct
  let transpose graph =
    let size = Array.length graph in
    let transposed = Array.make size [] in
    let add src dst = transposed.(src) <- dst :: transposed.(src) in
    Array.iteri (fun src dsts -> List.iter (fun dst -> add dst src) dsts)
      graph;
    transposed

  let depth_first_order (graph : int list array) : int array =
    let size = Array.length graph in
    let marked = Array.make size false in
    let stack = Array.make size ~-1 in
    let pos = ref 0 in
    let push i =
      stack.(!pos) <- i;
      incr pos
    in
    let rec aux node =
      if not marked.(node)
      then begin
        marked.(node) <- true;
        List.iter aux graph.(node);
        push node
      end
    in
    for i = 0 to size - 1 do
      aux i
    done;
    stack

  let mark order graph =
    let size = Array.length graph in
    let graph = transpose graph in
    let marked = Array.make size false in
    let id = Array.make size ~-1 in
    let count = ref 0 in
    let rec aux node =
      if not marked.(node)
      then begin
        marked.(node) <- true;
        id.(node) <- !count;
        List.iter aux graph.(node)
      end
    in
    for i = size - 1 downto 0 do
      let node = order.(i) in
      if not marked.(node)
      then begin
        aux order.(i);
        incr count
      end
    done;
    id, !count

  let kosaraju graph =
    let dfo = depth_first_order graph in
    let components, ncomponents = mark dfo graph in
    ncomponents, components

  type component_graph =
    { sorted_connected_components : int list array;
      component_edges : int list array;
    }

  let component_graph graph =
    let ncomponents, components = kosaraju graph in
    let id_scc = Array.make ncomponents [] in
    let component_graph = Array.make ncomponents Int.Set.empty in
    let add_component_dep node set =
      let node_deps = graph.(node) in
      List.fold_left (fun set dep -> Int.Set.add components.(dep) set)
        set node_deps
    in
    Array.iteri (fun node component ->
        id_scc.(component) <- node :: id_scc.(component);
        component_graph.(component) <-
          add_component_dep node (component_graph.(component)))
      components;
    { sorted_connected_components = id_scc;
      component_edges = Array.map Int.Set.elements component_graph;
    }
end

module type Id = sig
  type t

  module Set : sig
    type elt = t

    type t

    val empty : t

    val add : elt -> t -> t

    val elements : t -> elt list

    val iter : (elt -> unit) -> t -> unit

    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Map : sig
    type key = t

    type 'a t

    val empty : _ t

    val add : key -> 'a -> 'a t -> 'a t

    val cardinal : _ t -> int

    val bindings : 'a t -> (key * 'a) list

    val find : key -> 'a t -> 'a

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val mem : key -> 'a t -> bool
  end

  val print : Format.formatter -> t -> unit
end

module type S = sig
  module Id : Id

  type directed_graph = Id.Set.t Id.Map.t

  type component =
    | Has_loop of Id.t list
    | No_loop of Id.t

  val stable_connected_components_sorted_from_roots_to_leaf
    : (Id.t * Id.Set.t) list
    -> component array

  val connected_components_sorted_from_roots_to_leaf
     : directed_graph
    -> component array

  val component_graph : directed_graph -> (component * int list) array
end

module Make (Id : Id) = struct
  type directed_graph = Id.Set.t Id.Map.t

  type component =
    | Has_loop of Id.t list
    | No_loop of Id.t

  (* Ensure that the dependency graph does not have external dependencies. *)
  (* Note: this function is currently not used. *)
  let _check dependencies =
    Id.Map.iter (fun id set ->
        Id.Set.iter (fun v ->
            if not (Id.Map.mem v dependencies)
            then
              Misc.fatal_errorf "Strongly_connected_components.check: the \
                  graph has external dependencies (%a -> %a)"
               Id.print id Id.print v)
          set)
      dependencies

  let number ?(stable = false) (bindings : (Id.t * Id.Set.t) list) =
    let size = List.length bindings in
    let a = Array.of_list bindings in
    let forth = Array.map fst a in
    let back =
      let back = ref Id.Map.empty in
      for i = 0 to size - 1 do
        back := Id.Map.add forth.(i) i !back;
      done;
      !back
    in
    let integer_graph =
      Array.init size (fun i ->
        let _, dests = a.(i) in
        let integer_dests =
          Id.Set.fold (fun dest acc ->
              let v =
                try Id.Map.find dest back
                with Not_found ->
                  Misc.fatal_errorf
                    "Strongly_connected_components: missing dependency %a"
                    Id.print dest
              in
              v :: acc)
            dests []
        in
        if not stable then integer_dests
        else List.sort Int.compare integer_dests)
    in
    forth, integer_graph

  let rec int_list_mem x xs =
    match xs with
    | [] -> false
    | x' :: xs -> if Int.equal x x' then true else int_list_mem x xs

  let numbered_component_graph (forth, integer_graph) =
    let { Kosaraju. sorted_connected_components;
          component_edges } =
      Kosaraju.component_graph integer_graph
    in
    Array.mapi (fun component nodes ->
        match nodes with
        | [] -> assert false
        | [node] ->
          (if int_list_mem node integer_graph.(node)
           then Has_loop [forth.(node)]
           else No_loop forth.(node)),
            component_edges.(component)
        | _::_ ->
          (Has_loop (List.map (fun node -> forth.(node)) nodes)),
            component_edges.(component))
      sorted_connected_components

  let stable_component_graph graph =
    numbered_component_graph (number ~stable:true graph)

  let stable_connected_components_sorted_from_roots_to_leaf graph =
    Array.map fst (stable_component_graph graph)

  let component_graph graph =
    numbered_component_graph (number ~stable:false (Id.Map.bindings graph))

  let connected_components_sorted_from_roots_to_leaf graph =
    Array.map fst (component_graph graph)
end
