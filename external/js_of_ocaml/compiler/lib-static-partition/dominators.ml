open! Js_of_ocaml_compiler.Stdlib

let immediate_dominators ~successors ~root =
  (* Number the reachable nodes in postorder. *)
  let order = Int.Hashtbl.create 64 in
  let nodes = ref [] in
  let predecessors = Int.Hashtbl.create 64 in
  let visited = Int.Hashtbl.create 64 in
  let rec visit node =
    Int.Hashtbl.replace visited node ();
    List.iter (successors node) ~f:(fun successor ->
        Int.Hashtbl.replace
          predecessors
          successor
          (node :: Option.value ~default:[] (Int.Hashtbl.find_opt predecessors successor));
        if not (Int.Hashtbl.mem visited successor) then visit successor);
    Int.Hashtbl.replace order node (Int.Hashtbl.length order);
    nodes := node :: !nodes
  in
  visit root;
  let reverse_postorder = !nodes in
  let idom = Int.Hashtbl.create 64 in
  Int.Hashtbl.replace idom root root;
  let rec intersect a b =
    if a = b
    then a
    else
      let order_a = Int.Hashtbl.find order a in
      let order_b = Int.Hashtbl.find order b in
      if order_a < order_b
      then intersect (Int.Hashtbl.find idom a) b
      else intersect a (Int.Hashtbl.find idom b)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter reverse_postorder ~f:(fun node ->
        if node <> root
        then
          let processed =
            List.filter
              (Option.value ~default:[] (Int.Hashtbl.find_opt predecessors node))
              ~f:(fun predecessor -> Int.Hashtbl.mem idom predecessor)
          in
          match processed with
          | [] -> ()
          | first :: rest ->
              let dominator = List.fold_left rest ~init:first ~f:intersect in
              if
                match Int.Hashtbl.find_opt idom node with
                | Some previous -> previous <> dominator
                | None -> true
              then (
                Int.Hashtbl.replace idom node dominator;
                changed := true))
  done;
  fun node -> Int.Hashtbl.find idom node
