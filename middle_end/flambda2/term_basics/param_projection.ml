type source =
  | Param of int
  | My_closure

type projection =
  | Get_tag of projection
  | Untag of projection
  | Is_int of projection
  | Block_load of
      { field : Target_ocaml_int.t;
        projection : projection
      }
  | Identity

type t =
  { source : source;
    projection : projection
  }

let param i = { source = Param i; projection = Identity }

let my_closure = { source = My_closure; projection = Identity }

let get_tag p = { p with projection = Get_tag p.projection }

let untag p = { p with projection = Untag p.projection }

let is_int p = { p with projection = Is_int p.projection }

let block_load field p =
  { p with projection = Block_load { field; projection = p.projection } }

let compare_source s1 s2 =
  match s1, s2 with
  | Param i1, Param i2 -> Int.compare i1 i2
  | My_closure, My_closure -> 0
  | Param _, My_closure -> -1
  | My_closure, Param _ -> 1

let rec compare_projection p1 p2 =
  match p1, p2 with
  | Identity, Identity -> 0
  | Get_tag p1, Get_tag p2 -> compare_projection p1 p2
  | Untag p1, Untag p2 -> compare_projection p1 p2
  | Is_int p1, Is_int p2 -> compare_projection p1 p2
  | ( Block_load { field = f1; projection = p1 },
      Block_load { field = f2; projection = p2 } ) -> (
    match Target_ocaml_int.compare f1 f2 with
    | 0 -> compare_projection p1 p2
    | c -> c)
  | Identity, _ -> -1
  | _, Identity -> 1
  | Get_tag _, _ -> -1
  | _, Get_tag _ -> 1
  | Untag _, _ -> -1
  | _, Untag _ -> 1
  | Is_int _, _ -> -1
  | _, Is_int _ -> 1

let compare t1 t2 =
  match compare_source t1.source t2.source with
  | 0 -> compare_projection t1.projection t2.projection
  | c -> c

let equal t1 t2 = compare t1 t2 = 0

let[@ocamlformat "disable"] print_source ppf = function
  | Param i -> Format.fprintf ppf "(Param %d)" i
  | My_closure -> Format.fprintf ppf "My_closure"

let[@ocamlformat "disable"] rec print_projection source ppf = function
  | Identity -> print_source ppf source
  | Get_tag p -> Format.fprintf ppf "(Get_tag %a)" (print_projection source) p
  | Untag p -> Format.fprintf ppf "(Untag %a)" (print_projection source) p
  | Is_int p -> Format.fprintf ppf "(Is_int %a)" (print_projection source) p
  | Block_load { field; projection } ->
    Format.fprintf ppf "(Block_load %a %a)"
      Target_ocaml_int.print field (print_projection source) projection

let print ppf t = print_projection t.source ppf t.projection
