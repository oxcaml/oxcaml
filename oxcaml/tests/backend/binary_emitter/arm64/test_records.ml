(* Test records and data structures *)

(* Simple record *)
type point =
  { x : int;
    y : int
  }

(* Record with mutable field *)
type counter =
  { mutable count : int;
    name : string
  }

(* Nested records *)
type rect =
  { top_left : point;
    bottom_right : point
  }

(* Record operations *)
let make_point x y = { x; y }

let add_points p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }

let scale_point s p = { x = s * p.x; y = s * p.y }

(* Functional update *)
let move_x p dx = { p with x = p.x + dx }

(* Pattern matching on records *)
let point_to_string { x; y } =
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let is_origin = function { x = 0; y = 0 } -> true | _ -> false

(* Mutable record operations *)
let make_counter name = { count = 0; name }

let increment c = c.count <- c.count + 1

let get_count c = c.count

(* Nested record access *)
let rect_width r = r.bottom_right.x - r.top_left.x

let rect_height r = r.bottom_right.y - r.top_left.y

let rect_area r = rect_width r * rect_height r

(* Arrays of records *)
let points_sum arr =
  let sum = ref { x = 0; y = 0 } in
  for i = 0 to Array.length arr - 1 do
    sum := add_points !sum arr.(i)
  done;
  !sum

(* Entry point *)
let () =
  let p1 = make_point 3 4 in
  let p2 = make_point 1 2 in
  let _ = add_points p1 p2 in
  let _ = scale_point 2 p1 in
  let _ = move_x p1 10 in
  let _ = point_to_string p1 in
  let _ = is_origin (make_point 0 0) in
  let _ = is_origin p1 in
  let c = make_counter "test" in
  increment c;
  increment c;
  let _ = get_count c in
  let r = { top_left = make_point 0 0; bottom_right = make_point 10 5 } in
  let _ = rect_width r in
  let _ = rect_height r in
  let _ = rect_area r in
  let arr = [| make_point 1 1; make_point 2 2; make_point 3 3 |] in
  let _ = points_sum arr in
  ()
