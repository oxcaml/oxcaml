(* TEST
 flags = "-extension layout_poly_alpha -warn-error +51+71";
 { bytecode; }
 { native; }
*)

let[@tail_mod_cons] rec copy (xs : #(int * int) list) =
  match xs with
  | [] -> []
  | x :: xs -> x :: (copy [@tailcall]) xs

let[@tail_mod_cons] rec repeat n (x : #(int * int)) =
  if n = 0 then [] else x :: (repeat [@tailcall]) (n - 1) x

let rec check_pairs expected = function
  | [] -> assert (expected = [])
  | #(a, b) :: xs ->
    match expected with
    | [] -> failwith "unexpected list element"
    | (c, d) :: expected ->
      assert (a = c && b = d);
      check_pairs expected xs

let () =
  check_pairs [] (copy []);
  check_pairs [1, 2; 3, 4; 5, 6]
    (copy [#(1, 2); #(3, 4); #(5, 6)]);
  check_pairs [11, 22] (repeat 1 #(11, 22));
  check_pairs [11, 22; 11, 22; 11, 22] (repeat 3 #(11, 22))

let poly_ map f xs =
  let[@tail_mod_cons] rec loop = function
    | [] -> []
    | x :: xs -> f x :: (loop [@tailcall]) xs
  in
  loop xs

module type Filter_map = sig
  type ('a : any) t
  val poly_ filter_map : 'a t -> f:('a -> 'b option) -> 'b t
end

module Filter_opt (M : Filter_map @ static) = struct
  let poly_ filter_opt xs = M.filter_map xs ~f:(fun x -> x)
end

module List = struct
  type ('a : any) t = 'a list

  let poly_ filter_map xs ~f =
    let[@tail_mod_cons] rec loop = function
      | [] -> []
      | x :: xs ->
        match f x with
        | None -> loop xs
        | Some y -> y :: (loop [@tailcall]) xs
    in
    loop xs
end

module Filter = Filter_opt (List)

let () =
  check_pairs [1, 2; 3, 4] (map (fun x -> x) [#(1, 2); #(3, 4)]);
  check_pairs [1, 2; 3, 4]
    (Filter.filter_opt [None; Some #(1, 2); None; Some #(3, 4)]);
  check_pairs [1, 2; 3, 4]
    (map (fun #(a, #(b, #())) -> #(a, b))
       (map (fun x -> x) [#(1, #(2, #())); #(3, #(4, #()))]));
  check_pairs [1, 2; 1, 2]
    (map (fun #() -> #(1, 2)) (map (fun x -> x) [#(); #()]))

let () =
  let calls = ref [] in
  let result = map (fun n -> calls := n :: !calls; #(n, n + 1)) [1; 2; 3] in
  assert (!calls = [3; 2; 1]);
  check_pairs [1, 2; 2, 3; 3, 4] result

type tree = Leaf | Node of #(int * int) * tree * #(int * int)

let[@tail_mod_cons] rec tree n =
  if n = 0 then Leaf
  else Node (#(n, n + 1), tree (n - 1), #(n + 2, n + 3))

let () =
  let rec check n = function
    | Leaf -> assert (n = 0)
    | Node (#(a, b), t, #(c, d)) ->
      assert (a = n && b = n + 1 && c = n + 2 && d = n + 3);
      check (n - 1) t
  in
  check 100 (tree 100)

let () =
  let xs = map (fun n -> Gc.minor (); #(string_of_int n, n)) [1; 2; 3] in
  Gc.full_major ();
  assert (map (fun #(s, n) -> s, n) xs = ["1", 1; "2", 2; "3", 3]);
  let calls = ref [] in
  let exception Stop in
  match map (fun n ->
      calls := n :: !calls;
      if n = 2 then raise Stop;
      #(n, n + 1)) [1; 2; 3] with
  | exception Stop -> assert (!calls = [2; 1])
  | _ -> failwith "expected Stop"

let () =
  let xs = repeat 100_000 #(11, 22) in
  let rec length n = function
    | [] -> n
    | #(a, b) :: xs ->
      assert (a = 11 && b = 22);
      length (n + 1) xs
  in
  assert (length 0 (copy xs) = 100_000)
