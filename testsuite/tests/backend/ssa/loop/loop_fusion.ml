(* TEST
 flags = "-O3 -ssa-fuse-loops";
 native;
*)

(* Runtime-behaviour tests for SSA loop fusion (list deforestation of adjacent
   reversing list-map loops). Each fusable pipeline is checked against a
   semantically identical reference in which [Sys.opaque_identity] barriers
   between the stages stop the loops being recognised as an adjacent chain, so
   the reference is never fused. Agreement across many inputs -- and identical
   side-effect ordering for the effectful pipeline -- exercises the
   transformation without hard-coding any results. *)

let[@inline never] barrier x = Sys.opaque_identity x

let[@inline] f x = x, x + 1

let[@inline] g (a, b) = (a * b) + 2

let[@inline] h x = x lxor 5

let[@inline] k x = x + 1000

(* Fusable pipelines: chains of adjacent [rev_map]/[rev] loops. *)

(* Odd 3-loop chain whose result is returned. *)
let p3 xs = xs |> List.rev_map f |> List.rev_map g |> List.rev

(* Odd 5-loop chain. *)
let p5 xs =
  xs |> List.rev_map f |> List.rev_map g |> List.rev_map h |> List.rev_map k
  |> List.rev

(* Even 4-loop chain: only the odd prefix fuses, the trailing [rev] remains. *)
let even4 xs =
  xs |> List.rev_map f |> List.rev_map g |> List.rev_map h |> List.rev

(* Odd chain whose result is consumed further (not returned). *)
let used_further xs =
  List.fold_left ( + ) 0 (xs |> List.rev_map f |> List.rev_map g |> List.rev)

(* References: same computation, barriers between stages defeat chaining. *)

let r3 xs =
  barrier (List.rev (barrier (List.rev_map g (barrier (List.rev_map f xs)))))

let r5 xs =
  barrier
    (List.rev
       (barrier
          (List.rev_map k
             (barrier
                (List.rev_map h
                   (barrier
                      (List.rev_map g (barrier (List.rev_map f xs)))))))))

let r4 xs =
  barrier
    (List.rev
       (barrier
          (List.rev_map h
             (barrier (List.rev_map g (barrier (List.rev_map f xs)))))))

let ru xs = List.fold_left ( + ) 0 (r3 xs)

let fail = ref false

let check name eq a b =
  if not (eq a b)
  then begin
    fail := true;
    Printf.printf "%s: MISMATCH\n" name
  end

let () =
  Random.init 123;
  for _ = 1 to 500 do
    let n = Random.int 14 in
    let xs = List.init n (fun _ -> Random.int 100 - 50) in
    check "p3" ( = ) (p3 xs) (r3 xs);
    check "p5" ( = ) (p5 xs) (r5 xs);
    check "even4" ( = ) (even4 xs) (r4 xs);
    check "used_further" ( = ) (used_further xs) (ru xs)
  done;
  Printf.printf "checked 500 inputs, ok=%b\n" (not !fail);
  let show l = "[" ^ String.concat ";" (List.map string_of_int l) ^ "]" in
  List.iter
    (fun xs -> Printf.printf "p3 %s = %s\n" (show xs) (show (p3 xs)))
    [ []; [7]; [1; 2; 3] ];
  (* An effectful first stage must not be fused, and its effects must happen in
     input order regardless. *)
  let buf = Buffer.create 16 in
  let eff xs =
    xs
    |> List.rev_map (fun i ->
           Buffer.add_char buf (Char.chr (Char.code 'a' + i));
           i + 1)
    |> List.rev_map (fun y -> y * 2)
    |> List.rev
  in
  let ys = eff [ 0; 1; 2; 3 ] in
  Printf.printf "eff = %s effects = %s\n" (show ys) (Buffer.contents buf)
