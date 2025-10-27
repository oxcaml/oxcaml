(* TEST
 expect;
*)

let empty_collections () =
  let mtl1 : int ref list = [] in
  let mtl2 : int ref list = (let x = [] in x) in
  let mtia : int ref iarray = [: :] in
  let _ @ portable = fun () -> ((mtl1, mtl2, mtia) : _ @ uncontended) in
  ()
[%%expect {|
val empty_collections : unit -> unit = <fun>
|}]

let ref_fails () =
  let x : int ref = ref 5222 in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect {|
Line 3, characters 32-33:
3 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let array_fails () =
  let x : int array = [| |] in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect {|
Line 3, characters 32-33:
3 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let info_propagates () =
  let x : int ref = (let x = ref 5222 in x) in
  let y = x in
  let _ @ portable = fun () -> (y : _ @ uncontended) in
  ()
[%%expect {|
Line 4, characters 32-33:
4 |   let _ @ portable = fun () -> (y : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let nonempty_list () =
  let x : int ref list = [ ref 10801 ] in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect {|
Line 3, characters 32-33:
3 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let nonempty_iarray () =
  let x : int ref iarray = [: ref 10801 :] in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect {|
Line 3, characters 32-33:
3 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(* this should NOT typecheck, since in general x does not cross contention *)
let func (x : int ref option) =
  let sg = [ x ] in
  let _ @ portable = fun () -> (sg : _ @ uncontended) in
  ()
[%%expect {|
Line 3, characters 32-34:
3 |   let _ @ portable = fun () -> (sg : _ @ uncontended) in
                                    ^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let tuples_work () =
  let t1 : int ref list * int ref list = ([], []) in
  let t2 : #(int ref iarray * int ref iarray) = #([: :], [: :]) in
  let _ @ portable = fun () ->
    let _ = (t1 : _ @ uncontended) in (t2 : _ @ uncontended)
  in
  ()
[%%expect {|
val tuples_work : unit -> unit = <fun>
|}]

(* CR modes: once [tuple_modes] tracks crossing information, the error
   should move from [f] to [g] *)
let tuple_failure () =
  let (x, y) : int ref list * int ref list = ([], [ ref 1 ]) in
  let f @ portable = fun () -> (x : _ @ uncontended) in
  let g @ portable = fun () -> (y : _ @ uncontended) in
  ()
[%%expect{|
Line 3, characters 32-33:
3 |   let f @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]


type t1 = #{ a : int ref list; b : int ref list }

let unboxed_record_works () =
  let r = #{ a = []; b = [] } in
  let _ @ portable = fun () -> (r : _ @ uncontended) in
  ()
[%%expect{|
type t1 = #{ a : int ref list; b : int ref list; }
val unboxed_record_works : unit -> unit = <fun>
|}]

type t2 = C of int ref list * int ref list

let constructor_and_variants_work () =
  let x = (C ([], []) : t2) in
  (* when a coercion is added to this, it raises an error about y being
     nonportable, which is confusing to me... *)
  let y : [`A of int ref list] = `A [] in
  let _ @ portable = fun () -> ((x, y) : _ @ uncontended) in
  ()
[%%expect{|
type t2 = C of int ref list * int ref list
val constructor_and_variants_work : unit -> unit = <fun>
|}]

(* CR modes: fix this case, which requires a more careful treatment of
   patterns. currently skipped due to subtelty with modalities *)
let projection_doesnt_work () =
  let x = C ([], []) in
  match x with
  | C (a, b) -> let _ @ portable = fun () -> (a : _ @ uncontended) in
  ()
[%%expect{|
Line 4, characters 46-47:
4 |   | C (a, b) -> let _ @ portable = fun () -> (a : _ @ uncontended) in
                                                  ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let ite b =
  let x : int ref list = if b then [] else assert false in
  let y : int ref list = if b then [ ref 8903 ] else [] in
  (* x should be ok to use but y shouldn't be *)
  let _ @ portable = fun () -> ((x, y) : _ @ uncontended) in
  ()
[%%expect{|
Line 5, characters 36-37:
5 |   let _ @ portable = fun () -> ((x, y) : _ @ uncontended) in
                                        ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let seq_statements () =
  let x : int ref list =
    for i = 6 to 7 do () done;
    while false do () done;
    []
  in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect{|
val seq_statements : unit -> unit = <fun>
|}]

let nest_module_let () =
  let x : (int ref list * int ref list) =
    let exception E in
    let module M = struct
      let mt : int ref list = []
    end in
    let open M in
      (mt, M.mt)
  in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect{|
val nest_module_let : unit -> unit = <fun>
|}]

let nested_module_fails () =
  let x : int ref list =
    let module M = struct
      let lst : int ref list = [ ref 42 ]
    end
    in M.lst
  in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()

[%%expect{|
Line 8, characters 32-33:
8 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let try_match o =
  let mt1 : int ref list = try [] with _ -> [] in
  let mt2 : int ref list =
    match o with
    | Some _ -> []
    | None -> []
  in
  let ne : int ref list =
    match o with
    | Some l -> l
    | None -> []
  in
  (* mt1 and mt2 should be ok to use but ne shouldn't be *)
  let _ @ portable = fun () -> ((mt1, mt2, ne) : _ @ uncontended) in
  ()
[%%expect{|
Line 14, characters 43-45:
14 |   let _ @ portable = fun () -> ((mt1, mt2, ne) : _ @ uncontended) in
                                                ^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let stack_test () =
  let x = (stack_ C ([], [])) in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect{|
val stack_test : unit -> unit = <fun>
|}]

let let_mutable () =
  let mutable x : int ref list = [] in
  (* ensure that we don't give up too early, also sequencing *)
  let mt : int ref list = x <- [ ref 42 ]; [] in
  ignore x;
  (* if we tried to use in the closure -> error (mutable capture) *)
  let _ @ portable = fun () -> (mt : _ @ uncontended) in
  ()
[%%expect{|
val let_mutable : unit -> unit = <fun>
|}]

type boxed_rec = { a : int ref option; b : int }
type boxed_rec_mut = { mutable a : int ref option; b : int }

let immut_records () =
  let imm : boxed_rec = { a = None; b = 6 } in
  let mut : boxed_rec_mut = { a = None; b = 8 } in
  let imm2 = mut.a <- Some (ref 42); { imm with b = 7 } in
  let _ @ portable = fun () -> ((imm, imm2, mut) : _ @ uncontended) in
  ()
[%%expect{|
type boxed_rec = { a : int ref option; b : int; }
type boxed_rec_mut = { mutable a : int ref option; b : int; }
Line 8, characters 44-47:
8 |   let _ @ portable = fun () -> ((imm, imm2, mut) : _ @ uncontended) in
                                                ^^^
Error: This value is "contended" but is expected to be "uncontended".
|}]

let record_update_bad () =
  let r : boxed_rec = { a = None; b = 6 } in
  let x = { r with a = Some (ref 31998) } in
  let _ @ portable = fun () -> (x : _ @ uncontended) in
  ()
[%%expect{|
Line 4, characters 32-33:
4 |   let _ @ portable = fun () -> (x : _ @ uncontended) in
                                    ^
Error: This value is "contended" but is expected to be "uncontended".
|}]
