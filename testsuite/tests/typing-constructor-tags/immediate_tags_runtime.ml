(* TEST_BELOW
*)

type t =
  | A [@tag 2]
  | B
  | C [@tag 1]

type mixed =
  | I0 [@tag 1]
  | Block of int
  | I1

type signed =
  | Neg [@tag (-1)]
  | Zero
  | High [@tag 4]

type boundary_runtime =
  | BRmin [@tag (-4611686018427387904)]
  | BRmax [@tag 4611686018427387903]

type boundary_fail =
  | BFzero
  | BFmax [@tag 4611686018427387903]

type t_void : void

type void_payload =
  | VP : t_void -> void_payload
      [@tag 7] [@immediate_all_void_constructor]
  | VZ

type sparse_mixed =
  | Sneg [@tag (-2)]
  | Sblock of int
  | Shigh [@tag 5]
  | Szero

type many_sparse =
  | Mneg [@tag (-1)]
  | M00
  | M01
  | M02
  | M03
  | M04
  | M05
  | M06
  | M07
  | M08
  | M09
  | M10
  | M11
  | M12
  | M13
  | M14
  | M15
  | M16
  | M17
  | M18
  | M19
  | M20
  | M21
  | M22
  | M23
  | M24
  | M25
  | M26
  | M27
  | M28
  | M29
  | M30
  | M31
  | M32
  | M33
  | M34
  | M35
  | M36
  | M37
  | M38
  | M39
  | M40
  | Mhigh [@tag 100]

type many_sparse_mixed =
  | MSneg [@tag (-2)]
  | MSblock of int
  | MS00
  | MS01
  | MS02
  | MS03
  | MS04
  | MS05
  | MS06
  | MS07
  | MS08
  | MS09
  | MS10
  | MS11
  | MS12
  | MS13
  | MS14
  | MS15
  | MS16
  | MS17
  | MS18
  | MS19
  | MS20
  | MS21
  | MS22
  | MS23
  | MS24
  | MS25
  | MS26
  | MS27
  | MS28
  | MS29
  | MS30
  | MS31
  | MS32
  | MS33
  | MS34
  | MS35
  | MS36
  | MS37
  | MS38
  | MS39
  | MShigh [@tag 100]

(* Polymorphic [compare]/[hash]/[Hashtbl.hash] observe the runtime tag value,
   not declaration order.  With sparse/negative tags, tag order and declaration
   order no longer coincide, so structural comparison, sorting, [Set]/[Map]
   ordering, and hashing all key off the assigned immediate. *)
type ordered =
  | Oa [@tag 5]
  | Ob [@tag (-3)]
  | Oc

module Ordered_set = Set.Make (struct
  type t = ordered

  let compare = compare
end)

(* GADT with sparse/negative tags on constant constructors dispatches at runtime
   to its declared immediate. *)
type _ gadt =
  | Gi : int gadt [@tag 9]
  | Gb : bool gadt [@tag (-2)]
  | Gu : unit gadt

let int_of_t (x : t) : int = Obj.magic x

let int_of_mixed (x : mixed) : int = Obj.magic x

let int_of_signed (x : signed) : int = Obj.magic x

let int_of_boundary_runtime (x : boundary_runtime) : int = Obj.magic x

let int_of_void_payload (x : void_payload) : int = Obj.magic x

let int_of_sparse_mixed (x : sparse_mixed) : int = Obj.magic x

let print_t x =
  Printf.printf "%s:%d\n"
    (match x with A -> "A" | B -> "B" | C -> "C")
    (int_of_t x)

let print_mixed x =
  match x with
  | I0 -> Printf.printf "I0:%d\n" (int_of_mixed x)
  | I1 -> Printf.printf "I1:%d\n" (int_of_mixed x)
  | Block n -> Printf.printf "Block %d\n" n

let classify_t_default x =
  match x with
  | A -> "A"
  | _ -> "default"

let classify_mixed_default x =
  match x with
  | I0 -> "I0"
  | _ -> "mixed default"

let print_signed x =
  Printf.printf "%s:%d:%s\n"
    (match x with Neg -> "Neg" | Zero -> "Zero" | High -> "High")
    (int_of_signed x)
    (match x with Neg -> "negative" | _ -> "nonnegative")

let print_boundary_runtime x =
  Printf.printf "%s:%d\n"
    (match x with BRmin -> "BRmin" | BRmax -> "BRmax")
    (int_of_boundary_runtime x)

let boundary_fail_nonexhaustive x =
  (match x with BFzero -> "boundary zero") [@warning "-8"]

let print_boundary_fail_nonexhaustive x =
  try Printf.printf "boundary partial:%s\n" (boundary_fail_nonexhaustive x)
  with Match_failure _ -> Printf.printf "boundary partial:Match_failure\n"

let print_void_payload x =
  Printf.printf "%s:%d\n"
    (match x with VP _ -> "VP" | VZ -> "VZ")
    (int_of_void_payload x)

let print_sparse_mixed x =
  match x with
  | Sneg -> Printf.printf "Sneg\n"
  | Szero -> Printf.printf "Szero\n"
  | Shigh -> Printf.printf "Shigh\n"
  | Sblock n -> Printf.printf "Sblock %d\n" n

let classify_sparse_mixed x =
  match x with
  | Sneg -> "sparse negative"
  | Shigh -> "sparse high"
  | _ -> "sparse default"

let sparse_nonexhaustive x =
  (match x with Sneg -> "only Sneg") [@warning "-8"]

let print_sparse_nonexhaustive x =
  try Printf.printf "partial:%s\n" (sparse_nonexhaustive x)
  with Match_failure _ -> Printf.printf "partial:Match_failure\n"

let classify_many_sparse x =
  match x with
  | Mneg -> "many negative"
  | Mhigh -> "many high"
  | _ -> "many default"

let print_many_sparse_mixed x =
  match x with
  | MSneg -> Printf.printf "many mixed negative\n"
  | MShigh -> Printf.printf "many mixed high\n"
  | MSblock n -> Printf.printf "many mixed block %d\n" n
  | _ -> Printf.printf "many mixed default\n"

let name_ordered = function Oa -> "Oa" | Ob -> "Ob" | Oc -> "Oc"

let describe_gadt : type a. a gadt -> string = function
  | Gi -> "Gi"
  | Gb -> "Gb"
  | Gu -> "Gu"

let () =
  List.iter print_t [ A; B; C ];
  List.iter print_mixed [ I0; I1; Block 42 ];
  List.iter
    (fun x -> Printf.printf "%s\n" (classify_t_default x))
    [ A; B; C ];
  List.iter
    (fun x -> Printf.printf "%s\n" (classify_mixed_default x))
    [ I0; I1; Block 42 ];
  List.iter print_signed [ Neg; Zero; High ];
  List.iter print_boundary_runtime [ BRmin; BRmax ];
  List.iter print_boundary_fail_nonexhaustive [ BFzero; BFmax ];
  List.iter print_void_payload [ (Obj.magic 7 : void_payload); VZ ];
  List.iter print_sparse_mixed [ Sneg; Szero; Shigh; Sblock 42 ];
  Printf.printf "Sneg:%d\n" (int_of_sparse_mixed Sneg);
  Printf.printf "Szero:%d\n" (int_of_sparse_mixed Szero);
  Printf.printf "Shigh:%d\n" (int_of_sparse_mixed Shigh);
  List.iter print_sparse_nonexhaustive [ Sneg; Szero; Shigh; Sblock 42 ];
  List.iter
    (fun x -> Printf.printf "%s\n" (classify_sparse_mixed x))
    [ Sneg; Szero; Shigh; Sblock 42 ];
  List.iter
    (fun x -> Printf.printf "%s\n" (classify_many_sparse x))
    [ Mneg; M00; Mhigh ];
  List.iter print_many_sparse_mixed [ MSneg; MS00; MShigh; MSblock 42 ];
  (* compare/hash follow tag value, not declaration order *)
  Printf.printf "compare Oa Ob:%d\n" (compare Oa Ob);
  Printf.printf "compare Ob Oc:%d\n" (compare Ob Oc);
  Printf.printf "compare Oc Oa:%d\n" (compare Oc Oa);
  Printf.printf "eq Oa Oa:%b Oa Ob:%b\n" (Oa = Oa) (Oa = Ob);
  Printf.printf "sort:";
  List.iter
    (fun x -> Printf.printf " %s" (name_ordered x))
    (List.sort compare [ Oa; Ob; Oc ]);
  print_newline ();
  Printf.printf "set:";
  Ordered_set.iter
    (fun x -> Printf.printf " %s" (name_ordered x))
    (Ordered_set.of_list [ Oa; Ob; Oc ]);
  print_newline ();
  Printf.printf "hash Oa=hash 5:%b Oa=hash Ob:%b\n"
    (Hashtbl.hash Oa = Hashtbl.hash 5)
    (Hashtbl.hash Oa = Hashtbl.hash Ob);
  (* GADT sparse/negative tags dispatch at runtime *)
  Printf.printf "gadt:%s:%d %s:%d %s:%d\n" (describe_gadt Gi)
    (Obj.magic Gi : int) (describe_gadt Gb)
    (Obj.magic Gb : int) (describe_gadt Gu) (Obj.magic Gu : int)

(* TEST
 flags = "-extension layouts_alpha";
 reference = "${test_source_directory}/immediate_tags_runtime.reference";
 {
   bytecode;
 }{
   native;
 }
*)
