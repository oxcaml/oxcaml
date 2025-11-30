(* Provides functor [Make_boxed] that constructs a module with a
   similar interface as stdlib [Array] specialized to an unboxed type.
   This module can then be passed to functor [Test] to check for correctness. *)

module type Element_intf = sig
  type t
  val of_int : int -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg : t -> t
  val max_val : t
  val min_val : t
  val rand : t -> t
  val compare : t -> t -> int
  val print : t -> unit
end

module type S = sig
  type t
  type element_t
  val length : t -> int
  val get : t -> int -> element_t
  val make : int -> element_t -> t
  val init : int -> (int -> element_t) -> t
  val make_matrix : int -> int -> element_t -> t array
  val append : t -> t -> t
  val concat : t list -> t
  val sub : t -> int -> int -> t
  val copy : t -> t
  val iter : (element_t -> unit) -> t -> unit
  val iteri : (int -> element_t -> unit) -> t -> unit
  val map : (element_t -> element_t) -> t -> t
  val mapi : (int -> element_t -> element_t) -> t -> t
  val fold_left : ('a -> element_t -> 'a) -> 'a -> t -> 'a
  val fold_right : (element_t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter2 : (element_t -> element_t -> unit) -> t -> t -> unit
  val map2 : (element_t -> element_t -> element_t) -> t -> t -> t
  val for_all : (element_t -> bool) -> t -> bool
  val for_all2 : (element_t -> element_t -> bool) -> t -> t -> bool
  val exists : (element_t -> bool) -> t -> bool
  val exists2 : (element_t -> element_t -> bool) -> t -> t -> bool
  val mem : element_t -> t -> bool
  val find_index : (element_t-> bool) -> t -> int option
  val find_map : (element_t -> 'a option) -> t -> 'a option
  val find_mapi : (int -> element_t -> 'a option) -> t -> 'a option
  val sort : (element_t -> element_t -> int) -> t -> t
  val unsafe_get : t -> int -> element_t
  val equal : t -> t -> bool
  (* From Sys, rather than Float.Array *)
  val max_length : int

  module I : Element_intf with type t = element_t
end

module Make_boxed (Arg : sig
  module M : Gen_u_iarray.S
  module I : Element_intf
  module E : sig
    val to_boxed : M.element_arg -> I.t
    val of_boxed : I.t -> M.element_arg
  end
end) : S with type t = Arg.M.t
         and type element_t = Arg.I.t = struct
  include Arg.M
  include Arg.E

  module I = Arg.I

  type element_t = I.t

  let empty () = make 0 (of_boxed (I.of_int 0))

  let get t idx = to_boxed (get t idx)

  let make l f = make l (of_boxed f)
  let init l f = init l (fun i -> of_boxed (f i))
  let make_matrix sx sy init = make_matrix sx sy (of_boxed init)
  let iter f t = iter (fun v -> f (to_boxed v)) t
  let iteri f t = iteri (fun i v -> f i (to_boxed v)) t
  let map f t = map (fun v -> of_boxed (f (to_boxed v))) t
  let mapi f t = mapi (fun i v -> of_boxed (f i (to_boxed v))) t
  let fold_left f acc t = fold_left (fun acc v -> f acc (to_boxed v)) acc t
  let fold_right f t acc = fold_right (fun v acc -> f (to_boxed v) acc) t acc

  let iter2 f a b = iter2 (fun v1 v2 -> f (to_boxed v1) (to_boxed v2)) a b
  let map2 f a b = map2 (fun v1 v2 -> of_boxed (f (to_boxed v1) (to_boxed v2))) a b
  let for_all f t = for_all (fun v -> f (to_boxed v)) t
  let exists f t = exists (fun v -> f (to_boxed v)) t
  let mem v t = mem (of_boxed v) t

  let find_index f t = find_index (fun v -> f (to_boxed v)) t
  let find_map f t = find_map (fun v -> f (to_boxed v)) t
  let find_mapi f t = find_mapi (fun i v -> f i (to_boxed v)) t

  let sort cmp t = sort (fun x y -> cmp (to_boxed x) (to_boxed y)) t

  let unsafe_get t idx = to_boxed (unsafe_get t idx)
  let equal = for_all2 (fun x y -> I.compare (to_boxed x) (to_boxed y) = 0)
  let for_all2 f t1 t2 = for_all2 (fun a b -> f (to_boxed a) (to_boxed b)) t1 t2
  let exists2 f t1 t2 = exists2 (fun a b -> f (to_boxed a) (to_boxed b)) t1 t2
end

module Test (A : S) : sig end = struct
  let module I = A.I in
  let assert_eq x y = assert (I.compare x y = 0) in

  (* auxiliary functions *)
  let rec check_i_upto a i =
    if i >= 0 then begin
      assert_eq (A.get a i) (I.of_int i);
      check_i_upto a (i - 1);
    end
  in

  let check_i a = check_i_upto a (A.length a - 1) in

  let check_inval f arg =
    match f arg with
    | _ -> Format.printf "check_inval failed"; assert false
    | exception (Invalid_argument _) -> ()
    | exception _ -> assert false
  in

  (* [make] [get] *)
  let a = A.make 1000 (I.of_int 1) in
  let rec loop i =
    if i >= 0 then begin
      assert_eq (A.get a i) (I.of_int 1);
      loop (i - 1);
    end
  in loop 999;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1000);
  check_inval (fun i -> A.make i (I.of_int 1)) (-1);
  check_inval (fun i -> A.make i (I.of_int 1)) (A.max_length + 1);

  let a = A.make 1001 (I.of_int 1) in
  let rec loop i =
    if i >= 0 then begin
      assert_eq (A.get a i) (I.of_int 1);
      loop (i - 1);
    end
  in loop 1000;
  check_inval (A.get a) (-1);
  check_inval (A.get a) (1001);

  (* [length] *)
  let test_length l = assert (l = (A.length (A.make l (I.of_int 1)))) in
  test_length 0;
  test_length 1;
  test_length 10;
  test_length 25;
  test_length 255;
  test_length 256;
  test_length 1000;
  test_length 1001;

  (* [init] *)
  let a = A.init 1000 I.of_int in
  check_i a;
  let a = A.init 1001 I.of_int in
  check_i a;
  check_inval (fun i -> A.init i I.of_int) (-1);
  check_inval (fun i -> A.init i I.of_int) (A.max_length + 1);

  (* [append] *)
  let check m n =
    let a = A.init m I.of_int in
    let b = A.init n (fun x -> I.of_int (x + m)) in
    let c = A.append a b in
    assert (A.length c = (m + n));
    check_i c;
  in
  check 0 0;
  check 0 100;
  check 1 100;
  check 100 0;
  check 100 1;
  check 100 100;
  check 1000 1000;
  check 1000 1001;
  check 1001 1000;
  check 1001 1001;
  (* check_inval omitted *)

  (* [concat] *)
  let check l =
    let f (len, acc) n =
      (len + n, A.init n (fun i -> I.of_int (len + i)) :: acc)
    in
    let (total, ll) = List.fold_left f (0, []) l in
    let b = A.concat (List.rev ll) in
    assert (A.length b = total);
    check_i b;
  in
  check [0; 0; 0];
  check [1; 10; 100];
  check [10; 0];
  check [0];
  check [1000; 1000; 1000];
  check [];
  check [1001; 1000; 1000];
  check [1000; 1001; 1000];
  check [1000; 1000; 1001];
  check [1001; 1001; 1001];
  (* check_inval omitted *)

  (* [sub] *)
  let a = A.init 1000 (fun i -> I.of_int (i - 100)) in
  let b = A.sub a 100 200 in
  check_i b;
  assert (A.length b = 200);
  let b = A.sub a 1000 0 in
  check_i b;
  assert  (A.length b = 0);
  check_inval (A.sub a (-1)) 0;
  check_inval (A.sub a 0) (-1);
  check_inval (A.sub a 0) 1001;
  check_inval (A.sub a 1000) 1;

  let a = A.init 1001 (fun i -> I.of_int (i - 101)) in
  let b = A.sub a 101 199 in
  check_i b;
  assert (A.length b = 199);
  let b = A.sub a 1001 0 in
  check_i (A.sub a 1001 0);
  assert  (A.length b = 0);
  check_inval (A.sub a (-1)) 0;
  check_inval (A.sub a 0) (-1);
  check_inval (A.sub a 0) 1002;
  check_inval (A.sub a 1001) 1;

  (* [copy] *)
  let check len =
    let a = A.init len I.of_int in
    let b = A.copy a in
    check_i b;
    assert (A.length b = len);
  in
  check 0;
  check 1;
  check 128;
  check 1023;

  (* [iter] *)
  let a = A.init 300 (I.of_int) in
  let r = ref (I.of_int 0) in
  A.iter (fun x -> assert_eq x !r; r := I.add x (I.of_int 1)) a;
  A.iter (fun _ -> assert false) (A.make 0 (I.of_int 0));
  assert_eq !r (I.of_int 300);

  let a = A.init 301 (I.of_int) in
  let r = ref (I.of_int 0) in
  A.iter (fun x -> assert_eq x !r; r := I.add x (I.of_int 1)) a;
  assert_eq !r (I.of_int 301);

  (* [iteri] *)
  let a = A.init 300 I.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert_eq x (I.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.make 0 (I.of_int 0));
  assert (!r = 300);

  let a = A.init 301 I.of_int in
  let r = ref 0 in
  let f i x =
    assert (i = !r);
    assert_eq x (I.of_int i);
    r := i + 1
  in
  A.iteri f a;
  A.iteri (fun _ _ -> assert false) (A.make 0 (I.of_int 0));
  assert (!r = 301);

  (* [map], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x (!r);
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.map f a in
  check_i (A.sub b 1 499);

  let a = A.init 501 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x (!r);
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.map f a in
  check_i (A.sub b 1 500);

  (* [mapi], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let r = ref (I.of_int 0) in
  let f i x =
    assert_eq x (I.of_int i);
    assert_eq x (!r);
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 499);

  let a = A.init 501 I.of_int in
  let r = ref (I.of_int 0) in
  let f i x =
    assert_eq x (I.of_int i);
    assert_eq x !r;
    r := I.add !r (I.of_int 1);
    I.sub x (I.of_int 1)
  in
  let b = A.mapi f a in
  check_i (A.sub b 1 500);

  (* [fold_left], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let f acc x =
    assert_eq acc x;
    I.add x (I.of_int 1)
  in
  let acc = A.fold_left f (I.of_int 0) a in
  assert_eq acc (I.of_int 500);

  let a = A.init 501 I.of_int in
  let acc = A.fold_left f (I.of_int 0) a in
  assert_eq acc (I.of_int 501);

  (* [fold_right], test result and order of evaluation *)
  let a = A.init 500 I.of_int in
  let f x acc =
    assert_eq x (I.sub acc (I.of_int 1));
    x
  in
  let acc = A.fold_right f a (I.of_int 500) in
  assert_eq acc (I.of_int 0);

  let a = A.init 501 I.of_int in
  let acc = A.fold_right f a (I.of_int 501) in
  assert_eq acc (I.of_int 0);

  (* [iter2], test result and order of evaluation *)
  let a = A.init 123 I.of_int in
  let b = A.init 123 I.of_int in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y !r;
    r := I.add!r (I.of_int 1);
  in
  A.iter2 f a b;
  let c = A.make 456 (I.of_int 0) in
  check_inval (A.iter2 (fun _ _ -> assert false) a) c;
  check_inval (A.iter2 (fun _ _ -> assert false) c) a;

  let a = A.init 124 I.of_int in
  let b = A.init 124 I.of_int in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y !r;
    r := I.add !r (I.of_int 1);
  in
  A.iter2 f a b;

  (* [map2], test result and order of evaluation *)
  let a = A.init 456 I.of_int in
  let b = A.init 456 (fun i -> I.(mul (of_int i) (I.of_int 2))) in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y (I.mul !r (I.of_int 2));
    r := I.add !r (I.of_int 1);
    I.(neg (sub x y))
  in
  let c = A.map2 f a b in
  check_i c;
  let d = A.make 455 (I.of_int 0) in
  check_inval (A.map2 (fun _ _ -> assert false) a) d;
  check_inval (A.map2 (fun _ _ -> assert false) d) a;

  let a = A.init 457 I.of_int in
  let b = A.init 457 (fun i -> I.(mul (of_int i) (I.of_int 2))) in
  let r = ref (I.of_int 0) in
  let f x y =
    assert_eq x !r;
    assert_eq y (I.mul !r (I.of_int 2));
    r := I.add !r (I.of_int 1);
    I.(neg (sub x y))
  in
  let c = A.map2 f a b in
  check_i c;

  (* [for_all], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    true
  in
  assert (A.for_all f a);
  let f x = assert_eq x (I.of_int 0); false in
  assert (not (A.for_all f a));

  let a = A.init 778 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    true
  in
  assert (A.for_all f a);
  let f x = assert_eq x (I.of_int 0); false in
  assert (not (A.for_all f a));

  (* [exists], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (not (A.exists f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (A.exists f a);

  let a = A.init 778 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (not (A.exists f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (A.exists f a);

  (* [mem] *)
  let a = A.init 777 I.of_int in
  assert (A.mem (I.of_int 0) a);
  assert (A.mem (I.of_int 776) a);
  assert (not (A.mem ((I.of_int (-1))) a));
  assert (not (A.mem (I.of_int 777) a));

  let a = A.init 778 I.of_int in
  assert (A.mem (I.of_int 0) a);
  assert (A.mem (I.of_int 777) a);
  assert (not (A.mem ((I.of_int (-1))) a));
  assert (not (A.mem (I.of_int 778) a));

  (* [find_opt], test result and order of evaluation *)
  (* let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (Option.is_none (A.find_opt f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (Option.is_some (A.find_opt f a)); *)

  (* [find_index], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    false
  in
  assert (Option.is_none (A.find_index f a));
  let f x = assert_eq x (I.of_int 0); true in
  assert (Option.get (A.find_index f a) = 0);

  (* [find_map], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let f x =
    assert_eq x !r;
    r := I.add x (I.of_int 1);
    None
  in
  assert (Option.is_none (A.find_map f a));
  let f x = assert_eq x (I.of_int 0); Some "abc" in
  assert (Option.get (A.find_map f a) = "abc");

  (* [find_mapi], test result and order of evaluation *)
  let a = A.init 777 I.of_int in
  let r = ref (I.of_int 0) in
  let r_i = ref 0 in
  let f i x =
    assert (i = !r_i);
    assert_eq x !r;
    r_i := !r_i + 1;
    r := I.add x (I.of_int 1);
    None
  in
  assert (Option.is_none (A.find_mapi f a));
  let f i x =
    assert (i = 0);
    assert_eq x (I.of_int 0);
    Some "abc"
  in
  assert (Option.get (A.find_mapi f a) = "abc");

  (* [sort] *)
  let a = A.init 1000 (fun i -> I.of_int (999 - i)) in
  let b = A.sort I.compare a in
  check_i b;
  (* check that the original array is unchanged *)
  assert_eq (A.get a 0) (I.of_int 999);

  let a = A.init 1001 (fun i -> I.of_int (1000 - i)) in
  let b = A.sort I.compare a in
  check_i b;
  (* check that the original array is unchanged *)
  assert_eq (A.get a 0) (I.of_int 1000);

end
