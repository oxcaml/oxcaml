let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

module type OrderedType = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type SetType = sig
  type elem

  type t

  val empty : t

  val singleton : elem -> t

  val add : elem -> t -> t

  val mem : elem -> t -> bool

  val size : t -> int

  val fold : (elem -> 'a -> 'a) -> t -> 'a -> 'a
end

module MakeSet (Ord : OrderedType) : SetType with type elem = Ord.t = struct
  type elem = Ord.t

  type node =
    { value : elem;
      left : tree;
      right : tree;
      height : int
    }

  and tree =
    | Empty
    | Node of node

  type t =
    { root : tree;
      count : int
    }

  let empty = { root = Empty; count = 0 }

  let height = function Empty -> 0 | Node { height; _ } -> height

  let make_node value left right =
    let h = 1 + max (height left) (height right) in
    { value; left; right; height = h }

  let singleton x = { root = Node (make_node x Empty Empty); count = 1 }

  let rec add x tree =
    match tree with
    | Empty -> Node (make_node x Empty Empty)
    | Node { value; left; right; _ } ->
      let c = Ord.compare x value in
      if c = 0
      then tree
      else if c < 0
      then Node (make_node value (add x left) right)
      else Node (make_node value left (add x right))

  let add x t =
    let new_root = add x t.root in
    if new_root == t.root then t else { root = new_root; count = t.count + 1 }

  let rec mem x = function
    | Empty -> false
    | Node { value; left; right; _ } ->
      let c = Ord.compare x value in
      if c = 0 then true else if c < 0 then mem x left else mem x right

  let mem x t = mem x t.root

  let size t = t.count

  let rec fold f tree acc =
    match tree with
    | Empty -> acc
    | Node { value; left; right; _ } ->
      let acc' = fold f left acc in
      let acc'' = f value acc' in
      fold f right acc''

  let fold f t acc = fold f t.root acc
end

module type SimpleStore = sig
  type key

  type t

  val empty : t

  val add : key -> int -> t -> t

  val find : key -> t -> int option

  val size : t -> int
end

module MakeSimpleStore (K : OrderedType) : SimpleStore with type key = K.t =
struct
  type key = K.t

  type entry =
    { key : key;
      value : int
    }

  type t = entry list

  let empty = []

  let add key value store = { key; value } :: store

  let rec find key = function
    | [] -> None
    | { key = k; value } :: rest ->
      if K.compare key k = 0 then Some value else find key rest

  let size store = List.length store
end

module IntOrder = struct
  type t = int

  let compare = Int.compare

  let to_string = Int.to_string
end

module StringOrder = struct
  type t = string

  let compare = String.compare

  let to_string x = x
end

module FloatOrder = struct
  type t = float

  let compare = Float.compare

  let to_string = Float.to_string
end

module IntSet = MakeSet (IntOrder)
module StringSet = MakeSet (StringOrder)
module FloatSet = MakeSet (FloatOrder)
module IntStore = MakeSimpleStore (IntOrder)
module StringStore = MakeSimpleStore (StringOrder)

(* CR sspies: Functor-generated data structures show truncated internal data in
   DWARF output. Complex nested structures from functors display as
   [[[...]@<ADDRESS>], count] instead of showing the actual tree nodes, record
   fields, and internal structure. This makes debugging functor-generated types
   difficult as users cannot inspect the internal state of sets, trees, and
   other nested data structures created through module instantiation. *)
let[@inline never] [@local never] f_int_set (x : IntSet.t) = x

let int_set1 = IntSet.empty

let _ = f_int_set int_set1

let int_set2 = IntSet.singleton 42

let _ = f_int_set int_set2

let int_set3 = IntSet.add 10 (IntSet.add 20 (IntSet.add 30 IntSet.empty))

let _ = f_int_set int_set3

let[@inline never] [@local never] f_string_set (x : StringSet.t) = x

let string_set1 = StringSet.empty

let _ = f_string_set string_set1

let string_set2 = StringSet.singleton "hello"

let _ = f_string_set string_set2

let string_set3 = StringSet.add "world" (StringSet.add "foo" StringSet.empty)

let _ = f_string_set string_set3

let[@inline never] [@local never] f_float_set (x : FloatSet.t) = x

let float_set1 = FloatSet.singleton 3.14

let _ = f_float_set float_set1

let float_set2 = FloatSet.add 2.71 (FloatSet.add 1.41 FloatSet.empty)

let _ = f_float_set float_set2

let[@inline never] [@local never] f_int_store (x : IntStore.t) = x

let int_store1 = IntStore.empty

let _ = f_int_store int_store1

let int_store2 = IntStore.add 1 100 IntStore.empty

let _ = f_int_store int_store2

let int_store3 = IntStore.add 2 200 (IntStore.add 1 100 IntStore.empty)

let _ = f_int_store int_store3

let[@inline never] [@local never] f_string_store (x : StringStore.t) = x

let string_store1 = StringStore.empty

let _ = f_string_store string_store1

let string_store2 = StringStore.add "key1" 100 StringStore.empty

let _ = f_string_store string_store2

let string_store3 =
  StringStore.add "key2" 200 (StringStore.add "key1" 100 StringStore.empty)

let _ = f_string_store string_store3

let[@inline never] [@local never] f_set_operations (set : IntSet.t) =
  let size = IntSet.size set in
  let has_10 = IntSet.mem 10 set in
  let sum = IntSet.fold ( + ) set 0 in
  size, has_10, sum

let _ = f_set_operations int_set3

let[@inline never] [@local never] f_store_operations (store : IntStore.t) =
  let size = IntStore.size store in
  let lookup_result = IntStore.find 1 store in
  size, lookup_result

let _ = f_store_operations int_store3

module type ComputeType = sig
  type input

  type result =
    { value : input;
      computed : float;
      status : [`Success | `Error of string]
    }

  val compute : input -> result
end

module MakeCompute (Base : OrderedType) : ComputeType with type input = Base.t =
struct
  type input = Base.t

  type result =
    { value : input;
      computed : float;
      status : [`Success | `Error of string]
    }

  let compute input =
    let str_repr = Base.to_string input in
    let len = String.length str_repr in
    if len > 0
    then
      { value = input; computed = Float.of_int len *. 3.14; status = `Success }
    else
      { value = input; computed = 0.0; status = `Error "empty representation" }
end

module IntCompute = MakeCompute (IntOrder)
module StringCompute = MakeCompute (StringOrder)

let[@inline never] [@local never] f_int_compute_result (x : IntCompute.result) =
  x

let int_result1 = IntCompute.compute 42

let _ = f_int_compute_result int_result1

let int_result2 = IntCompute.compute 0

let _ = f_int_compute_result int_result2

let[@inline never] [@local never] f_string_compute_result
    (x : StringCompute.result) =
  x

let string_result1 = StringCompute.compute "test"

let _ = f_string_compute_result string_result1

let string_result2 = StringCompute.compute ""

let _ = f_string_compute_result string_result2

module type AdvancedFunctor = functor (A : OrderedType) (B : OrderedType) -> sig
  type pair_data =
    { first : A.t;
      second : B.t;
      combined : string
    }

  type comparison_result =
    | Less
    | Equal
    | Greater
    | Incomparable

  val make_pair : A.t -> B.t -> pair_data

  val compare_pairs : pair_data -> pair_data -> comparison_result
end

module MakeAdvanced : AdvancedFunctor =
functor
  (A : OrderedType)
  (B : OrderedType)
  ->
  struct
    type pair_data =
      { first : A.t;
        second : B.t;
        combined : string
      }

    type comparison_result =
      | Less
      | Equal
      | Greater
      | Incomparable

    let make_pair a b =
      { first = a; second = b; combined = A.to_string a ^ "+" ^ B.to_string b }

    let compare_pairs p1 p2 =
      let c1 = A.compare p1.first p2.first in
      if c1 <> 0
      then if c1 < 0 then Less else Greater
      else
        let c2 = B.compare p1.second p2.second in
        if c2 < 0 then Less else if c2 > 0 then Greater else Equal
  end

module IntStringAdvanced = MakeAdvanced (IntOrder) (StringOrder)
module StringFloatAdvanced = MakeAdvanced (StringOrder) (FloatOrder)

let[@inline never] [@local never] f_int_string_pair
    (x : IntStringAdvanced.pair_data) =
  x

let pair1 = IntStringAdvanced.make_pair 42 "hello"

let _ = f_int_string_pair pair1

let pair2 = IntStringAdvanced.make_pair 0 "world"

let _ = f_int_string_pair pair2

let[@inline never] [@local never] f_string_float_pair
    (x : StringFloatAdvanced.pair_data) =
  x

let pair3 = StringFloatAdvanced.make_pair "test" 3.14

let _ = f_string_float_pair pair3

let[@inline never] [@local never] f_comparison_result
    (x : IntStringAdvanced.comparison_result) =
  x

let comparison1 = IntStringAdvanced.compare_pairs pair1 pair2

let _ = f_comparison_result comparison1

let comparison2 = IntStringAdvanced.compare_pairs pair1 pair1

let _ = f_comparison_result comparison2
