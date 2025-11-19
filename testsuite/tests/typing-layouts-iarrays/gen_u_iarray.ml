(* Provides a functor [Make] to build modules similar to stdlib [Array]
   for unboxed iarrays. To make this work, unboxed values are boxed in
   closures with type [element_arg] to be passed between functions. *)

module type S0 = sig
  (* An alias for the type of arrays. *)
  type element_t : any mod separable
  type ('a : any mod separable) array_t
  (* Reason why we need [element_arg] is not to be able to define
     modules that implement [S0]. We can do that without it.

     Instead, we need this to be able to use the methods below when
     [S0] is the type of a functor argument. In which case, the
     function argument/return types need to be representable. *)
  type element_arg = unit -> element_t
  type t = element_t array_t

  (* Mutable array type for construction *)
  type mutable_t

  (* Array operations *)

  val length : t -> int
  val get: t -> int -> element_arg
  val unsafe_get: t -> int -> element_arg
  
  (* Construction primitives *)
  val unsafe_create_mutable : int -> mutable_t
  val unsafe_set_mutable : mutable_t -> int -> element_arg -> unit
  val freeze : mutable_t -> t
  
  (* Blit from iarray to mutable array *)
  val blit_to_mutable : t -> int -> mutable_t -> int -> int -> unit

  (* Get from mutable array *)
  val unsafe_get_mutable : mutable_t -> int -> element_arg

  val empty : unit -> t
  val compare_element: element_arg -> element_arg -> int
  val max_length : int
end


module type S = sig
  include S0

  (* methods below are copied from [array.mli] but adapted for immutability *)
  val make : int -> element_arg -> t

  val init : int -> (int -> element_arg) -> t
  val make_matrix : int -> int -> element_arg -> t array

  val append : t -> t -> t
  val concat : t list -> t
  val sub : t -> int -> int -> t
  val copy : t -> t

  (* Iterators *)

  val iter : (element_arg -> unit) -> t -> unit
  val iteri : (int -> element_arg -> unit) -> t -> unit
  val map : (element_arg -> element_arg) -> t -> t
  val mapi : (int -> element_arg -> element_arg) -> t -> t
  val fold_left : ('a -> element_arg -> 'a) -> 'a -> t -> 'a

  val fold_right : (element_arg -> 'a -> 'a) -> t -> 'a -> 'a

  (** {1 Iterators on two arrays} *)


  val iter2 : (element_arg -> element_arg -> unit) -> t -> t -> unit
  val map2 : (element_arg -> element_arg -> element_arg) -> t -> t -> t


  (** {1 Array scanning} *)

  val for_all : (element_arg -> bool) -> t -> bool

  val exists : (element_arg -> bool) -> t -> bool

  val for_all2 : (element_arg -> element_arg -> bool) -> t -> t -> bool

  val exists2 : (element_arg -> element_arg -> bool) -> t -> t -> bool

  val mem : element_arg -> t -> bool

  val find_index : (element_arg -> bool) -> t -> int option

  val find_map : (element_arg -> 'b option) -> t -> 'b option

  val find_mapi : (int -> element_arg -> 'b option) -> t -> 'b option

  (** {1 Sorting} *)

  (* Sort returning a new array *)
  val sort : (element_arg -> element_arg -> int) -> t -> t

end

module Make (M : S0)
  : (S
      with type element_t = M.element_t
      and type ('a : any mod separable) array_t = 'a M.array_t
      and type mutable_t = M.mutable_t) = struct

  include M

  let unsafe_fill_mutable : mutable_t -> int -> int -> element_arg -> unit =
    fun a ofs len v ->
      for i = ofs to ofs + len - 1 do unsafe_set_mutable a i v done


  let make : int -> element_arg -> t = fun n v ->
    let result = unsafe_create_mutable n in
    unsafe_fill_mutable result 0 n v;
    freeze result

  let append_prim: t -> t -> t = fun a1 a2 ->
    let l1 = length a1 in
    let l2 = length a2 in
    let result = unsafe_create_mutable (l1 + l2) in
    blit_to_mutable a1 0 result 0 l1;
    blit_to_mutable a2 0 result l1 l2;
    freeze result

  (* only used to defend against potential overflows in the length
    computation of array concatenation *)
  let ensure_ge (x:int) y =
    if x >= y then x else invalid_arg "Gen_u_iarray.concat"

  let rec sum_lengths acc = function
    | [] -> acc
    | hd :: tl -> sum_lengths (ensure_ge (length hd + acc) acc) tl

  let concat: t list -> t = fun l ->
    let len = sum_lengths 0 l in
    let result = unsafe_create_mutable len in
    let rec loop l i =
      match l with
      | [] -> assert (i = len)
      | hd :: tl ->
        let hlen = length hd in
        blit_to_mutable hd 0 result i hlen;
        loop tl (i + hlen)
    in
    loop l 0;
    freeze result

  let init: int -> (int -> element_arg) -> t = fun l f ->
    if l = 0 then (empty ()) else
    if l < 0 then invalid_arg "Gen_u_iarray.init"
    else
    let res = unsafe_create_mutable l in
    for i = 0 to l - 1 do
      unsafe_set_mutable res i (f i)
    done;
    freeze res

  let make_matrix sx sy init =
    let res = Array.make sx (empty ()) in
    for x = 0 to pred sx do
      Array.unsafe_set res x (make sy init)
    done;
    res

  let sub a ofs len =
    if ofs < 0 || len < 0 || ofs > length a - len
    then invalid_arg "Gen_u_iarray.sub"
    else
      let result = unsafe_create_mutable len in
      blit_to_mutable a ofs result 0 len;
      freeze result

  let copy a =
    let l = length a in
    if l = 0 then (empty ())
    else sub a 0 l

  let append a1 a2 =
    let l1 = length a1 in
    if l1 = 0 then copy a2
    else if length a2 = 0 then sub a1 0 l1
    else append_prim a1 a2

  let iter f a =
    for i = 0 to length a - 1 do f(unsafe_get a i) done

  let iter2 f a b =
    if length a <> length b then
      invalid_arg "Gen_u_iarray.iter2: arrays must have the same length"
    else
      for i = 0 to length a - 1 do f (unsafe_get a i) (unsafe_get b i) done

  let map f a =
    let l = length a in
    if l = 0 then (empty ()) else begin
      let r = unsafe_create_mutable l in
      for i = 0 to l - 1 do
        unsafe_set_mutable r i (f(unsafe_get a i))
      done;
      freeze r
    end

  let map2 f a b =
    let la = length a in
    let lb = length b in
    if la <> lb then
      invalid_arg "Gen_u_iarray.map2: arrays must have the same length"
    else begin
      if la = 0 then (empty ()) else begin
        let r = unsafe_create_mutable la in
        for i = 0 to la - 1 do
          unsafe_set_mutable r i (f (unsafe_get a i) (unsafe_get b i))
        done;
        freeze r
      end
    end

  let iteri f a =
    for i = 0 to length a - 1 do f i (unsafe_get a i) done

  let mapi f a =
    let l = length a in
    if l = 0 then (empty ()) else begin
      let r = unsafe_create_mutable l in
      for i = 0 to l - 1 do
        unsafe_set_mutable r i (f i (unsafe_get a i))
      done;
      freeze r
    end

  let fold_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r (unsafe_get a i)
    done;
    !r

  let fold_right f a x =
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f (unsafe_get a i) !r
    done;
    !r

  let exists p a =
    let n = length a in
    let rec loop i =
      if i = n then false
      else if p (unsafe_get a i) then true
      else loop (succ i) in
    loop 0

  let for_all p a =
    (* take [n], [p], and [a] as parameters to avoid a closure allocation *)
    let rec loop n p a i =
      if i = n then true
      else if p (unsafe_get a i) then loop n p a (succ i)
      else false in
    loop (length a) p a 0

  let for_all2 p l1 l2 =
    let n1 = length l1
    and n2 = length l2 in
    if n1 <> n2 then invalid_arg "Gen_u_iarray.for_all2"
    else let rec loop i =
      if i = n1 then true
      else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
      else false in
    loop 0

  let exists2 p l1 l2 =
    let n1 = length l1
    and n2 = length l2 in
    if n1 <> n2 then invalid_arg "Gen_u_iarray.exists2"
    else let rec loop i =
      if i = n1 then false
      else if p (unsafe_get l1 i) (unsafe_get l2 i) then true
      else loop (succ i) in
    loop 0

  let mem x a =
    let n = length a in
    let rec loop i =
      if i = n then false
      else if compare_element (unsafe_get a i) x = 0 then true
      else loop (succ i) in
    loop 0

  let find_index p a =
    let n = length a in
    let rec loop i =
      if i = n then None
      else if p (unsafe_get a i) then Some i
      else loop (succ i) in
    loop 0

  let find_map f a =
    let n = length a in
    let rec loop i =
      if i = n then None
      else
        match f (unsafe_get a i) with
        | None -> loop (succ i)
        | Some _ as r -> r
    in
    loop 0

  let find_mapi f a =
    let n = length a in
    let rec loop i =
      if i = n then None
      else
        match f i (unsafe_get a i) with
        | None -> loop (succ i)
        | Some _ as r -> r
    in
    loop 0

  exception Bottom of int
  let sort cmp a =
    let len = length a in
    if len = 0 then empty () else
    let a_mut = unsafe_create_mutable len in
    blit_to_mutable a 0 a_mut 0 len;
    let maxson l i =
      let i31 = i+i+i+1 in
      let x = ref i31 in
      if i31+2 < l then begin
        if cmp (unsafe_get_mutable a_mut i31) (unsafe_get_mutable a_mut (i31+1)) < 0 then x := i31+1;
        if cmp (unsafe_get_mutable a_mut !x) (unsafe_get_mutable a_mut (i31+2)) < 0 then x := i31+2;
        !x
      end else
        if i31+1 < l && cmp (unsafe_get_mutable a_mut i31) (unsafe_get_mutable a_mut (i31+1)) < 0
        then i31+1
        else if i31 < l then i31 else raise (Bottom i)
    in
    let rec trickledown l i e =
      let j = maxson l i in
      if cmp (unsafe_get_mutable a_mut j) e > 0 then begin
        unsafe_set_mutable a_mut i (unsafe_get_mutable a_mut j);
        trickledown l j e;
      end else begin
        unsafe_set_mutable a_mut i e;
      end;
    in
    let trickle l i e = try trickledown l i e with Bottom i -> unsafe_set_mutable a_mut i e in
    let rec bubbledown l i =
      let j = maxson l i in
      unsafe_set_mutable a_mut i (unsafe_get_mutable a_mut j);
      bubbledown l j
    in
    let bubble l i = try bubbledown l i with Bottom i -> i in
    let rec trickleup i e =
      let father = (i - 1) / 3 in
      assert (i <> father);
      if cmp (unsafe_get_mutable a_mut father) e < 0 then begin
        unsafe_set_mutable a_mut i (unsafe_get_mutable a_mut father);
        if father > 0 then trickleup father e else unsafe_set_mutable a_mut 0 e;
      end else begin
        unsafe_set_mutable a_mut i e;
      end;
    in
    let l = len in
    for i = (l + 1) / 3 - 1 downto 0 do trickle l i (unsafe_get_mutable a_mut i); done;
    for i = l - 1 downto 2 do
      let e = (unsafe_get_mutable a_mut i) in
      unsafe_set_mutable a_mut i (unsafe_get_mutable a_mut 0);
      trickleup (bubble i 0) e;
    done;
    if l > 1 then (let e = (unsafe_get_mutable a_mut 1) in unsafe_set_mutable a_mut 1 (unsafe_get_mutable a_mut 0); unsafe_set_mutable a_mut 0 e);
    freeze a_mut

end
