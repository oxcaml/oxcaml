module Example_usage = struct
  type t =
    | Int of int
    | String of int
  [@@deriving traverse_iter]

  let iter =
    object
      inherit iter as super

      (* 1. handle the base types *)
      method int _ = ()

      method! t =
        function
        | Int i ->
          (* 2. override the case you want to handle *)
          ignore i
        | _ as t ->
          (* 3. dispatch back to the superclass for the rest *)
          super#t t
    end
  ;;
end

module Catalog_of_available_traversals = struct
  type t = { u : u }

  and u =
    | Int of int
    | With_module_prefix of Int.t
    | Polymorphic of (int, bool) Either.t
    | Functor of int Map.Make(Int).t
  [@@deriving_inline traverse]
  
let _ = fun (_ : t) -> ()
let _ = fun (_ : u) -> ()
class virtual map =
  object (self)
    method virtual  bool : bool -> bool
    method virtual  int : int -> int
    method virtual  either__t :
      'a 'b .
        ('a -> 'a) -> ('b -> 'b) -> ('a, 'b) Either.t -> ('a, 'b) Either.t
    method virtual  int__t : Int.t -> Int.t
    method virtual  map__make_'int'__t :
      'a . ('a -> 'a) -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t
    method t : t -> t= fun { u } -> let u = self#u u in { u }
    method u : u -> u=
      fun x ->
        match x with
        | Int a -> let a = self#int a in Int a
        | With_module_prefix a ->
            let a = self#int__t a in With_module_prefix a
        | Polymorphic a ->
            let a = self#either__t self#int self#bool a in Polymorphic a
        | Functor a ->
            let a = self#map__make_'int'__t self#int a in Functor a
  end
class virtual iter =
  object (self)
    method virtual  bool : bool -> unit
    method virtual  int : int -> unit
    method virtual  either__t :
      'a 'b . ('a -> unit) -> ('b -> unit) -> ('a, 'b) Either.t -> unit
    method virtual  int__t : Int.t -> unit
    method virtual  map__make_'int'__t :
      'a . ('a -> unit) -> 'a Map.Make(Int).t -> unit
    method t : t -> unit= fun { u } -> self#u u
    method u : u -> unit=
      fun x ->
        match x with
        | Int a -> self#int a
        | With_module_prefix a -> self#int__t a
        | Polymorphic a -> self#either__t self#int self#bool a
        | Functor a -> self#map__make_'int'__t self#int a
  end
class virtual ['acc] fold =
  object (self)
    method virtual  bool : bool -> 'acc -> 'acc
    method virtual  int : int -> 'acc -> 'acc
    method virtual  either__t :
      'a 'b .
        ('a -> 'acc -> 'acc) ->
          ('b -> 'acc -> 'acc) -> ('a, 'b) Either.t -> 'acc -> 'acc
    method virtual  int__t : Int.t -> 'acc -> 'acc
    method virtual  map__make_'int'__t :
      'a . ('a -> 'acc -> 'acc) -> 'a Map.Make(Int).t -> 'acc -> 'acc
    method t : t -> 'acc -> 'acc= fun { u } acc -> self#u u acc
    method u : u -> 'acc -> 'acc=
      fun x acc ->
        match x with
        | Int a -> self#int a acc
        | With_module_prefix a -> self#int__t a acc
        | Polymorphic a -> self#either__t self#int self#bool a acc
        | Functor a -> self#map__make_'int'__t self#int a acc
  end
class virtual ['acc] fold_map =
  object (self)
    method virtual  bool : bool -> 'acc -> (bool * 'acc)
    method virtual  int : int -> 'acc -> (int * 'acc)
    method virtual  either__t :
      'a 'b .
        ('a -> 'acc -> ('a * 'acc)) ->
          ('b -> 'acc -> ('b * 'acc)) ->
            ('a, 'b) Either.t -> 'acc -> (('a, 'b) Either.t * 'acc)
    method virtual  int__t : Int.t -> 'acc -> (Int.t * 'acc)
    method virtual  map__make_'int'__t :
      'a .
        ('a -> 'acc -> ('a * 'acc)) ->
          'a Map.Make(Int).t -> 'acc -> ('a Map.Make(Int).t * 'acc)
    method t : t -> 'acc -> (t * 'acc)=
      fun { u } acc -> let (u, acc) = self#u u acc in ({ u }, acc)
    method u : u -> 'acc -> (u * 'acc)=
      fun x acc ->
        match x with
        | Int a -> let (a, acc) = self#int a acc in ((Int a), acc)
        | With_module_prefix a ->
            let (a, acc) = self#int__t a acc in ((With_module_prefix a), acc)
        | Polymorphic a ->
            let (a, acc) = self#either__t self#int self#bool a acc in
            ((Polymorphic a), acc)
        | Functor a ->
            let (a, acc) = self#map__make_'int'__t self#int a acc in
            ((Functor a), acc)
  end
class virtual ['ctx] map_with_context =
  object (self)
    method virtual  bool : 'ctx -> bool -> bool
    method virtual  int : 'ctx -> int -> int
    method virtual  either__t :
      'a 'b .
        ('ctx -> 'a -> 'a) ->
          ('ctx -> 'b -> 'b) ->
            'ctx -> ('a, 'b) Either.t -> ('a, 'b) Either.t
    method virtual  int__t : 'ctx -> Int.t -> Int.t
    method virtual  map__make_'int'__t :
      'a .
        ('ctx -> 'a -> 'a) ->
          'ctx -> 'a Map.Make(Int).t -> 'a Map.Make(Int).t
    method t : 'ctx -> t -> t= fun ctx { u } -> let u = self#u ctx u in { u }
    method u : 'ctx -> u -> u=
      fun ctx x ->
        match x with
        | Int a -> let a = self#int ctx a in Int a
        | With_module_prefix a ->
            let a = self#int__t ctx a in With_module_prefix a
        | Polymorphic a ->
            let a = self#either__t self#int self#bool ctx a in Polymorphic a
        | Functor a ->
            let a = self#map__make_'int'__t self#int ctx a in Functor a
  end
class virtual ['res] lift =
  object (self)
    method virtual  record : (string * 'res) list -> 'res
    method virtual  constr : string -> 'res list -> 'res
    method virtual  bool : bool -> 'res
    method virtual  int : int -> 'res
    method virtual  either__t :
      'a 'b . ('a -> 'res) -> ('b -> 'res) -> ('a, 'b) Either.t -> 'res
    method virtual  int__t : Int.t -> 'res
    method virtual  map__make_'int'__t :
      'a . ('a -> 'res) -> 'a Map.Make(Int).t -> 'res
    method t : t -> 'res=
      fun { u } -> let u = self#u u in self#record [("u", u)]
    method u : u -> 'res=
      fun x ->
        match x with
        | Int a -> let a = self#int a in self#constr "Int" [a]
        | With_module_prefix a ->
            let a = self#int__t a in self#constr "With_module_prefix" [a]
        | Polymorphic a ->
            let a = self#either__t self#int self#bool a in
            self#constr "Polymorphic" [a]
        | Functor a ->
            let a = self#map__make_'int'__t self#int a in
            self#constr "Functor" [a]
  end
class virtual ['ctx,'res] lift_map_with_context =
  object (self)
    method virtual  record : 'ctx -> (string * 'res) list -> 'res
    method virtual  constr : 'ctx -> string -> 'res list -> 'res
    method virtual  bool : 'ctx -> bool -> (bool * 'res)
    method virtual  int : 'ctx -> int -> (int * 'res)
    method virtual  either__t :
      'a 'b .
        ('ctx -> 'a -> ('a * 'res)) ->
          ('ctx -> 'b -> ('b * 'res)) ->
            'ctx -> ('a, 'b) Either.t -> (('a, 'b) Either.t * 'res)
    method virtual  int__t : 'ctx -> Int.t -> (Int.t * 'res)
    method virtual  map__make_'int'__t :
      'a .
        ('ctx -> 'a -> ('a * 'res)) ->
          'ctx -> 'a Map.Make(Int).t -> ('a Map.Make(Int).t * 'res)
    method t : 'ctx -> t -> (t * 'res)=
      fun ctx { u } ->
        let u = self#u ctx u in
        ({ u = (Stdlib.fst u) }, (self#record ctx [("u", (Stdlib.snd u))]))
    method u : 'ctx -> u -> (u * 'res)=
      fun ctx x ->
        match x with
        | Int a ->
            let a = self#int ctx a in
            ((Int (Stdlib.fst a)), (self#constr ctx "Int" [Stdlib.snd a]))
        | With_module_prefix a ->
            let a = self#int__t ctx a in
            ((With_module_prefix (Stdlib.fst a)),
              (self#constr ctx "With_module_prefix" [Stdlib.snd a]))
        | Polymorphic a ->
            let a = self#either__t self#int self#bool ctx a in
            ((Polymorphic (Stdlib.fst a)),
              (self#constr ctx "Polymorphic" [Stdlib.snd a]))
        | Functor a ->
            let a = self#map__make_'int'__t self#int ctx a in
            ((Functor (Stdlib.fst a)),
              (self#constr ctx "Functor" [Stdlib.snd a]))
  end
[@@@end]
end
