  $ cat >main.ml <<'EOF'
  > module M = struct module N = struct let x = () let y = () end end
  > module B = M.N
  > EOF

With index 0 only the first type is shown:
  $ $MERLIN single type-enclosing -position 2:7 -verbosity 0 -index 0 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 8
        },
        "type": "(module M.N)",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:7 -verbosity 1 -index 0 \
  > -filename ./main.ml < ./main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 7
        },
        "end": {
          "line": 2,
          "col": 8
        },
        "type": "sig val x : unit val y : unit end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ cat >main.ml <<'EOF'
  > module M = struct module N = List end
  > module B = M.N
  > EOF

With index 0 only the first type is shown:
  $ $MERLIN single type-enclosing -position 2:13 -verbosity 0 -index 0 \
  > -filename ./main.ml < ./main.ml  
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": "(module List)",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }

  $ $MERLIN single type-enclosing -position 2:13 -verbosity 1 -index 0 \
  > -filename ./main.ml < ./main.ml  
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": "sig
    type ('a : value_or_null) t = 'a list = [] | (::) of 'a * 'a list
    val length : 'a list -> int @@ stateless
    val compare_lengths : 'a list -> 'b list -> int @@ stateless
    val compare_length_with : 'a list -> int -> int @@ stateless
    val is_empty : 'a list -> bool @@ stateless
    val cons : 'a -> 'a list -> 'a list @@ stateless
    val singleton : 'a -> 'a list @@ stateless
    val hd : 'a list -> 'a @@ stateless
    val tl : 'a list -> 'a list @@ stateless
    val nth : 'a list -> int -> 'a @@ stateless
    val nth_opt : 'a list -> int -> 'a option @@ stateless
    val rev : 'a list -> 'a list @@ stateless
    val init : int -> (int -> 'a) -> 'a list @@ stateless
    val append : 'a list -> 'a list -> 'a list @@ stateless
    val rev_append : 'a list -> 'a list -> 'a list @@ stateless
    val concat : 'a list list -> 'a list @@ stateless
    val flatten : 'a list list -> 'a list @@ stateless
    val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool @@ stateless
    val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int @@ stateless
    val iter : ('a -> unit) -> 'a list -> unit @@ stateless
    val iteri : (int -> 'a -> unit) -> 'a list -> unit @@ stateless
    val map : ('a -> 'b) -> 'a list -> 'b list @@ stateless
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list @@ stateless
    val rev_map : ('a -> 'b) -> 'a list -> 'b list @@ stateless
    val filter_map : ('a -> 'b option) -> 'a list -> 'b list @@ stateless
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list @@ stateless
    val fold_left_map :
      ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list @@
      stateless
    val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc @@
      stateless
    val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc @@
      stateless
    val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit @@ stateless
    val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list @@ stateless
    val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list @@
      stateless
    val fold_left2 :
      ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc @@
      stateless
    val fold_right2 :
      ('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc @@
      stateless
    val for_all : ('a -> bool) -> 'a list -> bool @@ stateless
    val exists : ('a -> bool) -> 'a list -> bool @@ stateless
    val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool @@
      stateless
    val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool @@ stateless
    val mem : 'a @ local -> 'a list @ local -> bool @@ stateless
    val memq : 'a @ local -> 'a list @ local -> bool @@ stateless
    val find : ('a -> bool) -> 'a list -> 'a @@ stateless
    val find_opt : ('a -> bool) -> 'a list -> 'a option @@ stateless
    val find_index : ('a -> bool) -> 'a list -> int option @@ stateless
    val find_map : ('a -> 'b option) -> 'a list -> 'b option @@ stateless
    val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option @@
      stateless
    val filter : ('a -> bool) -> 'a list -> 'a list @@ stateless
    val find_all : ('a -> bool) -> 'a list -> 'a list @@ stateless
    val filteri : (int -> 'a -> bool) -> 'a list -> 'a list @@ stateless
    val take : int -> 'a list -> 'a list @@ stateless
    val drop : int -> 'a list -> 'a list @@ stateless
    val take_while : ('a -> bool) -> 'a list -> 'a list @@ stateless
    val drop_while : ('a -> bool) -> 'a list -> 'a list @@ stateless
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list @@ stateless
    val partition_map :
      ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list @@ stateless
    val assoc : 'a -> ('a * 'b) list -> 'b @@ stateless
    val assoc_opt : 'a -> ('a * 'b) list -> 'b option @@ stateless
    val assq : 'a -> ('a * 'b) list -> 'b @@ stateless
    val assq_opt : 'a -> ('a * 'b) list -> 'b option @@ stateless
    val mem_assoc : 'a -> ('a * 'b) list -> bool @@ stateless
    val mem_assq : 'a -> ('a * 'b) list -> bool @@ stateless
    val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list @@ stateless
    val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list @@ stateless
    val split : ('a * 'b) list -> 'a list * 'b list @@ stateless
    val combine : 'a list -> 'b list -> ('a * 'b) list @@ stateless
    val sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ stateless
    val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ stateless
    val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ stateless
    val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list @@ stateless
    val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list @@ stateless
    val to_seq : 'a list -> 'a Seq.t @@ stateless
    val of_seq : 'a Seq.t -> 'a list @@ stateless
  end",
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 11
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 1,
        "tail": "no"
      },
      {
        "start": {
          "line": 2,
          "col": 0
        },
        "end": {
          "line": 2,
          "col": 14
        },
        "type": 2,
        "tail": "no"
      }
    ],
    "notifications": []
  }
