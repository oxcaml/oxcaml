  $ $MERLIN single type-expression -expression "y" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value y",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "y" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "int",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "t" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value t",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "t" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value t",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "x + y" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound value x",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "x + y" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "int",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "T" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound constructor T",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "T" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "t",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "M" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound constructor M",
    "notifications": []
  }
  $ $MERLIN single type-expression -expression "M" -position end -filename test.ml < test.ml
  {
    "class": "return",
    "value": "(module List)",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "MT" -position start -filename test.ml < test.ml
  {
    "class": "return",
    "value": "Unbound constructor MT",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "MT" -position end -filename test.ml < test.ml 
  {
    "class": "return",
    "value": "sig
    type ('a : value_or_null) t = 'a list = [] | (::) of 'a * 'a list
    val length : 'a list -> int @@ portable
    val compare_lengths : 'a list -> 'b list -> int @@ portable
    val compare_length_with : 'a list -> int -> int @@ portable
    val is_empty : 'a list -> bool @@ portable
    val cons : 'a -> 'a list -> 'a list @@ portable
    val hd : 'a list -> 'a @@ portable
    val tl : 'a list -> 'a list @@ portable
    val nth : 'a list -> int -> 'a @@ portable
    val nth_opt : 'a list -> int -> 'a option @@ portable
    val rev : 'a list -> 'a list @@ portable
    val init : int -> (int -> 'a) -> 'a list @@ portable
    val append : 'a list -> 'a list -> 'a list @@ portable
    val rev_append : 'a list -> 'a list -> 'a list @@ portable
    val concat : 'a list list -> 'a list @@ portable
    val flatten : 'a list list -> 'a list @@ portable
    val equal : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool @@ portable
    val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int @@ portable
    val iter : ('a -> unit) -> 'a list -> unit @@ portable
    val iteri : (int -> 'a -> unit) -> 'a list -> unit @@ portable
    val map : ('a -> 'b) -> 'a list -> 'b list @@ portable
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list @@ portable
    val rev_map : ('a -> 'b) -> 'a list -> 'b list @@ portable
    val filter_map : ('a -> 'b option) -> 'a list -> 'b list @@ portable
    val concat_map : ('a -> 'b list) -> 'a list -> 'b list @@ portable
    val fold_left_map :
      ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list @@
      portable
    val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc @@ portable
    val fold_right : ('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc @@
      portable
    val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit @@ portable
    val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list @@ portable
    val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list @@
      portable
    val fold_left2 :
      ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a list -> 'b list -> 'acc @@
      portable
    val fold_right2 :
      ('a -> 'b -> 'acc -> 'acc) -> 'a list -> 'b list -> 'acc -> 'acc @@
      portable
    val for_all : ('a -> bool) -> 'a list -> bool @@ portable
    val exists : ('a -> bool) -> 'a list -> bool @@ portable
    val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool @@ portable
    val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool @@ portable
    val mem : 'a @ local -> 'a list @ local -> bool @@ portable
    val memq : 'a @ local -> 'a list @ local -> bool @@ portable
    val find : ('a -> bool) -> 'a list -> 'a @@ portable
    val find_opt : ('a -> bool) -> 'a list -> 'a option @@ portable
    val find_index : ('a -> bool) -> 'a list -> int option @@ portable
    val find_map : ('a -> 'b option) -> 'a list -> 'b option @@ portable
    val find_mapi : (int -> 'a -> 'b option) -> 'a list -> 'b option @@
      portable
    val filter : ('a -> bool) -> 'a list -> 'a list @@ portable
    val find_all : ('a -> bool) -> 'a list -> 'a list @@ portable
    val filteri : (int -> 'a -> bool) -> 'a list -> 'a list @@ portable
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list @@ portable
    val partition_map :
      ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list @@ portable
    val assoc : 'a -> ('a * 'b) list -> 'b @@ portable
    val assoc_opt : 'a -> ('a * 'b) list -> 'b option @@ portable
    val assq : 'a -> ('a * 'b) list -> 'b @@ portable
    val assq_opt : 'a -> ('a * 'b) list -> 'b option @@ portable
    val mem_assoc : 'a -> ('a * 'b) list -> bool @@ portable
    val mem_assq : 'a -> ('a * 'b) list -> bool @@ portable
    val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list @@ portable
    val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list @@ portable
    val split : ('a * 'b) list -> 'a list * 'b list @@ portable
    val combine : 'a list -> 'b list -> ('a * 'b) list @@ portable
    val sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ portable
    val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ portable
    val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list @@ portable
    val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list @@ portable
    val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list @@ portable
    val to_seq : 'a list -> 'a Seq.t @@ portable
    val of_seq : 'a Seq.t -> 'a list @@ portable
  end",
    "notifications": []
  }

  $ $MERLIN single type-expression -expression "f (" -position start \
  > -filename test.ml < test.ml | \
  > sed 's/\("value": \)".*\.Error.*",/\1<syntax error>,/'
  {
    "class": "return",
    "value": <syntax error>,
    "notifications": []
  }
