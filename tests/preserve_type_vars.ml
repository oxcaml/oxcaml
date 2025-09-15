module A : sig 
  type ('foo, 'bar) t

  val get_foo : ('foo, _) t -> 'foo option
end = struct
  type ('foo, 'bar) t =
    | Foo of 'foo
    | Bar of 'bar

  let get_foo = function
    | Foo foo -> Some foo
    | Bar _ -> None
end
;;

A.get_foo
;;
