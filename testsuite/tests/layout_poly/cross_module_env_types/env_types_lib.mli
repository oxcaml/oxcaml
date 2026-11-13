@@ static

val f : int -> int @@ dynamic

val top : layout_ l. ('a : l). 'a -> #(int * 'a)

module F (X : sig
  val x : int @@ dynamic
end @ static) : sig
  val g : int -> int @@ dynamic
  val r : int @@ dynamic
  val h : layout_ l. ('a : l). 'a -> #(int * 'a * int)
end @ static

module M1 : sig
  val r : int @@ dynamic
  val h : layout_ l. ('a : l). 'a -> #(int * 'a * int)
end

module M2 : sig
  val r : int @@ dynamic
  val h : layout_ l. ('a : l). 'a -> #(int * 'a * int)
end
