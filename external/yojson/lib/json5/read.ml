open Let_syntax.Result

module type S = sig
  type t

  val convert : Ast.t -> t
end

module type Out = sig
  type t

  val from_string : ?fname:string -> ?lnum:int -> string -> (t, string) result

  val from_channel :
    ?fname:string -> ?lnum:int -> in_channel -> (t, string) result

  val from_file : ?fname:string -> ?lnum:int -> string -> (t, string) result
end

module Make (F : S) : Out with type t = F.t = struct
  type t = F.t

  let from_string ?fname ?lnum input =
    let+ ast = Parser.parse_from_string ?fname ?lnum input in
    F.convert ast

  let from_channel ?fname ?lnum ic =
    let+ ast = Parser.parse_from_channel ?fname ?lnum ic in
    F.convert ast

  let from_file ?fname ?lnum file =
    let+ ast = Parser.parse_from_file ?fname ?lnum file in
    F.convert ast
end
