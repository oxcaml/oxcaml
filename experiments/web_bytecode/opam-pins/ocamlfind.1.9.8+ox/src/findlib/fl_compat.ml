let ( |> ) x f = f x

module String = struct
  let split_on_char sep s =
    let r = ref [] in
    let j = ref (String.length s) in
    for i = String.length s - 1 downto 0 do
      if String.unsafe_get s i = sep then begin
        r := String.sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    String.sub s 0 !j :: !r

  let starts_with ~prefix s =
    let len_s = String.length s
    and len_pre = String.length prefix in
    let rec aux i =
      if i = len_pre then true
      else if String.unsafe_get s i <> String.unsafe_get prefix i then false
      else aux (i + 1)
    in len_s >= len_pre && aux 0

  include String
end

module List = struct
  let rec find_map f = function
    | [] -> None
    | x :: l ->
       begin match f x with
         | Some _ as result -> result
         | None -> find_map f l
       end

  let rec filter_map f = function
    | [] -> []
    | x :: l -> (
       match f x with
       | None -> filter_map f l
       | Some v -> v :: filter_map f l)

  include List
end

module Option = struct
  let value o ~default = match o with Some v -> v | None -> default

  (* can't include Option because it was only introduced in 4.08 *)
  (* include Option *)
end

module Sys = struct
  let getenv_opt s =
    try Some (Sys.getenv s)
    with Not_found -> None

  include Sys
end
