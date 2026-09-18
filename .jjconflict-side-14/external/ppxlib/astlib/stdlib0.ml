module Int = struct
  let to_string = string_of_int
end

module Option = struct
  let is_none o = match o with None -> true | Some _ -> false
  let is_some o = match o with None -> false | Some _ -> true
  let map f o = match o with None -> None | Some v -> Some (f v)
end
