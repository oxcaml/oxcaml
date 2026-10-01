
let is_keyword_hook = ref (fun _ -> assert false)

let is_keyword s =
  !is_keyword_hook s
