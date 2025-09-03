let[@regalloc_param "abc"] f x = x

let[@regalloc_param "def"][@regalloc_param "ghi"] g x y = 
  let z = x in z

let h x = x