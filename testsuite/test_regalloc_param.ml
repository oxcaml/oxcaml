let[@regalloc_param "abc"] f x = x + 1

let[@regalloc_param "def"][@regalloc_param "ghi"] g x y = x + y

let h x = x * 2