include File_cache.Make (struct
  type t = Index_format.file_content
  let read file = Index_format.read ~file
  let cache_name = "Index_cache"
end)
