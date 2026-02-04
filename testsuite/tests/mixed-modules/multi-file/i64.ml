external box : int64# -> int64 = "%box_int64"

let to_f64 i64 = F64.of_int64 (box i64) [@@zero_alloc]
