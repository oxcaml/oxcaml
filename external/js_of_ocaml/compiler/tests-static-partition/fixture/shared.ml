external record : string -> unit = "partition_test_record"

type state = { mutable value : int }

let counter = ref 0
let state = ref { value = 1 }
let slot : (unit -> int) option ref = ref None
let factory : (unit -> int) option ref = ref None
let () = record "shared"
