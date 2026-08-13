type state = Not_ready | Broken of string | Ready

exception Invalid_config of state

let state = ref (Broken "deliberate failure")
