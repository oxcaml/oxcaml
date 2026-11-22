external frame_pointers : bool = "%frame_pointers"

let[@inline always] compiled_with_frame_pointers () = frame_pointers
