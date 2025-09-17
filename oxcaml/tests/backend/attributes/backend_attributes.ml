let[@regalloc irc] with_irc x = x

let[@regalloc irc] [@regalloc_param "IRC_SPILLING_HEURISTICS=flat"] with_irc_and_param
    x =
  x
