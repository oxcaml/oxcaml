import re

# Trim trailing numbers on symbols.
#
def trim_basename(name):
    return re.sub(r"(caml.*)_[0-9]+", r"\1", name)

class PrintBacktrace (gdb.Command):
  """Print backtrace by walking GDB frames"""

  def __init__ (self):
    super (PrintBacktrace, self).__init__ ("print_backtrace", gdb.COMMAND_USER)

  def invoke (self, arg, from_tty):
    # A function that the C compiler split or inlined in part (as GCC does
    # with caml_startup_common, but not Clang) shows up as several
    # consecutive frames of the same name. Print such frames once, so that
    # the output does not depend on the C compiler.
    frame = gdb.selected_frame ()
    level = 0
    previous = None
    while (not (frame is None)):
        name = trim_basename(frame.name())
        if name != previous:
            print("frame %i: %s"% (level, name))
            level += 1
        previous = name
        frame = frame.older();

PrintBacktrace ()

# Usage:
# (gdb) print-backtrace
