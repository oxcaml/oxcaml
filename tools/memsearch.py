# Python extension to GDB, for searching memory for a value.
# Nick Barnes, Jane Street, 2025.
#
# Linux-only, among a wide range of likely limitations and bugs.
#
# Usage:
#
#     $ gdb /bin/sh
#     [...]
#     (gdb) break main
#     Breakpoint 1 at 0x2ec50
#     (gdb) run
#     Starting program: /usr/bin/sh
#     Breakpoint 1, 0x0000555555582c50 in main ()
#     (gdb) source memsearch.py
#     memsearch <arg> [<size>] searches for <arg> across memory. <size> defaults to 8.
#     (gdb) memsearch 0x7ffff77d1600
#     Found at 0x55555566b620: <stderr>
#     Found at 0x7ffff77cfe88 (no symbol)
#     Found at 0x7ffff77d15e0: <__GI__IO_list_all>
#     Found at 0x7ffff77d17c0: <stderr>
#     (gdb) memsearch 0x215e 4
#     Found at 0x55555566b388: <history_subst_char>
#     Found at 0x7ffff75876c0: <translit_from_tbl+1088>
#     (gdb)
#
# The expression argument to mysearch must not contain spaces, because
# there's an optional second argument (the value size: 1, 2, 4, or 8)
# and I couldn't be bothered to do any parsing.
#
# This is fairly low-common-denominator code, suitable for GDB 8 or later.
# So it works around various terrible things in some older GDBs.
#
# Notes on discovering memory mappings in GDB:
#
# For a live process we could use "info proc mappings", but this was
# problematic with some old version of GDB (I forget which) so I read
# /proc/<pid>/maps instead. With a core file, that only reports file
# mappings (e.g. from object files). It doesn't report anonymous
# mappings such as those used for the C or OCaml heaps or stacks. To
# get a list of the other mappings preserved in the core dump, one
# must use "maintenance info sections".
#
# The output of that is divided between executable(s), library
# file(s), and the core dump. You can pass -all-objects to get library
# object files, and ALLOC to get secions which are actually allocated
# in the address space. Sample "maintenance info sections -all-objects ALLOC" output:
#
#   Exec file: `/usr/local/home/nbarnes/2025-07-21-blobstore-segv/blobstore_admin.exe', file type elf64-x86-64.
#    [0]      0x002002e0->0x002002fc at 0x000002e0: .interp ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [1]      0x002002fc->0x0020031c at 0x000002fc: .note.ABI-tag ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [2]      0x0020031c->0x00200334 at 0x0000031c: .note.gnu.build-id ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [3]      0x00200338->0x01cb91d8 at 0x00000338: .dynsym ALLOC LOAD READONLY DATA HAS_CONTENTS\
#   ...
#   Object file: `/lib64/libarchive.so.13', file type elf64-x86-64.
#    [0]      0x7ffff7800238->0x7ffff780025c at 0x00000238: .note.gnu.build-id ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [1]      0x7ffff7800260->0x7ffff7801510 at 0x00000260: .gnu.hash ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [2]      0x7ffff7801510->0x7ffff78061a8 at 0x00001510: .dynsym ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [3]      0x7ffff78061a8->0x7ffff780ab93 at 0x000061a8: .dynstr ALLOC LOAD READONLY DATA HAS_CONTENTS
#    [4]      0x7ffff780ab94->0x7ffff780b1f6 at 0x0000ab94: .gnu.version ALLOC LOAD READONLY DATA HAS_CONTENTS
#    ...
#   ...
#   Core file: `/usr/local/home/nbarnes/2025-07-21-blobstore-segv/core.blobstore_admin.19832.0bb3563e53e54593925ab91babacaab8.252363.1752782893000000', file type elf64-x86-64.
#    [165]      0x00200000->0x00201000 at 0x0003d000: load1a ALLOC LOAD READONLY HAS_CONTENTS
#    [166]      0x00201000->0x0805d000 at 0x0003e000: load1b ALLOC READONLY
#    [167]      0x0805d000->0x0814e000 at 0x0003e000: load2 ALLOC READONLY CODE
#    ...
#    [2044]     0x7ffff77df000->0x7ffff7800000 at 0x2613bd000: load1830 ALLOC LOAD HAS_CONTENTS
#    [2045]     0x7ffff7800000->0x7ffff7801000 at 0x2613de000: load1831a ALLOC LOAD READONLY CODE HAS_CONTENTS
#    [2046]     0x7ffff7801000->0x7ffff78b6000 at 0x2613df000: load1831b ALLOC READONLY CODE
#    [2047]     0x7ffff78b6000->0x7ffff7ab6000 at 0x2613df000: load1832 ALLOC READONLY
#    ...
#
# Note that the list of core file sections includes all the actual
# memory mappings from object files, so we don't need the entries from
# the object (or executable) files to search the whole address space:
# many of those sections are pretty small, unaligned, often mapped
# contiguously in the address space, etc. Perhaps a later version of
# the tool could use this to provide further information once a search
# hits, but for now we only use the "Core" sections.

import gdb

import re
# a file line from "maintenance info sections"
maint_file_re = re.compile(r"^([a-zA-Z]+) file: '(.*)'.*$")
# a section line from "maintenance info sections"
maint_section_re = re.compile(r'^ *\[[0-9]+\] +0x([0-9a-f]+)->0x([0-9a-f]+) at.*: (.*)$')

# a line from "info proc mapping", which we no longer parse.
# proc_mapping_re = re.compile(r'^ *0x([0-9a-f]+) +0x([0-9a-f]+) +0x[0-9a-f]+ +0x[0-9a-f]+ (.*)$')

def get_mappings():
    """Yield (base, limit, file, permissions) for each memory mapping
    in the inferior."""
    # Attempt to get live mappings from /proc/<pid>/maps. This will
    # fail with a coredump so in that case we back off to "maintenance
    # info sections" (which doesn't have as much information).
    pid = gdb.selected_inferior().pid
    try:
        for m in open(f'/proc/{pid}/maps'):
            l = m.split()
            if 'r' in l[1]: # readable
                base,lim = l[0].split('-')
                base,lim = int(base, 16), int(lim,16)
                file = l[5] if len(l) > 5 else ""
                yield base, lim, file, l[1]
    except FileNotFoundError:
        # Presumably running on a core dump rather than a live
        # process.  Parse the outputs of 'maintenance info sections`
        # This may be fragile to changes in GDB.
        text = gdb.execute('maintenance info sections ALLOC', to_string=True)
        in_core_file = False
        for l in text.split('\n'):
            m = maint_file_re.match(l)
            if m:
                in_core_file = (m.group(1) == 'Core')
                continue
            if not in_core_file:
                continue
            m = maint_section_re.match(l)
            if m:
                tail = m.group(3)
                perms = ('r'
                         + ('-' if ' READONLY' in tail else 'w')
                         + ('x' if ' CODE' in tail else '-')
                         + '-') # can't determine "p" (i.e. "private"/copy-on-write).

                yield (int(m.group(1), 16),
                       int(m.group(2), 16),
                       "core",
                       perms)

def search(arg, val, size):
    inf = gdb.selected_inferior()
    v = int(val)
    size_char = {1:'b', 2:'h', 4:'w', 8:'g'}
    if size not in size_char:
        raise ValueError(f"don't know how to search for values size {size}")

    for base, limit, file, perms in get_mappings():
        if limit-base < size:
            continue
        # Very annoyingly gdb.Inferior.search_memory is a bit rubbish:
        # There are some mappings in "info proc mappings" or "/proc/<pid>/maps"
        # which are not readable; search_memory produces both a noisy warning
        # and a garbage answer for such mappings. Instead, we can use
        # "find".
        cmd = f'find /{size_char[size]} 0x{base:x},+0x{limit-base:x},0x{v:016x}'
        text = gdb.execute(cmd, to_string=True)
        if "Pattern not found" in text:
            continue
        for line in text.split('\n'):
            if not line.startswith('0x'):
                continue
            words = line.split(' ')
            if len(words) > 1:
                print(f"Found at {words[0]}: {words[1]}")
            else:
                print(f"Found at {words[0]} (no symbol)")

        # Broken Python version:
        #
        # needle = bytes(v >> (8*i) & 255 for i in range(8))
        # p = base
        # while p:
        #     p = inf.search_memory(p, limit-p, needle)
        #     if p is None:
        #         break
        #     if p < base or p >= limit:
        #         print(f"Out of range {p:x} not in {base:x}-{limit:x}")
        #         break
        #     print(f"Found at 0x{p:x} in {base:x}-{limit:x} ({file}:{perms})")
        #     p += 8

class SearchCommand(gdb.Command):
    "ocaml find <expr>: report the location of <expr> on the OCaml heap."
    def __init__(self):
        super(SearchCommand, self).__init__("memsearch", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        self.dont_repeat()
        if ' ' in arg:
            v,s = arg.split()
            size = int(s)
        else:
            v,size = arg, 8
        val = gdb.parse_and_eval(v)
        search(v, val, size)

SearchCommand()

print("memsearch <arg> [<size>] searches for <arg> across memory. <size> defaults to 8.")
