"""Compare two files of concatenated marshal blobs (e.g. the output of
extern_standalone_test.c against that of
extern_standalone_gen_expected.ml), ignoring the header field at offset
12 of each blob (size in words when read on a 32-bit platform), which
extern_standalone.c writes as 0.

Usage:  python3 extern_standalone_compare.py actual.bin expected.bin
"""
import sys

def blobs(path):
    data = open(path, 'rb').read()
    i, out = 0, []
    while i < len(data):
        magic = int.from_bytes(data[i:i+4], 'big')
        assert magic == 0x8495A6BE, f"unexpected magic {magic:#x} at {i}"
        datalen = int.from_bytes(data[i+4:i+8], 'big')
        out.append(bytearray(data[i:i+20+datalen]))
        i += 20 + datalen
    assert i == len(data)
    return out

a, b = blobs(sys.argv[1]), blobs(sys.argv[2])
assert len(a) == len(b), f"blob counts differ: {len(a)} vs {len(b)}"
for n, (x, y) in enumerate(zip(a, b)):
    x[12:16] = b'\0\0\0\0'
    y[12:16] = b'\0\0\0\0'
    if x != y:
        print(f"blob {n} differs")
        sys.exit(1)
print(f"OK: {len(a)} blobs identical modulo the 32-bit size field")
