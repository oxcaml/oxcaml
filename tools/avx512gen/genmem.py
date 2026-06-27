#!/usr/bin/env python3
"""Generator for AVX512 *memory* intrinsics (load / store, masked and unmasked).

These differ from the compute intrinsics handled by generate.py in two ways:
  * they lower to [Isimd_mem] (the address occupies the binding's mem operand),
    spliced into the `select_operation_avx512_mem` function; and
  * the bit-exact test has a different shape -- a 64-byte buffer is filled (via a
    C helper), then loaded/stored through both the OxCaml builtin and the very
    same Intel intrinsic, and the results (a vector for loads, the buffer bytes
    for stores) are compared lane-by-lane.

Run:  python3 tools/avx512gen/genmem.py
"""
import os, re, sys, subprocess
import xml.etree.ElementTree as ET
sys.path.insert(0, os.path.dirname(__file__))
import simdb

HERE = os.path.dirname(__file__)
ROOT = os.path.normpath(os.path.join(HERE, "..", ".."))
XML = os.path.join(HERE, "intel_intrinsics.xml")
SELECTION = os.path.join(ROOT, "backend/amd64/simd_selection.ml")
TESTDIR = os.path.join(ROOT, "oxcaml/tests/simd/avx512")
ITERS = 100

TARGET_EXT = {"AVX512F","AVX512VL","AVX512DQ","AVX512CD","AVX512BW"}

binds, by_key = simdb.load()

# element tag -> (ml base type, element bits, is-float, unaligned mnem, aligned mnem)
ELT = {
    "epi8":  ("int",   8,  False, "VMOVDQU8",  None),
    "epi16": ("int",   16, False, "VMOVDQU16", None),
    "epi32": ("int",   32, False, "VMOVDQU32", "VMOVDQA32"),
    "epi64": ("int",   64, False, "VMOVDQU64", "VMOVDQA64"),
    "ps":    ("float", 32, False, "VMOVUPS",   "VMOVAPS"),
    "pd":    ("float", 64, False, "VMOVUPD",   "VMOVAPD"),
}
# si128/256/512 are untyped: represented as int64xN, no masking, vmovdqu64/a64.
SI = {"si128":128, "si256":256, "si512":512}

def vt_of(base, bits, width): return f"{base}{bits}x{width//bits}"

# ---- parse the load/store intrinsics we handle ----
LS_RE = re.compile(r'^_mm(512|256|)_(mask_|maskz_)?(load|loadu|store|storeu)_'
    r'(epi8|epi16|epi32|epi64|ps|pd|si128|si256|si512)$')

root = ET.parse(XML).getroot()
intr = {}
for it in root.findall("intrinsic"):
    cps={c.text for c in it.findall("CPUID")}
    if not cps or not cps <= TARGET_EXT: continue
    if it.get("tech")!="AVX-512": continue
    intr[it.get("name")] = it

def width_of(w): return {"512":512,"256":256,"":128}[w]

# the AVX512 feature subset (f/dq/bw/cd) used to name the builtin
def ext_of(cps):
    cps=set(cps)
    if "AVX512DQ" in cps: return "dq"
    if "AVX512BW" in cps: return "bw"
    if "AVX512CD" in cps: return "cd"
    return "f"
def cps_of(it): return {c.text for c in it.findall("CPUID")}

# ---- binding lookup ----
def vreg(w): return {512:"ZMM",256:"YMM",128:"XMM"}[w]
def fullmem(w): return {512:"M512",256:"M256",128:"M128"}[w]

def pick_load(mnem, width, masking):
    """reg <- mem.  unmasked: res=Res(reg), [RM_rm(mem)].  maskz: +Mask, ~z.
    merge: res=Arg[0], [RM_r(src), RM_rm(mem), Mask]."""
    want_mask = masking in ("mask","maskz"); want_merge = masking=="mask"
    vr,fm = vreg(width),fullmem(width)
    for b in by_key.get(mnem.upper(), []):
        if b.width!=width or b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        # the loaded-from operand is the RM_rm that allows memory; result is reg
        if rm and vr in rm[0].locs and fm in rm[0].locs:
            return b
    return None

def pick_store(mnem, width, masking):
    """mem <- reg, the *pure-store* form (memory marked read so it is an arg, not
    a result): res=Res_none, args=[RM_rm(mem-only), RM_r(reg)(, Mask)]. The
    Isimd_mem store path forms the address from the leading mem arg."""
    want_mask = masking=="mask"
    fm = fullmem(width); vr = vreg(width)
    for b in by_key.get(mnem.upper(), []):
        if b.width!=width or b.merge or b.res_kind!="none": continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and fm in rm[0].locs and vr not in rm[0].locs \
           and any(a.enc=="RM_r" for a in b.args):
            return b
    return None

# Masked load/store works: the writemask arg is declared (mask[@unboxed]) so it
# is passed in a K register (without it the mask is boxed and the box pointer is
# wrongly used as the EVEX writemask).
EMIT_MASKED = True
SKIP_WIDTHS = set()

# ---- emitted records ----
emitted=[]
seen=set()

def add(rec):
    if rec["caml"] in seen: return
    seen.add(rec["caml"]); emitted.append(rec)

for name, it in sorted(intr.items()):
    m=LS_RE.match(name)
    if not m: continue
    width=width_of(m.group(1))
    if width in SKIP_WIDTHS: continue
    masking={"mask_":"mask","maskz_":"maskz",None:"none"}[m.group(2)]
    if masking!="none" and not EMIT_MASKED: continue
    op=m.group(3); tag=m.group(4)
    is_load = op in ("load","loadu")
    aligned = op in ("load","store")
    if tag in SI:
        if masking!="none": continue   # si* has no masked form
        base,bits = "int",64
        mnem = "VMOVDQA64" if aligned else "VMOVDQU64"
        vt = vt_of("int",64,width)
        cty = {512:"__m512i",256:"__m256i",128:"__m128i"}[width]
    else:
        base,bits,isf,umn,amn = ELT[tag]
        mnem = amn if aligned else umn
        if mnem is None: continue       # no aligned byte/word move
        vt = vt_of(base,bits,width)
        cty = None  # filled below
    b = (pick_load if is_load else pick_store)(mnem, width, masking)
    if b is None: continue
    # descriptive name: caml_avx512_<vt>_<op>[_mask|_maskz]
    suff = {"mask":"_mask","maskz":"_maskz","none":""}[masking]
    caml=f"caml_avx512_{vt}_{op}{suff}"
    add(dict(intel=name, caml=caml, binding=b, width=width, vt=vt, base=base,
             bits=bits, lanes=width//bits, is_load=is_load, masking=masking,
             tag=tag, aligned=aligned, ext=ext_of(cps_of(it))))

# ---- broadcasting loads: load a small chunk from memory and broadcast it to
# fill the whole vector (vbroadcast* / vpbroadcast*).  Tested against the Intel
# broadcast intrinsic composed with a plain load of the same chunk. ----
# (suffix, mnemonic, elt_base, elt_bits, [widths], C-oracle template)
BROADCASTS = [
  ("f32x4","VBROADCASTF32X4","float",32,[256,512],"_mm{W}_broadcast_f32x4(_mm_loadu_ps((float const*)a))"),
  ("f64x2","VBROADCASTF64X2","float",64,[256,512],"_mm{W}_broadcast_f64x2(_mm_loadu_pd((double const*)a))"),
  ("i32x4","VBROADCASTI32X4","int",32,[256,512],"_mm{W}_broadcast_i32x4(_mm_loadu_si128((void const*)a))"),
  ("i64x2","VBROADCASTI64X2","int",64,[256,512],"_mm{W}_broadcast_i64x2(_mm_loadu_si128((void const*)a))"),
  ("f32x8","VBROADCASTF32X8","float",32,[512],"_mm512_broadcast_f32x8(_mm256_loadu_ps((float const*)a))"),
  ("f64x4","VBROADCASTF64X4","float",64,[512],"_mm512_broadcast_f64x4(_mm256_loadu_pd((double const*)a))"),
  ("i32x8","VBROADCASTI32X8","int",32,[512],"_mm512_broadcast_i32x8(_mm256_loadu_si256((void const*)a))"),
  ("i64x4","VBROADCASTI64X4","int",64,[512],"_mm512_broadcast_i64x4(_mm256_loadu_si256((void const*)a))"),
  ("d","VPBROADCASTD","int",32,[128,256,512],"_mm{W}_set1_epi32(*(int32_t const*)a)"),
  ("q","VPBROADCASTQ","int",64,[128,256,512],"_mm{W}_set1_epi64x(*(int64_t const*)a)"),
]
def pick_broadcast(mnem, width):
    vr=vreg(width)
    for b in by_key.get(mnem.upper(), []):
        if b.merge or any(a.enc=="Mask" for a in b.args): continue
        if not (b.res_kind=="res" and b.res_locs and vr in b.res_locs[0].locs): continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and any(l.startswith("M") for l in rm[0].locs): return b
    return None
for suf,mnem,base,bits,widths,otmpl in BROADCASTS:
    for width in widths:
        b=pick_broadcast(mnem,width)
        if b is None: continue
        vt=vt_of(base,bits,width)
        caml=f"caml_avx512_{vt}_broadcast_{suf}"
        oracle=otmpl.replace("_mm{W}_","_mm"+{512:"512",256:"256",128:""}[width]+"_")
        # _mm512_set1_epi64 has no trailing 'x' (unlike the 128/256 forms)
        oracle=oracle.replace("_mm512_set1_epi64x","_mm512_set1_epi64")
        add(dict(intel=oracle, caml=caml, binding=b, width=width, vt=vt, base=base,
                 bits=bits, lanes=width//bits, is_load=True, masking="none",
                 tag=None, aligned=False, bcast=True, oracle_c=oracle,
                 ext=ext_of(b.exts)))

# ---- expandloadu: masked load that expands the masked lanes from contiguous
# memory (vpexpand*/vexpand* with a memory source).  Reuses the masked-load path. ----
EXPAND_MNEM={"epi8":"VPEXPANDB","epi16":"VPEXPANDW","epi32":"VPEXPANDD",
    "epi64":"VPEXPANDQ","ps":"VEXPANDPS","pd":"VEXPANDPD"}
EXPLOAD_RE=re.compile(r'^_mm(512|256|)_(mask_|maskz_)?expandloadu_'
    r'(epi8|epi16|epi32|epi64|ps|pd)$')
ELT_BB={"epi8":("int",8),"epi16":("int",16),"epi32":("int",32),"epi64":("int",64),
    "ps":("float",32),"pd":("float",64)}
for name, it in sorted(intr.items()):
    m=EXPLOAD_RE.match(name)
    if not m: continue
    width=width_of(m.group(1))
    masking={"mask_":"mask","maskz_":"maskz",None:"none"}[m.group(2)]
    if masking=="none": continue   # expandloadu is always masked
    tag=m.group(3); base,bits=ELT_BB[tag]; vt=vt_of(base,bits,width)
    b=pick_load(EXPAND_MNEM[tag], width, masking)
    if b is None: continue
    suff={"mask":"_mask","maskz":"_maskz"}[masking]
    add(dict(intel=name, caml=f"caml_avx512_{vt}_expandloadu{suff}", binding=b,
             width=width, vt=vt, base=base, bits=bits, lanes=width//bits,
             is_load=True, masking=masking, tag=tag, aligned=False,
             ext=ext_of(cps_of(it))))

# ---- compressstoreu: masked store of the compressed (mask-selected) lanes to
# contiguous memory (vpcompress*/vcompressp* pure-store form).  Mask form only. ----
COMPRESS_MNEM={"epi32":"VPCOMPRESSD","epi64":"VPCOMPRESSQ","ps":"VCOMPRESSPS",
    "pd":"VCOMPRESSPD"}
CMPSTORE_RE=re.compile(r'^_mm(512|256|)_mask_compressstoreu_(epi32|epi64|ps|pd)$')
for name, it in sorted(intr.items()):
    m=CMPSTORE_RE.match(name)
    if not m: continue
    width=width_of(m.group(1)); tag=m.group(2); base,bits=ELT_BB[tag]
    vt=vt_of(base,bits,width)
    b=pick_store(COMPRESS_MNEM[tag], width, "mask")
    if b is None: continue
    add(dict(intel=name, caml=f"caml_avx512_{vt}_compressstoreu_mask", binding=b,
             width=width, vt=vt, base=base, bits=bits, lanes=width//bits,
             is_load=False, masking="mask", tag=tag, aligned=False,
             ext=ext_of(cps_of(it))))

# ---- cvt + store: down-convert a wide int vector to narrower elements and store
# the narrow result to memory (vpmov{,s,us}{qd,qw,qb,dw,db,wb} pure-store form,
# always masked).  The RM_r operand is the WIDE source and the RM_rm memory
# operand the NARROW destination, so a dedicated picker is used. ----
CVT_SRC={"epi64":(64,"q"),"epi32":(32,"d"),"epi16":(16,"w")}
CVT_DST={"epi32":(32,"d"),"epi16":(16,"w"),"epi8":(8,"b")}
CVT_SAT={"cvt":"","cvts":"s","cvtus":"us"}
CVT_SATSUF={"":"","s":"_saturating","us":"_saturating_unsigned"}
CVTSTORE_RE=re.compile(r'^_mm(512|256|)_mask_(cvt|cvts|cvtus)(epi64|epi32|epi16)'
    r'_storeu_(epi32|epi16|epi8)$')
def pick_cvt_store(mnem, src_width):
    vr=vreg(src_width)
    for b in by_key.get(mnem.upper(), []):
        if b.width!=src_width or b.merge or b.res_kind!="none": continue
        if not any(a.enc=="Mask" for a in b.args): continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        rr=[a for a in b.args if a.enc=="RM_r"]
        if rm and rr and any(l.startswith("M") for l in rm[0].locs) and vr in rr[0].locs:
            return b
    return None
for name, it in sorted(intr.items()):
    m=CVTSTORE_RE.match(name)
    if not m: continue
    src_width=width_of(m.group(1)); sat=CVT_SAT[m.group(2)]
    src_bits,srcL=CVT_SRC[m.group(3)]; dst_bits,dstL=CVT_DST[m.group(4)]
    if dst_bits>=src_bits: continue          # down-converts only
    lanes=src_width//src_bits                 # 1:1 lane mapping == writemask width
    b=pick_cvt_store("vpmov"+sat+srcL+dstL, src_width)
    if b is None: continue
    srcvt=vt_of("int",src_bits,src_width); dstvt=f"int{dst_bits}x{lanes}"
    caml=f"caml_avx512_cvt_{srcvt}_{dstvt}{CVT_SATSUF[sat]}_storeu_mask"
    add(dict(intel=name, caml=caml, binding=b, width=src_width, vt=srcvt,
             base="int", bits=src_bits, lanes=lanes, is_load=False,
             masking="mask", tag=m.group(3), aligned=False, ext=ext_of(cps_of(it))))

# ---- gather / scatter: masked VSIB (base + index-vector*scale) load / store.
# Lower like the AVX2 gathers: extract_scale + Iindexed2scaled addressing (the
# arms are hand-shaped, not via the load/store helper), with a K writemask.  The
# 512-bit prefix names the larger of {index,data}; #lanes = P/max(dbits,idxbits). ---
LETV={128:"X",256:"Y",512:"Z"}
GSBASE={"epi32":("int",32),"epi64":("int",64),"ps":("float",32),"pd":("float",64)}
GMNEM={(32,"epi32"):"VPGATHERDD",(32,"epi64"):"VPGATHERDQ",(32,"ps"):"VGATHERDPS",
    (32,"pd"):"VGATHERDPD",(64,"epi32"):"VPGATHERQD",(64,"epi64"):"VPGATHERQQ",
    (64,"ps"):"VGATHERQPS",(64,"pd"):"VGATHERQPD"}
SMNEM={(32,"epi32"):"VPSCATTERDD",(32,"epi64"):"VPSCATTERDQ",(32,"ps"):"VSCATTERDPS",
    (32,"pd"):"VSCATTERDPD",(64,"epi32"):"VPSCATTERQD",(64,"epi64"):"VPSCATTERQQ",
    (64,"ps"):"VSCATTERQPS",(64,"pd"):"VSCATTERQPD"}
GATHER_RE=re.compile(r'^_mm(512|256|)_(mask|mmask)_i(32|64)gather_(epi32|epi64|ps|pd)$')
SCAT_RE=re.compile(r'^_mm(512|256|)_mask_i(32|64)scatter_(epi32|epi64|ps|pd)$')
def pick_gs(mnem, vwidth, memloc, gather):
    for b in by_key.get(mnem.upper(), []):
        if b.merge or b.res_kind!=("res" if gather else "none"): continue
        if not any(a.enc=="Mask" for a in b.args): continue
        rmr=[a for a in b.args if a.enc=="RM_r"]
        rmm=[a for a in b.args if a.enc=="RM_rm"]
        if rmr and rmm and vreg(vwidth) in rmr[0].locs and memloc in rmm[0].locs:
            return b
    return None
def gs_dims(P, idxbits, dt):
    base,dbits=GSBASE[dt]; nl=P//max(dbits,idxbits)
    destw=nl*dbits; idxw=min(512,max(128,nl*idxbits))
    return base,dbits,nl,destw,idxw,vt_of(base,dbits,destw),vt_of("int",idxbits,idxw)
for name, it in sorted(intr.items()):
    m=GATHER_RE.match(name)
    if not m: continue
    P=width_of(m.group(1)); idxbits=int(m.group(3)); dt=m.group(4)
    base,dbits,nl,destw,idxw,outvt,idxvt=gs_dims(P, idxbits, dt)
    if destw<128: continue   # sub-128 result (e.g. 2xi32) is not a vec type
    memloc=f"VM{idxbits}{LETV[idxw]}"
    b=pick_gs(GMNEM[(idxbits,dt)], destw, memloc, gather=True)
    if b is None: continue
    add(dict(intel=name, caml=f"caml_avx512_{outvt}_i{idxbits}gather_mask", binding=b,
             width=destw, vt=outvt, base=base, bits=dbits, lanes=nl, is_load=True,
             masking="mask", gather=True, idxvt=idxvt, idxbits=idxbits,
             idxlanes=idxw//idxbits, scale=dbits//8, bufelts=64//(dbits//8), ext="f"))
for name, it in sorted(intr.items()):
    m=SCAT_RE.match(name)
    if not m: continue
    P=width_of(m.group(1)); idxbits=int(m.group(2)); dt=m.group(3)
    base,dbits,nl,destw,idxw,valvt,idxvt=gs_dims(P, idxbits, dt)
    if destw<128: continue   # sub-128 value (e.g. 2xi32) is not a vec type
    memloc=f"VM{idxbits}{LETV[idxw]}"
    b=pick_gs(SMNEM[(idxbits,dt)], destw, memloc, gather=False)
    if b is None: continue
    add(dict(intel=name, caml=f"caml_avx512_{valvt}_i{idxbits}scatter_mask", binding=b,
             width=destw, vt=valvt, base=base, bits=dbits, lanes=nl, is_load=False,
             masking="mask", scatter=True, idxvt=idxvt, idxbits=idxbits,
             idxlanes=idxw//idxbits, scale=dbits//8, ext="f"))

# ---- load / store a mask register to / from memory (kmov k,m32 / kmov m32,k).
# The mask is the data (not a writemask); compared via int64_of_mask. ----
KMASK_RE=re.compile(r'^_(load|store)_mask(8|16|32|64)$')
for name, it in sorted(intr.items()):
    m=KMASK_RE.match(name)
    if not m: continue
    is_load=(m.group(1)=="load"); W=int(m.group(2)); sz={8:"b",16:"w",32:"d",64:"q"}[W]
    bd=binds.get(f"kmov{sz}_K_Km{W}" if is_load else f"kmov{sz}_m{W}_K")
    if bd is None: continue
    add(dict(intel=name, caml=f"caml_avx512_{'load' if is_load else 'store'}_mask{W}",
             binding=bd, width=W, vt=f"mask{W}", base="mask", bits=W, lanes=W,
             is_load=is_load, masking="none", kmask=True, aligned=False,
             ext=ext_of(cps_of(it))))

# encode the AVX512 feature subset in each builtin name (caml_avx512<ext>_...)
for e in emitted:
    e["caml"] = e["caml"].replace("caml_avx512_", f"caml_avx512{e['ext']}_", 1)

# ===================== EMIT: selection arms =====================
def emit_arm(e):
    b=e["binding"]
    if e.get("gather"):
        return (f'    | "{e["caml"]}" ->\n'
                f'      let i, args = extract_scale args op in\n'
                f'      simd_load ~mode:(Iindexed2scaled (i, 0)) {b.name} args')
    if e.get("scatter"):
        return (f'    | "{e["caml"]}" ->\n'
                f'      let i, args = extract_scale args op in\n'
                f'      simd_store ~mode:(Iindexed2scaled (i, 0)) {b.name} args')
    if e["is_load"]:
        # maskz load zeroes masked lanes (~z:true); plain and merge (_K_merge,
        # which fixes z) loads take no ~z.
        bexpr = f"({b.name} ~z:true)" if e["masking"]=="maskz" else b.name
        return f'    | "{e["caml"]}" -> load {bexpr}'
    # pure-store bindings (masked or not) take no ~z parameter.
    return f'    | "{e["caml"]}" -> store {b.name}'

# ===================== EMIT: ML test + C stubs =====================
def width_bits(vt):
    m=re.match(r'[a-z]+(\d+)x(\d+)', vt); return int(m.group(1))*int(m.group(2))

def vt_nl(vt): return width_bits(vt)//64

def emit_vt_helpers(vt):
    w=width_bits(vt); nl=w//64
    s=f'external {vt}_of_bits : '+("int64 -> "*nl)+f'{vt} = "" "vec{w}_of_int64s" [@@noalloc] [@@unboxed]\n'
    for i in range(nl):
        s+=f'external {vt}_w{i} : {vt} -> int64 = "" "vec{w}_w{i}" [@@noalloc] [@@unboxed]\n'
    s+=f'let lanes_{vt} v = [| '+"; ".join(f"{vt}_w{i} v" for i in range(nl))+" |]\n"
    return s

# Each load returns a vector; each store mutates a buffer we then read back.
def ml_id(e): return e["caml"][len("caml_"):]
def c_id(e): return "c_"+ml_id(e)

def emit_external(e):
    vt=e["vt"]
    if e.get("kmask"):
        if e["is_load"]:
            return (f'external {ml_id(e)} : addr -> (mask[@unboxed])\n'
                    f'  = "caml_vec512_unreachable" "{e["caml"]}"\n[@@noalloc] [@@builtin]\n'
                    f'external {c_id(e)} : addr -> (int64[@unboxed])\n'
                    f'  = "" "{c_id(e)}"\n[@@noalloc]\n')
        return (f'external {ml_id(e)} : addr -> (mask[@unboxed]) -> void\n'
                f'  = "caml_vec512_unreachable" "{e["caml"]}"\n[@@noalloc] [@@builtin]\n'
                f'external {c_id(e)} : addr -> (int64[@unboxed]) -> (int[@untagged])\n'
                f'  = "" "{c_id(e)}"\n[@@noalloc]\n')
    if e.get("gather"):
        iv=e["idxvt"]
        tys=(f'int64# -> ({vt}[@unboxed]) -> addr -> ({iv}[@unboxed]) '
             f'-> (mask[@unboxed]) -> ({vt}[@unboxed])')
        cty=(f'({vt}[@unboxed]) -> (int64[@unboxed]) -> ({iv}[@unboxed]) '
             f'-> addr -> ({vt}[@unboxed])')
        return (f'external {ml_id(e)} : {tys}\n  = "caml_vec512_unreachable" "{e["caml"]}"\n'
                f'[@@noalloc] [@@builtin]\n'
                f'external {c_id(e)} : {cty}\n  = "" "{c_id(e)}"\n[@@noalloc]\n')
    if e.get("scatter"):
        iv=e["idxvt"]
        tys=(f'int64# -> addr -> ({iv}[@unboxed]) -> ({vt}[@unboxed]) '
             f'-> (mask[@unboxed]) -> void')
        cty=(f'addr -> ({iv}[@unboxed]) -> ({vt}[@unboxed]) '
             f'-> (int64[@unboxed]) -> (int[@untagged])')
        return (f'external {ml_id(e)} : {tys}\n  = "caml_vec512_unreachable" "{e["caml"]}"\n'
                f'[@@noalloc] [@@builtin]\n'
                f'external {c_id(e)} : {cty}\n  = "" "{c_id(e)}"\n[@@noalloc]\n')
    if e["is_load"]:
        if e["masking"]=="none":
            tys=f'addr -> ({vt}[@unboxed])'
            cty=f'addr -> ({vt}[@unboxed])'
        elif e["masking"]=="maskz":
            # the writemask must be unboxed (passed in a K register); without the
            # explicit [@unboxed] it is boxed and the box pointer is (wrongly)
            # used as the EVEX writemask.
            tys=f'addr -> (mask[@unboxed]) -> ({vt}[@unboxed])'
            cty=f'addr -> (int64[@unboxed]) -> ({vt}[@unboxed])'
        else: # mask (merge): src addr mask
            tys=f'({vt}[@unboxed]) -> addr -> (mask[@unboxed]) -> ({vt}[@unboxed])'
            cty=f'({vt}[@unboxed]) -> addr -> (int64[@unboxed]) -> ({vt}[@unboxed])'
    else:
        # the builtin returns void; the C oracle (a plain external) cannot return
        # void, so it returns an ignored untagged int -- its effect is the store.
        if e["masking"]=="none":
            tys=f'addr -> ({vt}[@unboxed]) -> void'
            cty=f'addr -> ({vt}[@unboxed]) -> (int[@untagged])'
        else: # mask store: addr vec mask
            tys=f'addr -> ({vt}[@unboxed]) -> (mask[@unboxed]) -> void'
            cty=f'addr -> ({vt}[@unboxed]) -> (int64[@unboxed]) -> (int[@untagged])'
    s =(f'external {ml_id(e)} : {tys}\n  = "caml_vec512_unreachable" "{e["caml"]}"\n'
        f'[@@noalloc] [@@builtin]\n')
    s+=(f'external {c_id(e)} : {cty}\n  = "" "{c_id(e)}"\n[@@noalloc]\n')
    return s

def emit_c_oracle(e):
    if e.get("kmask"):
        mm=MMASK[e["lanes"]]
        if e["is_load"]:
            return (f'int64_t {c_id(e)}(intnat a)\n{{\n'
                    f'    return (int64_t){e["intel"]}((void const*)a);\n}}\n')
        return (f'intnat {c_id(e)}(intnat a, int64_t k)\n{{\n'
                f'    {e["intel"]}((void*)a, ({mm})k); return 0;\n}}\n')
    vt=e["vt"]; cty=CVT[vt]; n=vt_nl(vt)
    addrcast="(void const*)" if e["is_load"] else "(void*)"
    if e.get("gather"):
        ict=CVT[e["idxvt"]]; mm=MMASK[e["lanes"]]
        return (f'{cty} {c_id(e)}({cty} src, int64_t k, {ict} vindex, intnat a)\n{{\n'
                f'    return {e["intel"]}(src, ({mm})k, vindex, (void const*)a, {e["scale"]});\n}}\n')
    if e.get("scatter"):
        ict=CVT[e["idxvt"]]; mm=MMASK[e["lanes"]]
        return (f'intnat {c_id(e)}(intnat a, {ict} idx, {cty} v, int64_t k)\n{{\n'
                f'    {e["intel"]}((void*)a, ({mm})k, idx, v, {e["scale"]}); return 0;\n}}\n')
    if e.get("bcast"):
        # the oracle template already embeds the load+broadcast; `a` is the addr
        return f'{cty} {c_id(e)}(intnat a)\n{{\n    return {e["oracle_c"]};\n}}\n'
    if e["is_load"]:
        if e["masking"]=="none":
            body=f'return {e["intel"]}({addrcast}a);'
            sig=f'{cty} {c_id(e)}(intnat a)'
        elif e["masking"]=="maskz":
            body=f'return {e["intel"]}(({MMASK[e["lanes"]]})k, {addrcast}a);'
            sig=f'{cty} {c_id(e)}(intnat a, int64_t k)'
        else:
            body=f'return {e["intel"]}(src, ({MMASK[e["lanes"]]})k, {addrcast}a);'
            sig=f'{cty} {c_id(e)}({cty} src, intnat a, int64_t k)'
    else:
        if e["masking"]=="none":
            body=f'{e["intel"]}({addrcast}a, v); return 0;'
            sig=f'intnat {c_id(e)}(intnat a, {cty} v)'
        else:
            body=f'{e["intel"]}({addrcast}a, ({MMASK[e["lanes"]]})k, v); return 0;'
            sig=f'intnat {c_id(e)}(intnat a, {cty} v, int64_t k)'
    return f'{sig}\n{{\n    {body}\n}}\n'

CVT={}      # vt -> C type
MMASK={}    # lanes -> __mmaskN

def setup_types():
    for e in emitted:
        if e.get("kmask"):  # mask<->memory: only the __mmaskN cast is needed
            MMASK[e["lanes"]]="__mmask8" if e["lanes"]<=8 else f"__mmask{e['lanes']}"
            continue
        vt=e["vt"]; base=e["base"]; bits=e["bits"]; w=e["width"]
        if base=="float" and bits==32: CVT[vt]={512:"__m512",256:"__m256",128:"__m128"}[w]
        elif base=="float" and bits==64: CVT[vt]={512:"__m512d",256:"__m256d",128:"__m128d"}[w]
        else: CVT[vt]={512:"__m512i",256:"__m256i",128:"__m128i"}[w]
        MMASK[e["lanes"]]= "__mmask8" if e["lanes"]<=8 else f"__mmask{e['lanes']}"
        if e.get("idxvt"):  # gather/scatter index vector is always an int vector
            CVT[e["idxvt"]]={512:"__m512i",256:"__m256i",128:"__m128i"}[width_bits(e["idxvt"])]

def _idx_words(idxvt, idxbits, idxlanes, hi):
    """random in-range indices packed into the index vector's int64 words; each
    index is masked into [0, hi) (hi a power of two)."""
    iw=width_bits(idxvt)//64
    if idxbits==64:
        return " ".join(f'(Int64.logand (rand_bits ()) {hi-1}L)' for _ in range(iw))
    m=((hi-1)<<32)|(hi-1)
    return " ".join(f'(Int64.logand (rand_bits ()) 0x{m:X}L)' for _ in range(iw))

def emit_test(e):
    """Buffer-based bit-exact check. The buffer is filled with the int64x{N}
    storeu builtin (its correctness is irrelevant: both the load builtin and the
    oracle read the same bytes); stores are read back with the C [mem_w] helper,
    independent of any builtin."""
    if e.get("kmask"):
        if e["is_load"]:
            L=[f'    let mem = aligned_alloc ~align:#64n ~size:#64n in',
               f'    let fillv = int64x8_of_bits '+" ".join("(rand_bits ())" for _ in range(8))+' in',
               f'    let _ = avx512f_int64x8_storeu mem fillv in',
               f'    let g = int64_of_mask ({ml_id(e)} mem) in',
               f'    let x = {c_id(e)} mem in',
               f'    cmp1 "{e["caml"]}" g x']
        else:
            L=[f'    let m1 = aligned_alloc ~align:#64n ~size:#16n in',
               f'    let m2 = aligned_alloc ~align:#64n ~size:#16n in',
               f'    let pre = int64x2_of_bits (rand_bits ()) (rand_bits ()) in',
               f'    let _ = avx512f_int64x2_storeu m1 pre in',
               f'    let _ = avx512f_int64x2_storeu m2 pre in',
               f'    let k = rand_mask {e["lanes"]} in',
               f'    let _ = {ml_id(e)} m1 (mask_of_int64 k) in',
               f'    let _ = {c_id(e)} m2 k in',
               f'    cmp_buf "{e["caml"]}" m1 m2 2']
        return "let () =\n  for _i = 1 to iters do\n"+"\n".join(L)+"\n  done\n"
    if e.get("gather"):
        vt=e["vt"]; nl=vt_nl(vt); iv=e["idxvt"]
        idxw=_idx_words(iv, e["idxbits"], e["idxlanes"], e["bufelts"])
        L=[f'    let mem = aligned_alloc ~align:#64n ~size:#64n in',
           f'    let fillv = int64x8_of_bits '+" ".join("(rand_bits ())" for _ in range(8))+' in',
           f'    let _ = avx512f_int64x8_storeu mem fillv in',
           f'    let src = {vt}_of_bits '+" ".join("(rand_bits ())" for _ in range(nl))+' in',
           f'    let vindex = {iv}_of_bits {idxw} in',
           f'    let k = rand_mask {e["lanes"]} in',
           f'    let g = lanes_{vt} ({ml_id(e)} #{e["scale"]}L src mem vindex (mask_of_int64 k)) in',
           f'    let x = lanes_{vt} ({c_id(e)} src k vindex mem) in',
           f'    cmp "{e["caml"]}" g x']
        return "let () =\n  for _i = 1 to iters do\n"+"\n".join(L)+"\n  done\n"
    if e.get("scatter"):
        vt=e["vt"]; nl=vt_nl(vt); iv=e["idxvt"]; w=width_bits(vt); wq=w//64
        bufelts=(w//8)//e["scale"]   # indices stay within the value-width buffer
        idxw=_idx_words(iv, e["idxbits"], e["idxlanes"], bufelts)
        L=[f'    let m1 = aligned_alloc ~align:#64n ~size:#{w//8}n in',
           f'    let m2 = aligned_alloc ~align:#64n ~size:#{w//8}n in',
           f'    let pre = int64x{wq}_of_bits '+" ".join("(rand_bits ())" for _ in range(wq))+' in',
           f'    let _ = avx512f_int64x{wq}_storeu m1 pre in',
           f'    let _ = avx512f_int64x{wq}_storeu m2 pre in',
           f'    let v = {vt}_of_bits '+" ".join("(rand_bits ())" for _ in range(nl))+' in',
           f'    let idx = {iv}_of_bits {idxw} in',
           f'    let k = rand_mask {e["lanes"]} in',
           f'    let _ = {ml_id(e)} #{e["scale"]}L m1 idx v (mask_of_int64 k) in',
           f'    let _ = {c_id(e)} m2 idx v k in',
           f'    cmp_buf "{e["caml"]}" m1 m2 {wq}']
        return "let () =\n  for _i = 1 to iters do\n"+"\n".join(L)+"\n  done\n"
    vt=e["vt"]; nl=vt_nl(vt); w=width_bits(vt); bytes_=w//8
    fill=f"avx512f_int64x{w//64}_storeu"
    rb=lambda c: f'let {c} = {vt}_of_bits '+" ".join("(rand_bits ())" for _ in range(nl))+' in'
    L=[]
    if e["is_load"]:
        L.append(f'    let mem = aligned_alloc ~align:#64n ~size:#{bytes_}n in')
        L.append(f'    let fillv = int64x{w//64}_of_bits '+" ".join("(rand_bits ())" for _ in range(w//64))+' in')
        L.append(f'    let _ = {fill} mem fillv in')
        if e["masking"]=="none":
            L.append(f'    let g = lanes_{vt} ({ml_id(e)} mem) in')
            L.append(f'    let x = lanes_{vt} ({c_id(e)} mem) in')
        elif e["masking"]=="maskz":
            L.append(f'    let k = rand_mask {e["lanes"]} in')
            L.append(f'    let g = lanes_{vt} ({ml_id(e)} mem (mask_of_int64 k)) in')
            L.append(f'    let x = lanes_{vt} ({c_id(e)} mem k) in')
        else:
            L.append(f'    let k = rand_mask {e["lanes"]} in')
            L.append(f'    {rb("src")}')
            L.append(f'    let g = lanes_{vt} ({ml_id(e)} src mem (mask_of_int64 k)) in')
            L.append(f'    let x = lanes_{vt} ({c_id(e)} src mem k) in')
        L.append(f'    cmp "{e["caml"]}" g x;')
    else:
        L.append(f'    let m1 = aligned_alloc ~align:#64n ~size:#{bytes_}n in')
        L.append(f'    let m2 = aligned_alloc ~align:#64n ~size:#{bytes_}n in')
        L.append(f'    let pre = int64x{w//64}_of_bits '+" ".join("(rand_bits ())" for _ in range(w//64))+' in')
        L.append(f'    let _ = avx512f_int64x{w//64}_storeu m1 pre in')
        L.append(f'    let _ = avx512f_int64x{w//64}_storeu m2 pre in')
        L.append(f'    {rb("v")}')
        if e["masking"]=="none":
            L.append(f'    let _ = {ml_id(e)} m1 v in')
            L.append(f'    let _ = {c_id(e)} m2 v in')
        else:
            L.append(f'    let k = rand_mask {e["lanes"]} in')
            L.append(f'    let _ = {ml_id(e)} m1 v (mask_of_int64 k) in')
            L.append(f'    let _ = {c_id(e)} m2 v k in')
        L.append(f'    cmp_buf "{e["caml"]}" m1 m2 {nl};')
    L[-1]=L[-1][:-1]   # drop trailing ';'
    body="\n".join(L)
    return f"let () =\n  for _i = 1 to iters do\n{body}\n  done\n"

# The vecNNN_w* / vecNNN_of_int64s helpers are already provided by stubs512.c
# (512-bit) and ops512_gen_stubs.c (256/128-bit) in the same archive; only the
# buffer helpers are unique to this file.
CVEC_HELPERS = r'''
#include <caml/simd.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <stdlib.h>
intnat vec_aligned_alloc(intnat align, intnat size) {
    void* p = NULL;
    if (posix_memalign(&p, (size_t)align, (size_t)size)) return 0;
    return (intnat)p;
}
int64_t mem_w(intnat a, intnat i){ int64_t* p=(int64_t*)a; return p[i]; }
'''

ML_PRELUDE = '''open Stdlib
[@@@ocaml.warning "-32-26-27"]

(* GENERATED by tools/avx512gen/genmem.py -- do not edit by hand.
   Each OxCaml AVX512 memory builtin is compared bit-for-bit against the same
   Intel intrinsic, acting on a shared aligned buffer.  Success prints nothing. *)

let iters = %d
type void : void
type addr = nativeint#

external aligned_alloc : align:nativeint# -> size:nativeint# -> addr
  = "" "vec_aligned_alloc"
external mem_w : addr -> (int[@untagged]) -> (int64[@unboxed]) = "" "mem_w" [@@noalloc]
external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64" [@@noalloc] [@@unboxed] [@@builtin]

let seed = ref 0x1234567
let rand_bits () =
  let next () = seed := (!seed * 1103515245 + 12345) land 0x3FFFFFFF; !seed in
  let a = Int64.of_int (next ()) and b = Int64.of_int (next ()) and c = Int64.of_int (next ()) in
  Int64.logxor (Int64.shift_left a 34) (Int64.logxor (Int64.shift_left b 17) c)
let rand_mask lanes =
  let m = if lanes >= 64 then -1L else Int64.sub (Int64.shift_left 1L lanes) 1L in
  Int64.logand (rand_bits ()) m
let cmp name g e =
  Array.iteri (fun i x -> if Int64.compare x e.(i) <> 0 then
    Printf.printf "%%s lane %%d: got %%016Lx exp %%016Lx\\n" name i x e.(i)) g
let cmp_buf name a b nl =
  for i = 0 to nl - 1 do
    let x = mem_w a i and y = mem_w b i in
    if Int64.compare x y <> 0 then
      Printf.printf "%%s buf %%d: got %%016Lx exp %%016Lx\\n" name i x y
  done
'''

def write_files():
    setup_types()
    arms="\n".join(emit_arm(e) for e in emitted)
    txt=open(SELECTION).read()
    B="    (* BEGIN GENERATED AVX512 MEM *)\n"; E="    (* END GENERATED AVX512 MEM *)\n"
    i=txt.index(B)+len(B); j=txt.index(E)
    open(SELECTION,"w").write(txt[:i]+arms+"\n"+txt[j:])
    try: subprocess.run(["ocamlformat","-i",SELECTION], check=True, capture_output=True)
    except Exception as ex: print(f"warning: ocamlformat: {ex}")
    # C stubs: a BUILTIN() fallback symbol per builtin (the framework references
    # these in its symbol table; the recognized builtin is lowered inline so the
    # fallback body is never run), plus the C oracles.
    fallbacks="\n".join(f'BUILTIN({e["caml"]});' for e in emitted)
    oracles="\n".join(emit_c_oracle(e) for e in emitted)
    cfile=("/* GENERATED by tools/avx512gen/genmem.py -- do not edit. */\n"
        "#include <assert.h>\n"
        +CVEC_HELPERS+'\n#define BUILTIN(name) void name() { assert(0); }\n'
        +fallbacks+"\n#ifdef ARCH_AVX512\n#include <immintrin.h>\n"
        +oracles+"\n#endif\n")
    open(os.path.join(TESTDIR,"ops512_mem_stubs.c"),"w").write(cfile)
    # ML test
    used=sorted({e["vt"] for e in emitted if not e.get("kmask")}
                | {e["idxvt"] for e in emitted if e.get("idxvt")}
                | {"int64x8","int64x4","int64x2"}, key=width_bits)
    ml=(ML_PRELUDE % ITERS)+"\n"
    if any(e.get("kmask") for e in emitted):  # mask<->memory uses int64_of_mask/cmp1
        ml+=('external int64_of_mask : mask -> int64\n'
             '  = "caml_vec512_unreachable" "caml_int64_of_mask" [@@noalloc] [@@unboxed] [@@builtin]\n'
             'let cmp1 name g e =\n'
             '  if Int64.compare g e <> 0 then Printf.printf "%s: got %016Lx exp %016Lx\\n" name g e\n')
    ml+="".join(emit_vt_helpers(vt) for vt in used)+"\n"
    ml+="\n".join(emit_external(e) for e in emitted)+"\n"
    ml+="\n".join(emit_test(e) for e in emitted)
    open(os.path.join(TESTDIR,"ops512_mem.ml"),"w").write(ml)

if __name__=="__main__":
    print(f"matched {len(emitted)} memory intrinsics")
    from collections import Counter
    c=Counter((e["is_load"],e["masking"]) for e in emitted)
    for k,v in sorted(c.items()): print(f"  {'load' if k[0] else 'store':6s} {k[1]:6s}: {v}")
    if "--report" not in sys.argv:
        write_files()
        print("wrote select_operation_avx512_mem arms + ops512_mem.ml + ops512_mem_stubs.c")
