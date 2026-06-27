#!/usr/bin/env python3
"""AVX512 intrinsic generator for OxCaml.

Maps Intel Intrinsics Guide entries (the vendored intel_intrinsics.xml) onto the
OxCaml instruction bindings produced by tools/simdgen, and (re)generates:

  - the selection match arms spliced into backend/amd64/simd_selection.ml
    between the `(* BEGIN/END GENERATED AVX512 *)` markers;
  - oxcaml/tests/simd/avx512/ops512_gen.ml  -- per-intrinsic OCaml externals,
    a C-counterpart oracle external, and a bit-exact comparison test;
  - oxcaml/tests/simd/avx512/ops512_gen_stubs.c -- the C oracles (each calls the
    very same Intel intrinsic the OxCaml builtin lowers to) + BUILTIN fallbacks.

The descriptive names follow the existing avx2 convention
(caml_avx512_<type>_<op>[_maskz|_mask]), derived systematically from the Intel
name grammar with an op-vocabulary table.  Everything not handled yet is SKIPPED
with a logged reason (run with --report) so coverage is explicit -- no silent
caps.  See coverage.py for the Intel-vs-exposed completeness diff.

Usage:
  python3 generate.py            # regenerate all artifacts + splice arms
  python3 generate.py --report   # just print the emit/skip coverage summary
"""
import re, sys, os, collections, subprocess, xml.etree.ElementTree as ET
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.normpath(os.path.join(HERE, "..", ".."))
sys.path.insert(0, HERE)
import simdb

XML = os.path.join(HERE, "intel_intrinsics.xml")
SELECTION = os.path.join(REPO, "backend", "amd64", "simd_selection.ml")
TESTDIR = os.path.join(REPO, "oxcaml", "tests", "simd", "avx512")
TARGET_EXT = {"AVX512F","AVX512DQ","AVX512CD","AVX512BW","AVX512VL"}
ITERS = 200

# ---------- element-type / width -> OxCaml vector type ----------
SUFFIX = {  # tag -> (base, bits, is_float, is_unsigned)
    "pd":("float",64,True,False), "ps":("float",32,True,False),
    "epi8":("int",8,False,False), "epi16":("int",16,False,False),
    "epi32":("int",32,False,False), "epi64":("int",64,False,False),
    "epu8":("int",8,False,True), "epu16":("int",16,False,True),
    "epu32":("int",32,False,True), "epu64":("int",64,False,True),
}
WHOLE = {"si512":512,"si256":256,"si128":128}

def vectype(tag, width):
    if tag in WHOLE: return None
    base, bits, _, _ = SUFFIX[tag]
    return f"{base}{bits}x{width // bits}"

def c_vectype(tag, width):
    if tag in WHOLE: return {512:"__m512i",256:"__m256i",128:"__m128i"}[width]
    base, bits, isf, _ = SUFFIX[tag]
    if isf and bits==64: return {512:"__m512d",256:"__m256d",128:"__m128d"}[width]
    if isf and bits==32: return {512:"__m512",256:"__m256",128:"__m128"}[width]
    return {512:"__m512i",256:"__m256i",128:"__m128i"}[width]

def mask_lanes(tag, width):
    if tag in WHOLE: return None
    _, bits, _, _ = SUFFIX[tag]
    return width // bits

def width_of_vt(vt):
    base, lanes = vt.split('x'); lanes=int(lanes)
    return int(re.search(r'\d+', base).group()) * lanes

# ---------- op-core -> op-name (reuse avx2 vocabulary) ----------
OP = {
    "add":"add", "sub":"sub", "mul":"mul", "div":"div",
    "adds":"add_saturating", "subs":"sub_saturating",
    "mullo":"mul_low", "mulhi":"mul_high", "mulhrs":"mul_round",
    "min":"min", "max":"max", "abs":"abs", "avg":"avg_unsigned",
    "sqrt":"sqrt",
    "and":"and", "or":"or", "xor":"xor", "andnot":"andnot",
    # variable (per-lane count) shifts and rotates -- same element type, two
    # vector inputs.  Keep the Intel abbreviations to match avx2 (int32x8_sllv).
    "sllv":"sllv", "srlv":"srlv", "srav":"srav", "rolv":"rolv", "rorv":"rorv",
    # AVX512F float helpers (same type); rcp14/rsqrt14 are deterministic 14-bit
    # approximations, so the same-instruction oracle is still bit-exact.
    "getexp":"getexp", "scalef":"scalef", "rcp14":"rcp14", "rsqrt14":"rsqrt14",
    # AVX512CD
    "conflict":"conflict", "lzcnt":"lzcnt",
    # element duplication (unary, same type)
    "movedup":"duplicate_even", "moveldup":"duplicate_even",
    "movehdup":"duplicate_odd",
    # byte-granular shuffle by a vector control (vpshufb): uniform, no immediate.
    "shuffle":"shuffle",
    # --- immediate-operand ops (imm8); the C oracle bakes a few test values ---
    "ternarylogic":"ternarylogic", "alignr":"align_right",
    "dbsad":"dbsad",  # vdbpsadbw: block sum-of-abs-diff, two inputs + imm8
    # mask-driven pack/unpack (register forms): the mask selects lanes, only the
    # masked / zero-masked variants exist (an unmasked compress is meaningless).
    "compress":"compress", "expand":"expand",
    "shufflehi":"shuffle_high", "shufflelo":"shuffle_low",
    "slli":"slli", "srli":"srli", "srai":"srai", "rol":"rol", "ror":"ror",
    "roundscale":"roundscale", "getmant":"getmant", "range":"range",
    "reduce":"reduce",  # VREDUCEPD precision reduction (imm), not horizontal
    # NOTE: "mov" (masked copy) intentionally omitted -- it lowers to the
    # *aligned* VMOVDQA32/64, which faults if the register allocator spills the
    # source to an unaligned stack slot (observed as a SEGV in the test).
    # interleave (vpunpckh/lpckl): two same-type inputs, no immediate
    "unpackhi":"interleave_high", "unpacklo":"interleave_low",
    # within-lane permute by immediate (vpermilpd/ps, vpermq/pd cross-lane)
    "permute":"permute", "permutex":"permute_across",
    # NOTE: permutex2var is intentionally NOT here -- it lowers to vpermi2*/
    # vpermt2* whose operand order differs from the intrinsic's parameter order,
    # so the uniform "binding order == Intel order" assumption (and hence the
    # oracle mapping) is wrong; the bit-exact test catches it. Needs a dedicated
    # operand permutation to support.
}
ALWAYS_UNSIGNED = {"avg"}
# fused multiply-add cores -> OxCaml op name (mirrors the caml_fma_* vocabulary)
FMA={"fmadd":"mul_add","fmsub":"mul_sub","fnmadd":"neg_mul_add",
     "fnmsub":"neg_mul_sub","fmaddsub":"mul_addsub","fmsubadd":"mul_subadd"}

NAME_RE = re.compile(r'^_mm(512|256|)_(mask_|maskz_|mask3_|mask2_)?(.+)_'
    r'(pd|ps|epi8|epi16|epi32|epi64|epu8|epu16|epu32|epu64|si512|si256|si128)$')

def parse_name(name):
    m = NAME_RE.match(name)
    if not m: return None
    return dict(width={"512":512,"256":256,"":128}[m.group(1)],
                masking={"mask_":"mask","maskz_":"maskz","mask3_":"mask3",
                         "mask2_":"mask2",None:"none"}[m.group(2)],
                core=m.group(3), tag=m.group(4))

# ---------- load data ----------
binds, by_key = simdb.load()
root = ET.parse(XML).getroot()
intrinsics=[]
for it in root.findall("intrinsic"):
    cps={c.text for c in it.findall("CPUID")}
    if not cps or not cps <= TARGET_EXT: continue
    if it.get("tech")!="AVX-512": continue
    insts=[(i.get("name"),i.get("form")) for i in it.findall("instruction")]
    params=[(p.get("etype"),p.get("type"),p.get("varname"),p.get("memwidth"),p.get("immwidth"))
            for p in it.findall("parameter")]
    intrinsics.append(dict(name=it.get("name"),cps=cps,insts=insts,
        cat=it.findtext("category") or "", params=params))

# ---------- binding selection ----------
def full_mem(w): return {512:"M512",256:"M256",128:"M128"}[w]
def vreg(w): return {512:"ZMM",256:"YMM",128:"XMM"}[w]

def pick_binding(mnem, width, masking, want_imm=None):
    want_mask = masking in ("mask","maskz")
    want_merge = masking=="mask"
    fm, vr = full_mem(width), vreg(width)
    cands=[]
    for b in by_key.get(mnem.upper(), []):
        if b.width!=width or b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and vr in rm[0].locs and fm in rm[0].locs:
            cands.append(b)
    if want_imm is not None:  # prefer a binding whose immediate-ness matches
        for b in cands:
            if (b.imm in ("Imm_spec","Imm_reg"))==want_imm: return b
    return cands[0] if cands else None

def arg_roles(b):
    roles=[]
    for i,a in enumerate(b.args):
        if a.enc=="Mask": roles.append(("mask",a))
        # merge bindings always carry the prepended merge-source at index 0;
        # its encoding is RM_r for ModRM:reg dsts and Vex_v for imm-operand dsts.
        elif b.merge and i==0: roles.append(("merge",a))
        else: roles.append(("vec",a))
    return roles

def _norm(t): return t.replace("const","").replace(" ","")

def vec_input_types(it, masking):
    """C type strings of the vector INPUT params (excludes the merge-source for
    _mask, the writemask, and the immediate)."""
    start = 1 if masking=="mask" else 0
    out=[]
    for etype,typ,var,memw,immw in it["params"][start:]:
        if etype in ("MASK","IMM") or immw: continue
        out.append(_norm(typ))
    return out

def uniform(it, masking, want_ctype):
    return all(t==_norm(want_ctype) for t in vec_input_types(it, masking))

# ---------- converts ----------
ELSIZE={"epi8":8,"epi16":16,"epi32":32,"epi64":64,"epu8":8,"epu16":16,
        "epu32":32,"epu64":64,"ps":32,"pd":64,"ph":16}
CVT_RE=re.compile(r'^cvt(t?)(us|s|)(epi8|epi16|epi32|epi64|epu8|epu16|epu32|epu64|ps|pd|ph)$')

def parse_convert(core, dst_tag, prefix_w):
    m=CVT_RE.match(core)
    if not m or dst_tag not in ELSIZE: return None
    trunc, sat, src = m.group(1), m.group(2), m.group(3)
    if src=="ph" or dst_tag=="ph": return None  # FP16 out of scope
    in_el, out_el = ELSIZE[src], ELSIZE[dst_tag]
    lanes = prefix_w // max(in_el, out_el)
    in_w, out_w = lanes*in_el, lanes*out_el
    in_int = src.startswith("ep"); out_int = dst_tag.startswith("ep")
    in_uns = src.startswith("epu")
    if in_int and out_int:
        if in_el < out_el:
            flavor = "cvtzx" if in_uns else "cvtsx"; extra=""
        elif in_el > out_el:
            flavor="cvt"; extra={"s":"_saturating","us":"_saturating_unsigned"}.get(sat,"")
        else:
            return None  # same-size int->int convert: not a thing
    elif (not in_int) and out_int:
        flavor="cvtt" if trunc else "cvt"
        extra="_unsigned" if dst_tag.startswith("epu") else ""
    elif in_int and (not out_int):
        flavor="cvt"; extra="_unsigned" if in_uns else ""
    else:
        flavor="cvt"; extra=""
    in_vt=vectype(src,in_w); out_vt=vectype(dst_tag,out_w)
    return dict(flavor=flavor,extra=extra,in_tag=src,out_tag=dst_tag,
        in_w=in_w,out_w=out_w,in_vt=in_vt,out_vt=out_vt,lanes=lanes)

def loc_vw(locs):
    if "ZMM" in locs or "M512" in locs: return 512
    if "YMM" in locs or "M256" in locs: return 256
    if "XMM" in locs or "M128" in locs: return 128
    return None

def res_width(b):
    if b.res_kind=="res" and b.res_locs: return loc_vw(b.res_locs[0].locs)
    if b.res_kind=="arg": return loc_vw(b.args[b.res_locs[0]].locs)
    return None

def conv_input_width(b):
    for i,a in enumerate(b.args):
        if a.enc=="Mask": continue
        if b.merge and i==0: continue
        return loc_vw(a.locs)
    return None

def pick_convert_binding(mnem, masking, in_w, out_w, label=None):
    # in_w=None matches any input register width (used by up-converts, whose
    # input register may be wider than the consumed low lanes).
    want_mask = masking in ("mask","maskz"); want_merge = masking=="mask"
    for b in by_key.get(mnem.upper(), []):
        if b.merge != want_merge: continue
        # evex_b marks a broadcast memory form for plain converts, but is reused
        # as the rounding-control bit for the ~rnd/~sae register-only forms.
        if label is None and b.evex_b: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        if label is not None and label not in b.labels.split(): continue
        if label is None and ("~rnd" in b.labels or "~sae" in b.labels):
            continue  # plain form excludes the embedded-rounding/sae variants
        if res_width(b)==out_w and (in_w is None or conv_input_width(b)==in_w):
            return b
    return None

# ---------- mask-producing compares ----------
# Either the generic predicate-by-immediate form (cmp_<type>_mask), or a
# fixed-predicate convenience form (cmpeq/cmpgt/.../cmpneq_<type>_mask) whose
# predicate is baked into a constant immediate.
CMP_RE=re.compile(r'^_mm(512|256|)_(mask_)?cmp(eq|neq|nle|nlt|le|lt|ge|gt|ord|unord)?_'
    r'(epi8|epi16|epi32|epi64|epu8|epu16|epu32|epu64|ps|pd)_mask$')
PRED_IMM={"eq":0,"lt":1,"le":2,"neq":4,"ge":5,"gt":6}  # VPCMP* imm8 encoding
# VCMPPS/PD imm8 encoding (the legacy 8 ordered/unordered float predicates)
FLOAT_PRED_IMM={"eq":0,"lt":1,"le":2,"unord":3,"neq":4,"nlt":5,"nle":6,"ord":7}
MOVEPI_RE=re.compile(r'^_mm(512|256|)_movepi(8|16|32|64)_mask$')   # vector -> mask
MOVM_RE=re.compile(r'^_mm(512|256|)_movm_(epi8|epi16|epi32|epi64)$')  # mask -> vector
MOV_RE=re.compile(r'^_mm(512|256|)_(mask|maskz)_mov_(ps|pd|epi8|epi16|epi32|epi64)$')
# other mask-producing ops: test (a&b!=0), testn (a&b==0), fpclass (classify+imm)
MASKOUT_RE=re.compile(r'^_mm(512|256|)_(mask_)?(test|testn|fpclass)_'
    r'(epi8|epi16|epi32|epi64|epu8|epu16|epu32|epu64|ps|pd)_mask$')
# k-register (mask) ops: mask(s) -> mask
K_RE=re.compile(r'^_k(andn|and|xnor|xor|or|add|not|shiftli|shiftri)_mask(8|16|32|64)$')
# mask concatenation (kunpck) and mask <-> GP-register moves (kmov)
KUNPACK_RE=re.compile(r'^_mm512_kunpack(b|w|d)$')
CVTMASK_RE=re.compile(r'^_cvtmask(8|16|32|64)_u(32|64)$')   # mask -> GP int
CVTU_RE=re.compile(r'^_cvtu(32|64)_mask(8|16|32|64)$')      # GP int -> mask
# broadcast a mask register's value into every vector lane (vpbroadcastm{b2q,w2d})
BCASTM_RE=re.compile(r'^_mm(512|256|)_broadcastm(b|w)_epi(32|64)$')
# 128-bit-lane shuffle by immediate (vshuff32x4 etc.)
SHUF4_RE=re.compile(r'^_mm(512|256)_(mask_|maskz_)?shuffle_(([fi])(\d+)x(\d+))$')
# subvector insert/extract by immediate (vinsert/vextract f32x4/i64x2/...): a
# narrow (128/256-bit) lane is placed into / taken out of a wider vector.
INSERT_RE=re.compile(r'^_mm(512|256)_(mask_|maskz_)?insert([fi])(32|64)x(2|4|8)$')
EXTRACT_RE=re.compile(r'^_mm(512|256)_(mask_|maskz_)?extract([fi])(32|64)x(2|4|8)'
    r'_(ps|pd|epi32|epi64)$')
def _ie_tag(fi, esize): return ({32:"ps",64:"pd"} if fi=="f" else {32:"epi32",64:"epi64"})[esize]
KOP={"and":"and","or":"or","xor":"xor","xnor":"xnor","andn":"andnot","add":"add",
     "not":"not","shiftli":"shift_left","shiftri":"shift_right"}

def parse_compare(name):
    m=CMP_RE.match(name)
    if not m: return None
    return dict(width={"512":512,"256":256,"":128}[m.group(1)],
        writemask=(m.group(2)=="mask_"), pred=(m.group(3) or ""), tag=m.group(4))

def pick_compare_binding(mnem, width, writemask):
    fm,vr=full_mem(width),vreg(width)
    for b in by_key.get(mnem.upper(), []):
        if b.width!=width or b.merge: continue
        if not (b.res_kind=="res" and b.res_locs and b.res_locs[0].locs==["K"]):
            continue
        if any(a.enc=="Mask" for a in b.args)!=writemask: continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and vr in rm[0].locs and fm in rm[0].locs: return b
    return None

def pick_round_binding(mnem, width, masking, label):
    """The register-only embedded-rounding/sae form (carries ~rnd or ~sae)."""
    want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
    vr,fm=vreg(width),full_mem(width)
    for b in by_key.get(mnem.upper(), []):
        if b.width!=width or b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        if label not in b.labels.split(): continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and vr in rm[0].locs and fm not in rm[0].locs:
            return b
    return None

# scalar ss/sd ops operate on lane 0 (upper lanes flow from the 1st operand);
# only the AVX512 masked forms exist (unmasked _mm_add_ss is SSE).  They use the
# vec128 surface with a 1-bit writemask.
SCALAR_RE=re.compile(r'^_mm_(mask_|maskz_)?(add|sub|mul|div|min|max|sqrt|'
    r'getexp|scalef|rcp14|rsqrt14)_(ss|sd)$')
SCALAR_FMA_RE=re.compile(r'^_mm_(mask_|maskz_|mask3_)?(fmadd|fmsub|fnmadd|fnmsub)'
    r'_(round_)?(ss|sd)$')
FMA_MNEM={"fmadd":"VFMADD","fmsub":"VFMSUB","fnmadd":"VFNMADD","fnmsub":"VFNMSUB"}
# scalar control-immediate ops (getmant/range/reduce/roundscale): 2 vec inputs +
# a control imm8 (+ embedded SAE in the _round forms).
SCALAR_CTRL_RE=re.compile(r'^_mm_(mask_|maskz_)?(getmant|range|reduce|roundscale)'
    r'_(round_)?(ss|sd)$')
SCALAR_CTRL_MNEM={"getmant":"VGETMANT","range":"VRANGE","reduce":"VREDUCE",
    "roundscale":"VRNDSCALE"}
SCALAR_FIX_RE=re.compile(r'^_mm_(mask_|maskz_)?fixupimm_(round_)?(ss|sd)$')
def pick_scalar_binding(mnem, masking):
    want_mask = masking in ("mask","maskz"); want_merge = masking=="mask"
    for b in by_key.get(mnem.upper(), []):
        if b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and "XMM" in rm[0].locs and any(l in ("M32","M64") for l in rm[0].locs):
            return b
    return None
# register-source x2 sub-vector broadcast (vbroadcastf32x2/i32x2; the x4/x8/64x2
# forms are memory-only in the ISA so only the x2 forms have a register input)
SUBVEC_BCAST_RE=re.compile(r'^_mm(512|256|)_(mask_|maskz_)?broadcast_(f32x2|i32x2)$')
# scalar float lane0 <-> GP int converts (vcvtss2si / vcvtsi2ss + usi/round/sae)
SC_F2I_RE=re.compile(r'^_mm_cvt(t?)(_round)?(ss|sd)_(si|i|u)(32|64)$')
SC_I2F_RE=re.compile(r'^_mm_cvt(_round)?(si|i|u)(32|64)_(ss|sd)$')
def pick_scalar_cvt_binding(mnem, label, gp, gp_is_res):
    for b in by_key.get(mnem.upper(), []):
        if b.merge or b.imm!="Imm_none": continue
        if any(a.enc=="Mask" for a in b.args): continue
        labs=b.labels.split()
        if label is None:
            if "~rnd" in labs or "~sae" in labs: continue
        elif label not in labs: continue
        if gp_is_res:
            if not (b.res_kind=="res" and b.res_locs and gp in b.res_locs[0].locs):
                continue
        else:
            rm=[a for a in b.args if a.enc=="RM_rm"]
            if not (rm and gp in rm[0].locs): continue
        return b
    return None
def gpspec(bits, signed):
    return {"kind":"gpint","ml":("int32" if bits==32 else "int64"),
            "c":("" if signed else "u")+("int32_t" if bits==32 else "int64_t"),
            "bits":bits,"signed":signed}
# scalar mask-producing compare / classify, and scalar masked move
SCALAR_CMP_RE=re.compile(r'^_mm_(mask_)?cmp_(ss|sd)_mask$')
SCALAR_FPCLASS_RE=re.compile(r'^_mm_(mask_)?fpclass_(ss|sd)_mask$')
SCALAR_MOVE_RE=re.compile(r'^_mm_(mask|maskz)_move_(ss|sd)$')
def pick_scalar_compare_binding(mnem, writemask):
    for b in by_key.get(mnem.upper(), []):
        if b.merge: continue
        if not (b.res_kind=="res" and b.res_locs and b.res_locs[0].locs==["K"]):
            continue
        if any(a.enc=="Mask" for a in b.args)!=writemask: continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and "XMM" in rm[0].locs and any(l in ("M32","M64") for l in rm[0].locs):
            return b
    return None
def pick_move_binding(mnem, masking):
    want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
    for b in by_key.get(mnem.upper(), []):
        if b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        if any(l.startswith("M") for a in b.args for l in a.locs): continue  # reg-only
        if sum(1 for r in arg_roles(b) if r[0]=="vec")==2: return b
    return None
# scalar embedded-rounding / SAE form (reg-only, carries ~rnd or ~sae)
SCALAR_ROUND_RE=re.compile(r'^_mm_(mask_|maskz_)?(add|sub|mul|div|sqrt|scalef|'
    r'min|max|getexp)_round_(ss|sd)$')
def pick_scalar_round_binding(mnem, masking, label):
    want_mask = masking in ("mask","maskz"); want_merge = masking=="mask"
    for b in by_key.get(mnem.upper(), []):
        if b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        if label not in b.labels.split(): continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and "XMM" in rm[0].locs and not any(l.startswith("M") for l in rm[0].locs):
            return b
    return None

def pick_sae_imm_binding(mnem, width, masking):
    """The register-only embedded-SAE form that also carries a control imm8
    (range/roundscale/getmant _round)."""
    want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
    vr,fm=vreg(width),full_mem(width)
    for b in by_key.get(mnem.upper(), []):
        if b.width!=width or b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        if "~sae" not in b.labels.split(): continue
        if b.imm not in ("Imm_spec","Imm_reg"): continue
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if rm and vr in rm[0].locs and fm not in rm[0].locs: return b
    return None

def pick_shift_binding(mnem, width, masking):
    """Shift-by-xmm-count: data in Vex_v (vector), count in RM_rm (xmm/m128)."""
    want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
    vr=vreg(width)
    for b in by_key.get(mnem.upper(), []):
        if b.merge!=want_merge: continue
        if any(a.enc=="Mask" for a in b.args)!=want_mask: continue
        vexv=[a for a in b.args if a.enc=="Vex_v"]
        rm=[a for a in b.args if a.enc=="RM_rm"]
        if vexv and rm and vr in vexv[0].locs and "XMM" in rm[0].locs and "M128" in rm[0].locs:
            return b
    return None

# ---------- drive ----------
emitted=[]
skips=collections.Counter()
skip_examples=collections.defaultdict(list)
def skip(reason, name):
    skips[reason]+=1
    if len(skip_examples[reason])<4: skip_examples[reason].append(name)

def imm_params_of(it):
    # An immediate parameter has either an immwidth or the IMM etype (enum-style
    # controls such as roundscale/getmant have IMM etype with no immwidth).
    return [p for p in it["params"] if p[4] or p[0]=="IMM"]
def imm_max_of(imm_params):
    if not imm_params: return None
    w=imm_params[0][4]
    return (1<<int(w))-1 if w else 255
def masksuffix(m): return {"maskz":"_maskz","mask":"_mask","mask3":"_mask3",
                           "mask2":"_mask2","none":""}[m]
def make_spec(ml,c): return {"ml":ml,"c":c,"nl":width_of_vt(ml)//64}
def build_specs(roles,in_ml,in_c,out_ml,out_c):
    specs=[]
    for k,_ in roles:
        if k=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
        elif k=="merge": specs.append({"kind":"merge",**make_spec(out_ml,out_c)})
        else: specs.append({"kind":"vec",**make_spec(in_ml,in_c)})
    return specs

seen_caml=set()
def add(name,caml,b,mnem,masking,lanes,imm_max,in_ml,in_c,out_ml,out_c):
    if caml in seen_caml: skip("dup_caml_name", name); return
    seen_caml.add(caml)
    emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
        lanes=lanes,imm_max=imm_max,argspecs=build_specs(arg_roles(b),in_ml,in_c,out_ml,out_c),
        out=make_spec(out_ml,out_c)))

for it in intrinsics:
    name=it["name"]
    # Composite intrinsics with no underlying instruction (set/setr/...) are out
    # of scope; when several instruction forms are listed (e.g. vpermt2/vpermi2
    # for permutex2var) the first is a valid lowering, and the bit-exact test
    # confirms the choice.
    if not it["insts"]: skip("no_instruction", name); continue
    mnem,_=it["insts"][0]
    if not mnem: skip("no_mnemonic", name); continue
    # ---- mask-producing compare (returns a mask, with predicate imm) ----
    cm=parse_compare(name)
    if cm is not None:
        width,tag,writemask,pred=cm["width"],cm["tag"],cm["writemask"],cm["pred"]
        _,bits,isf,isu=SUFFIX[tag]
        b=pick_compare_binding(mnem, width, writemask)
        if b is None:
            skip(f"no_cmp_binding:{mnem}/{width}/{'wm' if writemask else 'u'}", name); continue
        in_vt=vectype(tag,width); in_c=c_vectype(tag,width)
        has_imm = b.imm in ("Imm_spec","Imm_reg")
        if pred=="":
            # generic compare: predicate is a runtime immediate
            if not has_imm: skip("cmp_no_imm", name); continue
            opname="cmp"+("_unsigned" if isu else ""); imm_max=(31 if isf else 7); bake=None
        else:
            # fixed-predicate compare: bake the predicate (dedicated
            # vpcmpeq*/vpcmpgt* take no immediate, so bake=None there).
            # float uses the VCMPPS encoding, integers the VPCMP encoding.
            opname="cmp"+pred+("_unsigned" if isu else "")
            ptab=FLOAT_PRED_IMM if isf else PRED_IMM
            imm_max=None; bake=ptab[pred] if has_imm else None
        caml=f"caml_avx512_{in_vt}_{opname}"+("_mask" if writemask else "")
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=width//bits, imm_max=imm_max, bake_imm=bake,
            argspecs=build_specs(arg_roles(b),in_vt,in_c,in_vt,in_c),
            out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- k-register (mask) ops: and/or/xor/.../shift, mask(s) -> mask ----
    km=K_RE.match(name)
    if km:
        op=km.group(1); W=int(km.group(2))
        bd=binds.get(mnem.lower())
        if bd is None: skip(f"no_k_binding:{mnem}", name); continue
        caml=f"caml_avx512_mask{W}_{KOP[op]}"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[{"kind":"mask","ml":"mask","c":"int64","nl":0} for _ in bd.args]
        emitted.append(dict(intel=name,caml=caml,binding=bd,mnem=mnem,masking="none",
            lanes=W, imm_max=imm_max_of(imm_params_of(it)), bake_imm=None,
            argspecs=specs, out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- kunpck: concatenate two half-width masks into a double-width mask ----
    ku=KUNPACK_RE.match(name)
    if ku:
        outW={"b":16,"w":32,"d":64}[ku.group(1)]
        bd=binds.get(mnem.lower())
        if bd is None: skip(f"no_kunpack_binding:{mnem}", name); continue
        caml=f"caml_avx512_mask{outW}_unpack"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[{"kind":"mask","ml":"mask","c":"int64","nl":0} for _ in bd.args]
        emitted.append(dict(intel=name,caml=caml,binding=bd,mnem=mnem,masking="none",
            lanes=outW, imm_max=None, bake_imm=None,
            argspecs=specs, out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- cvtmask -> GP int (kmov r, k): mask bits zero-extended into a GPR ----
    cmk=CVTMASK_RE.match(name)
    if cmk:
        mw=int(cmk.group(1)); rw=int(cmk.group(2))
        bd=binds.get(f"{mnem.lower()}_{'r64' if rw==64 else 'r32'}_K")
        if bd is None: skip(f"no_cvtmask_binding:{mnem}", name); continue
        caml=f"caml_avx512_mask{mw}_to_int{rw}"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=bd,mnem=mnem,masking="none",
            lanes=mw, imm_max=None, bake_imm=None,
            argspecs=[{"kind":"mask","ml":"mask","c":"int64","nl":0}],
            out=gpspec(rw, False)))
        continue
    # ---- cvtu -> mask (kmov k, r): low bits of a GPR loaded into a mask ----
    cvu=CVTU_RE.match(name)
    if cvu:
        gw=int(cvu.group(1)); mw=int(cvu.group(2))
        bd=binds.get(f"{mnem.lower()}_K_{'r64' if gw==64 else 'r32'}")
        if bd is None: skip(f"no_cvtu_binding:{mnem}", name); continue
        caml=f"caml_avx512_int{gw}_to_mask{mw}"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=bd,mnem=mnem,masking="none",
            lanes=mw, imm_max=None, bake_imm=None,
            argspecs=[gpspec(gw, False)],
            out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- broadcast a mask value into every lane (vpbroadcastmb2q / mw2d): the
    # input is an 8-/16-bit mask, each lane receives its zero-extended value. ----
    bcm=BCASTM_RE.match(name)
    if bcm:
        width={"512":512,"256":256,"":128}[bcm.group(1)]
        bw=bcm.group(2)
        in_mask_bits={"b":8,"w":16}[bw]; out_tag={"b":"epi64","w":"epi32"}[bw]
        out_vt=vectype(out_tag,width); out_c=c_vectype(out_tag,width)
        b=next((x for x in by_key.get(mnem.upper(),[])
                if x.width==width and x.res_kind=="res"), None)
        if b is None: skip(f"no_broadcastm_binding:{mnem}/{width}", name); continue
        caml=f"caml_avx512_{out_vt}_broadcastmask"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=in_mask_bits, imm_max=None, bake_imm=None,
            argspecs=[{"kind":"mask","ml":"mask","c":"int64","nl":0}],
            out=make_spec(out_vt,out_c)))
        continue
    # ---- other mask-producing ops: test / testn / fpclass ----
    mo=MASKOUT_RE.match(name)
    if mo:
        width={"512":512,"256":256,"":128}[mo.group(1)]
        writemask=(mo.group(2)=="mask_"); op=mo.group(3); tag=mo.group(4)
        _,bits,isf,isu=SUFFIX[tag]
        b=pick_compare_binding(mnem, width, writemask)
        if b is None: skip(f"no_maskout_binding:{mnem}/{width}", name); continue
        imm_params=imm_params_of(it); has_imm=b.imm in ("Imm_spec","Imm_reg")
        if (len(imm_params)>0)!=has_imm: skip(f"maskout_imm:{op}", name); continue
        in_vt=vectype(tag,width); in_c=c_vectype(tag,width)
        caml=f"caml_avx512_{in_vt}_{op}"+("_unsigned" if isu else "")+("_mask" if writemask else "")
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=width//bits, imm_max=imm_max_of(imm_params), bake_imm=None,
            argspecs=build_specs(arg_roles(b),in_vt,in_c,in_vt,in_c),
            out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- 128-bit-lane shuffle by immediate (shuffle_f32x4 etc.) ----
    sh=SHUF4_RE.match(name)
    if sh:
        width={"512":512,"256":256}[sh.group(1)]
        masking={"mask_":"mask","maskz_":"maskz",None:"none"}[sh.group(2)]
        suffix=sh.group(3); fc=sh.group(4); elbits=int(sh.group(5))
        tag=("pd" if elbits==64 else "ps") if fc=="f" else "epi"+str(elbits)
        b=pick_binding(mnem, width, masking, want_imm=True)
        if b is None: skip(f"no_shuf4_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("shuf4_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        # name by the lane suffix (shuffle_i32x4) so it never collides with the
        # in-lane dword/qword shuffle vpshufd/vpshufq (caml ..._shuffle).
        caml=f"caml_avx512_{vt}_shuffle_{suffix}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=imm_max_of(imm_params_of(it)), bake_imm=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- subvector insert: place a narrow lane (b) into a wide vector (a) ----
    ins=INSERT_RE.match(name)
    if ins:
        width=int(ins.group(1)); masking={"mask_":"mask","maskz_":"maskz",None:"none"}[ins.group(2)]
        fi=ins.group(3); esize=int(ins.group(4)); count=int(ins.group(5))
        subw=esize*count; tag=_ie_tag(fi,esize)
        wide_vt=vectype(tag,width); wide_c=c_vectype(tag,width)
        sub_vt=vectype(tag,subw); sub_c=c_vectype(tag,subw)
        # The insert binding's RM_rm holds the NARROW source, so generic
        # pick_binding (which wants a full-width RM_rm) never matches; select the
        # insert form directly by its wide Vex_v source operand (like extract).
        want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
        b=next((x for x in by_key.get(mnem.upper(),[]) if x.width==width and x.merge==want_merge
                and x.imm in ("Imm_spec","Imm_reg")
                and (any(a.enc=="Mask" for a in x.args))==want_mask
                and any(a.enc=="Vex_v" for a in x.args)), None)
        if b is None: skip(f"no_insert_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("insert_arity", name); continue
        caml=f"caml_avx512_{wide_vt}_insert_{sub_vt}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]; nv=0
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(wide_vt,wide_c)})
            else:  # vec 1 = wide a, vec 2 = narrow b
                nv+=1
                specs.append({"kind":"vec",**make_spec(wide_vt if nv==1 else sub_vt,
                    wide_c if nv==1 else sub_c)})
        # only the low log2(width/subw) bits select the lane; gcc rejects a
        # wider immediate, so cap at the number of positions - 1.
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=width//subw-1, bake_imm=None,
            argspecs=specs, out=make_spec(wide_vt,wide_c)))
        continue
    # ---- subvector extract: take a narrow lane out of a wide vector ----
    # (the merge form uses the RM_rm-register _K_merge binding; the merge source
    # is the narrow result type.)
    ext=EXTRACT_RE.match(name)
    if ext:
        width=int(ext.group(1)); masking={"mask_":"mask","maskz_":"maskz",None:"none"}[ext.group(2)]
        fi=ext.group(3); esize=int(ext.group(4)); count=int(ext.group(5))
        subw=esize*count; tag=_ie_tag(fi,esize)
        wide_vt=vectype(tag,width); wide_c=c_vectype(tag,width)
        sub_vt=vectype(tag,subw); sub_c=c_vectype(tag,subw)
        want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
        b=next((x for x in by_key.get(mnem.upper(),[]) if x.width==width and x.merge==want_merge
                and x.imm in ("Imm_spec","Imm_reg")
                and (any(a.enc=="Mask" for a in x.args))==want_mask
                and [a for a in x.args if a.enc=="RM_r"]
                and vreg(width) in [a for a in x.args if a.enc=="RM_r"][0].locs), None)
        if b is None: skip(f"no_extract_binding:{mnem}/{masking}", name); continue
        caml=f"caml_avx512_{wide_vt}_extract_{sub_vt}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(sub_vt,sub_c)})
            else: specs.append({"kind":"vec",**make_spec(wide_vt,wide_c)})
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=subw//esize, imm_max=width//subw-1, bake_imm=None,
            argspecs=specs, out=make_spec(sub_vt,sub_c)))
        continue
    # ---- mask <-> vector conversions (vpmovd2m / vpmovm2d etc.) ----
    me=MOVEPI_RE.match(name)
    if me:
        width={"512":512,"256":256,"":128}[me.group(1)]; tag="epi"+me.group(2)
        in_vt=vectype(tag,width); in_c=c_vectype(tag,width)
        b=next((x for x in by_key.get(mnem.upper(),[]) if x.width==width and not x.merge
                and x.imm=="Imm_none" and x.res_kind=="res" and x.res_locs
                and x.res_locs[0].locs==["K"]
                and not any(a.enc=="Mask" for a in x.args)), None)
        if b is None: skip("no_movepi_binding", name); continue
        caml=f"caml_avx512_{in_vt}_movepi_mask"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=width//SUFFIX[tag][1], imm_max=None,
            argspecs=[{"kind":"vec",**make_spec(in_vt,in_c)}],
            out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    mv=MOVM_RE.match(name)
    if mv:
        width={"512":512,"256":256,"":128}[mv.group(1)]; tag=mv.group(2)
        out_vt=vectype(tag,width); out_c=c_vectype(tag,width)
        b=next((x for x in by_key.get(mnem.upper(),[]) if not x.merge and x.imm=="Imm_none"
                and x.res_kind=="res" and x.res_locs and loc_vw(x.res_locs[0].locs)==width
                and x.args and x.args[0].locs==["K"]), None)
        if b is None: skip("no_movm_binding", name); continue
        caml=f"caml_avx512_{out_vt}_movm"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=width//SUFFIX[tag][1], imm_max=None,
            argspecs=[{"kind":"mask","ml":"mask","c":"int64","nl":0}],
            out=make_spec(out_vt,out_c)))
        continue
    # ---- scalar ss/sd FMA (same 132/231 form selection as packed; mask3 merges
    # into the addend) ----
    scf=SCALAR_FMA_RE.match(name)
    if scf:
        masking={"mask_":"mask","maskz_":"maskz","mask3_":"mask3",None:"none"}[scf.group(1)]
        core=scf.group(2); rnd=bool(scf.group(3)); suf=scf.group(4)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        form="231" if masking=="mask3" else "132"
        mnem=f"{FMA_MNEM[core]}{form}{suf.upper()}"
        bmask="maskz" if masking in ("maskz","mask","mask3") else "none"
        b=(pick_scalar_round_binding(mnem,bmask,"~rnd") if rnd
           else pick_scalar_binding(mnem,bmask))
        if b is None: skip(f"no_scalarfma_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("scalarfma_arity", name); continue
        caml=f"caml_avx512_{elt}_{FMA[core]}"+("_round" if rnd else "")+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        e=dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=1, imm_max=(11 if rnd else None), bake_imm=None, round_dispatch=rnd,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc))
        if masking=="mask3":
            e.update(rot3=True, force_z="false", oracle_mask_pos=3)
        else:
            e["swap12"]=True
            if masking=="mask": e.update(force_z="false", oracle_mask_pos=1)
        emitted.append(e)
        continue
    # ---- scalar fixupimm (a, b float + int table c, imm [+SAE]); res = Arg[0]
    # (dst = a) so the merge form reuses the _K binding with ~z:false ----
    scx=SCALAR_FIX_RE.match(name)
    if scx:
        masking={"mask_":"mask","maskz_":"maskz",None:"none"}[scx.group(1)]
        rnd=bool(scx.group(2)); suf=scx.group(3)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        tbits=32 if suf=="ss" else 64
        tbl_vt=f"int{tbits}x{128//tbits}"; tbl_c="__m128i"
        mnem="VFIXUPIMM"+suf.upper()
        bmask="maskz" if masking in ("maskz","mask") else "none"
        b=(pick_scalar_round_binding(mnem,bmask,"~sae") if rnd
           else pick_scalar_binding(mnem,bmask))
        if b is None: skip(f"no_scalarfix_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("scalarfix_arity", name); continue
        caml=f"caml_avx512_{elt}_fixupimm"+("_round" if rnd else "")+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]; nv=0
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            else:
                nv+=1
                specs.append({"kind":"vec",**make_spec(tbl_vt if nv==3 else vt,
                    tbl_c if nv==3 else vc)})
        e=dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=1, imm_max=255, bake_imm=None, sae_imm=rnd,
            oracle_imm=(["{V}","8"] if rnd else ["{V}"]),
            argspecs=specs, out=make_spec(vt,vc))
        if masking=="mask": e.update(force_z="false", oracle_mask_pos=1)
        emitted.append(e)
        continue
    # ---- scalar getmant/range/reduce/roundscale (2 inputs + control imm [+SAE]) ----
    sci=SCALAR_CTRL_RE.match(name)
    if sci:
        masking={"mask_":"mask","maskz_":"maskz",None:"none"}[sci.group(1)]
        op=sci.group(2); rnd=bool(sci.group(3)); suf=sci.group(4)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        mnem=SCALAR_CTRL_MNEM[op]+suf.upper()
        if rnd:
            b=pick_scalar_round_binding(mnem, masking, "~sae")
        else:
            b=pick_scalar_binding(mnem, masking)
        if b is None: skip(f"no_scalarctrl_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip(f"scalarctrl_arity:{op}", name); continue
        if op=="getmant": imm_max=15; oimm=["({V}) & 3","(({V}) >> 2) & 3"]
        elif op=="range": imm_max=15; oimm=["{V}"]
        else: imm_max=255; oimm=["{V}"]  # reduce, roundscale
        if rnd: oimm=oimm+["8"]  # append the fixed SAE control
        caml=f"caml_avx512_{elt}_{op}"+("_round" if rnd else "")+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=1, imm_max=imm_max, bake_imm=None, sae_imm=rnd, oracle_imm=oimm,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- scalar ss/sd with embedded rounding/SAE ----
    scr=SCALAR_ROUND_RE.match(name)
    if scr:
        masking={"mask_":"mask","maskz_":"maskz",None:"none"}[scr.group(1)]
        op=scr.group(2); suf=scr.group(3)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        sae = op in ("min","max","getexp")
        mnem="V"+op.upper()+suf.upper()
        b=pick_scalar_round_binding(mnem, masking, "~sae" if sae else "~rnd")
        if b is None: skip(f"no_scalarround_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip(f"scalarround_arity:{op}", name); continue
        caml=f"caml_avx512_{elt}_{op}_round"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=1, imm_max=(8 if sae else 11), bake_imm=None,
            round_dispatch=(not sae), sae_dispatch=sae,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- scalar ss/sd op (lane 0 computed, upper from operand a; 1-bit mask) ----
    sc=SCALAR_RE.match(name)
    if sc:
        masking={"mask_":"mask","maskz_":"maskz",None:"none"}[sc.group(1)]
        op=sc.group(2); suf=sc.group(3)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        mnem="V"+op.upper()+suf.upper()
        b=pick_scalar_binding(mnem, masking)
        if b is None: skip(f"no_scalar_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip(f"scalar_arity:{op}", name); continue
        caml=f"caml_avx512_{elt}_{op}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=1, imm_max=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- scalar float lane0 -> GP int (vcvtss2si etc.); _round=er, cvtt=sae ----
    m2i=SC_F2I_RE.match(name)
    if m2i:
        trunc=m2i.group(1)=="t"; rnd=bool(m2i.group(2)); suf=m2i.group(3)
        pfx=m2i.group(4); bits=int(m2i.group(5))
        elt="float32" if suf=="ss" else "float64"; vt=elt+("x4" if suf=="ss" else "x2")
        vc="__m128" if suf=="ss" else "__m128d"
        signed=pfx in ("i","si"); gp="R32" if bits==32 else "R64"
        label="~sae" if trunc else ("~rnd" if rnd else None)
        b=pick_scalar_cvt_binding(mnem, label, gp, gp_is_res=True)
        if b is None: skip(f"no_scalarcvt_binding:{mnem}/{gp}", name); continue
        extra=("" if signed else "_unsigned")
        caml=(f"caml_avx512_{elt}_cvt"+("t" if trunc else "")
              +f"_int{bits}{extra}"+("_round" if (rnd or trunc) else ""))
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=1, imm_max=(8 if trunc else 11 if rnd else None), bake_imm=None,
            round_dispatch=(rnd and not trunc), sae_dispatch=trunc,
            argspecs=[{"kind":"vec",**make_spec(vt,vc)}], out=gpspec(bits,signed)))
        continue
    # ---- GP int -> scalar float lane0 (vcvtsi2ss etc.); _round=er ----
    i2m=SC_I2F_RE.match(name)
    if i2m:
        rnd=bool(i2m.group(1)); pfx=i2m.group(2); bits=int(i2m.group(3)); suf=i2m.group(4)
        elt="float32" if suf=="ss" else "float64"; vt=elt+("x4" if suf=="ss" else "x2")
        vc="__m128" if suf=="ss" else "__m128d"
        signed=pfx in ("i","si"); gp="R32" if bits==32 else "R64"
        b=pick_scalar_cvt_binding(mnem, "~rnd" if rnd else None, gp, gp_is_res=False)
        if b is None: skip(f"no_scalarcvt_binding:{mnem}/{gp}", name); continue
        extra=("" if signed else "_unsigned")
        caml=f"caml_avx512_int{bits}{extra}_cvt_{elt}"+("_round" if rnd else "")
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[{"kind":"vec",**make_spec(vt,vc)}, gpspec(bits,signed)]
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=1, imm_max=(11 if rnd else None), bake_imm=None, round_dispatch=rnd,
            argspecs=specs, out=make_spec(vt,vc)))
        continue
    # ---- register-source x2 sub-vector broadcast (vbroadcast{f32,i32}x2) ----
    svb=SUBVEC_BCAST_RE.match(name)
    if svb:
        width={"512":512,"256":256,"":128}[svb.group(1)]
        masking={"mask_":"mask","maskz_":"maskz",None:"none"}[svb.group(2)]
        suf=svb.group(3); tag="ps" if suf=="f32x2" else "epi32"
        in_vt=vectype(tag,128); in_c=c_vectype(tag,128)
        out_vt=vectype(tag,width); out_c=c_vectype(tag,width)
        b=pick_convert_binding(mnem, masking, 128, width)
        if b is None: skip(f"no_subbcast_binding:{mnem}/{width}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=1: skip("subbcast_arity", name); continue
        caml=f"caml_avx512_{out_vt}_broadcast_{suf}"+masksuffix(masking)
        add(name,caml,b,mnem,masking,mask_lanes(tag,width),None,in_vt,in_c,out_vt,out_c)
        continue
    # ---- scalar mask-producing compare (cmp_ss/sd -> 1-bit mask, predicate imm) ----
    scm=SCALAR_CMP_RE.match(name)
    if scm:
        writemask=(scm.group(1)=="mask_"); suf=scm.group(2)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        b=pick_scalar_compare_binding(mnem, writemask)
        if b is None: skip(f"no_scalarcmp_binding:{mnem}", name); continue
        caml=f"caml_avx512_{elt}_cmp"+("_mask" if writemask else "")
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=1, imm_max=31, bake_imm=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc),
            out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- scalar fpclass (classify lane0 by imm8 category -> 1-bit mask) ----
    scfp=SCALAR_FPCLASS_RE.match(name)
    if scfp:
        writemask=(scfp.group(1)=="mask_"); suf=scfp.group(2)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        b=pick_scalar_compare_binding(mnem, writemask)
        if b is None: skip(f"no_scalarfpclass_binding:{mnem}", name); continue
        caml=f"caml_avx512_{elt}_fpclass"+("_mask" if writemask else "")
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=1, imm_max=imm_max_of(imm_params_of(it)), bake_imm=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc),
            out={"ml":"mask","c":"int64","nl":0,"mask_result":True}))
        continue
    # ---- scalar masked move (lane0 = k?b:src/0 ; upper lanes from a) ----
    scmv=SCALAR_MOVE_RE.match(name)
    if scmv:
        masking=scmv.group(1); suf=scmv.group(2)
        elt="float32" if suf=="ss" else "float64"
        vt="float32x4" if suf=="ss" else "float64x2"
        vc="__m128" if suf=="ss" else "__m128d"
        b=pick_move_binding(mnem, masking)
        if b is None: skip(f"no_move_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("move_arity", name); continue
        caml=f"caml_avx512_{elt}_move"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=1, imm_max=None, bake_imm=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- masked register move (vmov*): masked merge / zeroing copy (only the
    # _mask/_maskz forms exist).  Lower via the UNALIGNED mnemonic so a source
    # spilled to an unaligned stack slot does not fault (the aligned VMOVDQA*/
    # VMOVAP* that Intel names would #GP). ----
    mov=MOV_RE.match(name)
    if mov:
        width={"512":512,"256":256,"":128}[mov.group(1)]
        masking="mask" if mov.group(2)=="mask" else "maskz"
        tag=mov.group(3)
        umnem={"ps":"vmovups","pd":"vmovupd","epi8":"vmovdqu8","epi16":"vmovdqu16",
               "epi32":"vmovdqu32","epi64":"vmovdqu64"}[tag]
        W={512:"Z",256:"Y",128:"X"}[width]
        Wm={512:"Zm512",256:"Ym256",128:"Xm128"}[width]
        bname=(f"{umnem}_{W}_{W}_K_merge" if masking=="mask"
               else f"{umnem}_{W}_{Wm}_K")
        b=next((x for x in by_key.get(umnem.upper(),[]) if x.name==bname), None)
        if b is None: skip(f"no_mov_binding:{bname}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_mov"+masksuffix(masking)
        add(name,caml,b,umnem,masking,mask_lanes(tag,width),None,vt,vc,vt,vc)
        continue
    pn=parse_name(name)
    if not pn: skip("unparsed_name", name); continue
    width,masking,tag,core=pn["width"],pn["masking"],pn["tag"],pn["core"]
    # ---- set1: broadcast a GP-int scalar into every lane (vpbroadcast{b,w,d,q}
    # from a general-purpose register; the input is an OCaml int held in a GP reg,
    # like the SSE insert builtins).  epi32 uses r32, others use r64.  set1_ps/pd
    # are composite (no backing instruction). ----
    if core=="set1":
        if tag not in ("epi8","epi16","epi32","epi64"):
            skip(f"set1_tag:{tag}", name); continue
        # vpbroadcastb/w/d take an r32 GP source (W0), vpbroadcastq an r64 (W1).
        rgp,gp_ml,gp_c=(("R32","int32","int") if tag in ("epi8","epi16","epi32")
                        else ("R64","int64","long long"))
        want_mask=masking in ("mask","maskz"); want_merge=masking=="mask"
        b=next((x for x in by_key.get(mnem.upper(),[])
                if x.width==width and x.merge==want_merge
                and (any(a.enc=="Mask" for a in x.args))==want_mask
                and (masking!="maskz" or "~z" in x.labels.split())
                and any(a.enc=="RM_rm" and rgp in a.locs for a in x.args)), None)
        if b is None: skip(f"no_set1_binding:{mnem}/{masking}", name); continue
        out_vt=vectype(tag,width); out_c=c_vectype(tag,width)
        specs=[]
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(out_vt,out_c)})
            else: specs.append({"kind":"gpint","ml":gp_ml,"c":gp_c,"nl":1})
        caml=f"caml_avx512_{out_vt}_set1"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=None,
            argspecs=specs, out=make_spec(out_vt,out_c)))
        continue
    # ---- whole-vector logical (and/or/xor/andnot _si128/256/512) ----
    if tag in WHOLE:
        if masking!="none" or core not in ("and","or","xor","andnot"):
            skip("whole_vector_TODO", name); continue
        W=WHOLE[tag]; carrier=f"int64x{W//64}"; cc=c_vectype("epi64",W)
        b=pick_binding(mnem, W, "none")
        if b is None: skip(f"no_whole_binding:{mnem}/{W}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("whole_arity", name); continue
        caml=f"caml_avx512_vec{W}_{OP[core]}"
        add(name,caml,b,mnem,"none",None,None,carrier,cc,carrier,cc)
        continue
    imm_params=imm_params_of(it)
    # getmant has two enum immediates (interval, sign) packed into one imm8 =
    # (sign << 2) | interval; expose the combined imm8 and split it in the oracle
    if core=="getmant":
        _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip("op_not_in_table:getmant", name); continue
        b=pick_binding(mnem, width, masking, want_imm=True)
        if b is None: skip(f"no_getmant_binding:{mnem}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_getmant"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=15, bake_imm=None,
            oracle_imm=["({V}) & 3","(({V}) >> 2) & 3"],
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- SAE round variants carrying a control immediate (range/roundscale/
    # getmant _round): extract the control imm, embed ~sae, and have the oracle
    # pass the control value(s) plus the fixed sae control (8 = _MM_FROUND_NO_EXC).
    if core in ("range_round","roundscale_round","getmant_round","reduce_round"):
        base=core[:-6]; _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip(f"op_not_in_table:{core}", name); continue
        b=pick_sae_imm_binding(mnem, width, masking)
        if b is None: skip(f"no_saeimm_binding:{mnem}/{masking}", name); continue
        nvec=sum(1 for r in arg_roles(b) if r[0]=="vec")
        if nvec!=(2 if base=="range" else 1): skip(f"saeimm_arity:{base}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_{base}_round"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        if base=="getmant": imm_max=15; oimm=["({V}) & 3","(({V}) >> 2) & 3","8"]
        elif base=="range": imm_max=15; oimm=["{V}","8"]
        else: imm_max=255; oimm=["{V}","8"]  # roundscale, reduce
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=imm_max, bake_imm=None, sae_imm=True,
            oracle_imm=oimm,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # fixupimm_round carries (control imm8, SAE imm); its handler bakes the SAE.
    if len(imm_params)>1 and core!="fixupimm_round": skip("multi_imm", name); continue
    # ---- convert with embedded rounding/sae (cvt_round* / cvtt_round*) ----
    if core.startswith("cvt") and "_round" in core:
        cvr=parse_convert(core.replace("_round",""), tag, width)
        if cvr is None or cvr["in_w"]<128 or cvr["out_w"]<128:
            skip(f"op_not_in_table:{core}", name); continue
        truncating = core.startswith("cvtt")
        label = "~sae" if truncating else "~rnd"
        b=pick_convert_binding(mnem, masking, cvr["in_w"], cvr["out_w"], label)
        if b is None: skip(f"no_cvtround_binding:{mnem}/{masking}", name); continue
        in_c=c_vectype(cvr["in_tag"],cvr["in_w"]); out_c=c_vectype(cvr["out_tag"],cvr["out_w"])
        caml=f"caml_avx512_{cvr['flavor']}_{cvr['in_vt']}_{cvr['out_vt']}{cvr['extra']}_round"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=cvr["lanes"], imm_max=(8 if truncating else 11), bake_imm=None,
            round_dispatch=(not truncating), sae_dispatch=truncating,
            argspecs=build_specs(arg_roles(b),cvr["in_vt"],in_c,cvr["out_vt"],out_c),
            out=make_spec(cvr["out_vt"],out_c)))
        continue
    # ---- convert? ----
    cv=parse_convert(core, tag, width)
    if cv is not None:
        if imm_params: skip(f"conv_with_imm:{core}", name); continue
        # The output must be a real (>=128-bit) vector type. An up-convert
        # (sign/zero extend) reads its input from a register that may be wider
        # than the consumed low lanes -- the binding dictates the input width, so
        # match on output width only and take the input type from the binding.
        up = cv["in_w"] < cv["out_w"]
        if cv["out_w"]<128 or (not up and cv["in_w"]<128):
            skip("conv_subvector", name); continue  # e.g. 2xf32 result, not a type
        b=pick_convert_binding(mnem, masking, None if up else cv["in_w"], cv["out_w"])
        if b is None:
            skip(f"no_conv_binding:{cv['in_w']}to{cv['out_w']}/{masking}", name); continue
        if b.imm!="Imm_none": skip("conv_binding_imm", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=1: skip("conv_arity", name); continue
        in_w = conv_input_width(b) if up else cv["in_w"]
        in_vt = vectype(cv["in_tag"], in_w)
        caml=f"caml_avx512_{cv['flavor']}_{in_vt}_{cv['out_vt']}{cv['extra']}"+masksuffix(masking)
        add(name,caml,b,mnem,masking,cv["lanes"],None,
            in_vt,c_vectype(cv["in_tag"],in_w),
            cv["out_vt"],c_vectype(cv["out_tag"],cv["out_w"]))
        continue
    # ---- shift by a scalar count held in an xmm (sll/srl/sra) ----
    if core in ("sll","srl","sra"):
        _,bits,isf,isu=SUFFIX[tag]
        if isf or imm_params: skip(f"op_not_in_table:{core}", name); continue
        b=pick_shift_binding(mnem, width, masking)
        if b is None: skip(f"no_shift_binding:{mnem}/{width}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_{core}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]; nvec=0
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(vt,vc)})
            else:
                nvec+=1
                specs.append({"kind":"vec",**make_spec(vt if nvec==1 else "int64x2",
                    vc if nvec==1 else "__m128i")})
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=None, argspecs=specs, out=make_spec(vt,vc)))
        continue
    # ---- broadcast the low element of an xmm to the whole vector ----
    if core in ("broadcastb","broadcastw","broadcastd","broadcastq","broadcastss","broadcastsd"):
        # at 128 bits only the masked forms are AVX512 (the unmasked xmm->xmm
        # broadcast is the AVX2 intrinsic, already filtered out of scope).
        if imm_params or (width==128 and masking=="none"):
            skip(f"op_not_in_table:{core}", name); continue
        in_vt=vectype(tag,128); in_c=c_vectype(tag,128)
        out_vt=vectype(tag,width); out_c=c_vectype(tag,width)
        b=pick_convert_binding(mnem, masking, 128, width)
        if b is None: skip(f"no_bcast_binding:{mnem}/{width}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=1: skip("bcast_arity", name); continue
        caml=f"caml_avx512_{out_vt}_broadcast"+masksuffix(masking)
        add(name,caml,b,mnem,masking,mask_lanes(tag,width),None,in_vt,in_c,out_vt,out_c)
        continue
    # ---- fixupimm: a, b (float) and an integer fixup table, plus an imm8 ----
    if core in ("fixupimm","fixupimm_round"):
        rnd=(core=="fixupimm_round")
        _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip(f"op_not_in_table:{core}", name); continue
        # fixupimm's result reuses its dst as the 1st input (res = Arg[0]), so
        # the merge form is the _K binding with ~z:false (writemask after the
        # 1st operand), like ternarylogic / FMA -- there is no _K_merge binding.
        # The _round form selects the embedded-SAE (~sae) reg-reg binding and
        # bakes the SAE control imm (8) as a second oracle immediate.
        def _fpick(msk):
            want_mask=msk in ("mask","maskz"); want_merge=msk=="mask"
            for bb in by_key.get(mnem.upper(), []):
                if bb.width!=width or bb.merge!=want_merge: continue
                if (any(a.enc=="Mask" for a in bb.args))!=want_mask: continue
                if bb.imm not in ("Imm_spec","Imm_reg"): continue
                if ("~sae" in bb.labels.split())!=rnd: continue
                return bb
            return None
        b=(_fpick(masking) if rnd else pick_binding(mnem, width, masking, want_imm=True))
        merge_via_z=False
        if b is None and masking=="mask":
            bz=(_fpick("maskz") if rnd else pick_binding(mnem, width, "maskz", want_imm=True))
            if bz is not None and bz.res_kind=="arg": b=bz; merge_via_z=True
        if b is None: skip(f"no_fixup_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("fixup_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        tbl_vt=f"int{bits}x{width//bits}"; tbl_c={512:"__m512i",256:"__m256i",128:"__m128i"}[width]
        caml=f"caml_avx512_{vt}_fixupimm"+("_round" if rnd else "")+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]; nv=0
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(vt,vc)})
            else:
                nv+=1
                specs.append({"kind":"vec",**make_spec(tbl_vt if nv==3 else vt,
                    tbl_c if nv==3 else vc)})
        e=dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=255, bake_imm=None,
            sae_imm=rnd, oracle_imm=(["{V}","8"] if rnd else None),
            argspecs=specs, out=make_spec(vt,vc))
        if merge_via_z: e.update(force_z="false", oracle_mask_pos=1)
        emitted.append(e)
        continue
    # ---- multiply-add adjacent pairs into double-width elements ----
    # vpmaddwd: int16 x int16 -> int32 ; vpmaddubsw: uint8 x int8 -> int16 (sat).
    # Both inputs share a C type (__m512i), only the element interpretation and
    # the output element size differ, so the bit-exact oracle still applies.
    if core in ("madd","maddubs"):
        if imm_params: skip(f"op_not_in_table:{core}", name); continue
        in_el = 16 if core=="madd" else 8
        in_tag="epi"+str(in_el); out_tag="epi"+str(in_el*2)
        in_vt=vectype(in_tag,width); in_c=c_vectype(in_tag,width)
        out_vt=vectype(out_tag,width); out_c=c_vectype(out_tag,width)
        b=pick_binding(mnem, width, masking)
        if b is None: skip(f"no_madd_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("madd_arity", name); continue
        opname="madd" if core=="madd" else "maddubs"
        caml=f"caml_avx512_{in_vt}_{opname}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(out_vt,out_c)})
            else: specs.append({"kind":"vec",**make_spec(in_vt,in_c)})
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=width//(in_el*2), imm_max=None, argspecs=specs, out=make_spec(out_vt,out_c)))
        continue
    # ---- pack two vectors into one with half-width elements (saturating) ----
    if core in ("packs","packus"):
        _,bits,isf,isu=SUFFIX[tag]
        if isf or bits not in (16,32) or imm_params:
            skip(f"op_not_in_table:{core}", name); continue
        out_tag="epi"+str(bits//2)
        in_vt=vectype(tag,width); in_c=c_vectype(tag,width)
        out_vt=vectype(out_tag,width); out_c=c_vectype(out_tag,width)
        b=pick_binding(mnem, width, masking)
        if b is None: skip(f"no_pack_binding:{mnem}/{width}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("pack_arity", name); continue
        extra="_saturating_unsigned" if core=="packus" else "_saturating"
        caml=f"caml_avx512_cvt_{in_vt}_{out_vt}{extra}"+masksuffix(masking)
        add(name,caml,b,mnem,masking,mask_lanes(out_tag,width),None,in_vt,in_c,out_vt,out_c)
        continue
    # ---- embedded-rounding arithmetic: <op>_round, rounding via imm 8..11 ----
    if core.endswith("_round") and core[:-6] in ("add","sub","mul","div","sqrt","scalef"):
        base=core[:-6]; _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip(f"op_not_in_table:{core}", name); continue
        b=pick_round_binding(mnem, width, masking, "~rnd")
        if b is None: skip(f"no_round_binding:{mnem}/{masking}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_{OP[base]}_round"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=11, bake_imm=None, round_dispatch=True,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- suppress-all-exceptions arithmetic: min/max/getexp _round (sae only) ----
    if core.endswith("_round") and core[:-6] in ("max","min","getexp"):
        base=core[:-6]; _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip(f"op_not_in_table:{core}", name); continue
        b=pick_round_binding(mnem, width, masking, "~sae")
        if b is None: skip(f"no_sae_binding:{mnem}/{masking}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_{OP[base]}_round"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=8, bake_imm=None, sae_dispatch=True,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- FMA with embedded rounding (fmadd_round etc.) ----
    if core.endswith("_round") and core[:-6] in FMA:
        base=core[:-6]; _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip(f"op_not_in_table:{core}", name); continue
        # Same form selection as the non-round FMA path (132 for unmasked/maskz/
        # mask, 231 for mask3); the masked variants are the _K (zero-masking)
        # ~rnd binding used with ~z:false for merge.
        form = "231" if masking=="mask3" else "132"
        mnem=next((i for (i,_) in it["insts"] if i and form in i), None)
        if mnem is None: skip(f"no_fma_{form}:{core}", name); continue
        want_mask = masking in ("maskz","mask","mask3")
        b=None
        for cand in by_key.get(mnem.upper(), []):
            if cand.width!=width or cand.merge: continue
            if (any(a.enc=="Mask" for a in cand.args))!=want_mask: continue
            if "~rnd" not in cand.labels.split(): continue
            rm=[a for a in cand.args if a.enc=="RM_rm"]
            if rm and vreg(width) in rm[0].locs and full_mem(width) not in rm[0].locs:
                b=cand; break
        if b is None: skip(f"no_fmaround_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("fmaround_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_{FMA[base]}_round"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        e=dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=11, bake_imm=None,
            round_dispatch=True,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc))
        if masking=="mask3":
            e.update(rot3=True, force_z="false", oracle_mask_pos=3)
        else:
            e["swap12"]=True
            if masking=="mask": e.update(force_z="false", oracle_mask_pos=1)
        emitted.append(e)
        continue
    # ---- fused multiply-add: a*b (+/-) c, three same-type inputs ----
    if core in FMA:
        _,bits,isf,isu=SUFFIX[tag]
        if not isf: skip(f"op_not_in_table:{core}", name); continue
        # Match the C compiler's FMA form so the chosen instruction -- and hence
        # the NaN payload -- agrees with the oracle bit-for-bit:
        #   unmasked / maskz / mask -> 132 (dst = a, the 1st multiplicand);
        #   mask3                   -> 231 (dst = c, the addend).
        # 132 computes dst*rm + vvvv, so a*b+c needs operands (a, c, b) -> the
        # arm swaps the 2nd/3rd args (swap12). 231 computes vvvv*rm + dst, so
        # a*b+c merging into c needs (c, a, b) -> the arm rotates (rot3). The
        # masked forms reuse the zero-masking (_K) binding -- FMA's result is
        # already tied to its dst (res = Arg [|0|]) -- with ~z:false for merge.
        form = "231" if masking=="mask3" else "132"
        mnem=next((i for (i,_) in it["insts"] if i and form in i), None)
        if mnem is None: skip(f"no_fma_{form}:{core}", name); continue
        bmask = "maskz" if masking in ("maskz","mask","mask3") else "none"
        b=pick_binding(mnem, width, bmask)
        if b is None: skip(f"no_fma_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("fma_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_{FMA[core]}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        e=dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width),imm_max=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc),out=make_spec(vt,vc))
        if masking=="mask3":
            e.update(rot3=True, force_z="false", oracle_mask_pos=3)
        else:
            e["swap12"]=True
            if masking=="mask": e.update(force_z="false", oracle_mask_pos=1)
        emitted.append(e)
        continue
    # ---- permute by an integer control vector (permutexvar / permutevar) ----
    if core in ("permutexvar","permutevar"):
        _,bits,isf,isu=SUFFIX[tag]
        # permutevar is a vpermilps/pd op: the integer-data form is documented by
        # Intel but absent from immintrin.h, so it has no C oracle -> skip it.
        if core=="permutevar" and not isf:
            skip("permutevar_int_no_oracle", name); continue
        b=pick_binding(mnem, width, masking)
        if b is None: skip(f"no_permv_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("permv_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        idx_vt=f"int{bits}x{width//bits}"; idx_c={512:"__m512i",256:"__m256i",128:"__m128i"}[width]
        caml=f"caml_avx512_{vt}_{core}"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        # permutexvar's control is the 1st vector input, permutevar's is the 2nd
        int_pos = 1 if core=="permutexvar" else 2
        specs=[]; nv=0
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(vt,vc)})
            else:
                nv+=1
                specs.append({"kind":"vec",**make_spec(idx_vt if nv==int_pos else vt,
                    idx_c if nv==int_pos else vc)})
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=None, argspecs=specs, out=make_spec(vt,vc)))
        continue
    # ---- two-source permute (a, idx, b); idx is always an integer vector ----
    if core=="permutex2var" and masking=="mask2":
        # mask2: the result merges into the index operand, so use the vpermi2
        # form whose dst IS the index (res = Arg[0]); ~z:false, and the arm swaps
        # (a, idx) so idx lands in the dst slot.
        _,bits,isf,isu=SUFFIX[tag]
        mnemi2=next((i for (i,_) in it["insts"] if i and "I2" in i.upper()), None)
        if mnemi2 is None: skip(f"no_perm2i_mnem:{core}", name); continue
        b=pick_binding(mnemi2, width, "maskz")
        if b is None or b.res_kind!="arg": skip(f"no_perm2i_binding:{mnemi2}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("perm2i_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        idx_vt=f"int{bits}x{width//bits}"; idx_c={512:"__m512i",256:"__m256i",128:"__m128i"}[width]
        caml=f"caml_avx512_{vt}_permutex2var_mask2"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        # external/oracle order is Intel's (a, idx, b); the arm swaps to the
        # binding order (idx, a, b).
        specs=[{"kind":"vec",**make_spec(vt,vc)},
               {"kind":"vec",**make_spec(idx_vt,idx_c)},
               {"kind":"vec",**make_spec(vt,vc)},
               {"kind":"mask","ml":"mask","c":"int64","nl":0}]
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnemi2,masking="mask2",
            lanes=mask_lanes(tag,width), imm_max=None, argspecs=specs, out=make_spec(vt,vc),
            swap01=True, force_z="false", oracle_mask_pos=2))
        continue
    if core=="permutex2var":
        _,bits,isf,isu=SUFFIX[tag]
        mnemt2=next((i for (i,_) in it["insts"] if i and "T2" in i.upper()), None)
        if mnemt2 is None: skip(f"op_not_in_table:{core}", name); continue
        # vpermt2*'s result reuses its dst (the 1st table) as a read input
        # (res = Arg[0]); the merge form is the _K binding with ~z:false.
        b=pick_binding(mnemt2, width, masking)
        merge_via_z=False
        if b is None and masking=="mask":
            bz=pick_binding(mnemt2, width, "maskz")
            if bz is not None and bz.res_kind=="arg": b=bz; merge_via_z=True
        if b is None: skip(f"no_perm2_binding:{mnemt2}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=3: skip("perm2_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        idx_vt=f"int{bits}x{width//bits}"; idx_c={512:"__m512i",256:"__m256i",128:"__m128i"}[width]
        caml=f"caml_avx512_{vt}_permutex2var"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]; nv=0
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(vt,vc)})
            else:
                nv+=1
                specs.append({"kind":"vec",**make_spec(idx_vt if nv==2 else vt,
                    idx_c if nv==2 else vc)})
        e=dict(intel=name,caml=caml,binding=b,mnem=mnemt2,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=None, argspecs=specs, out=make_spec(vt,vc))
        if merge_via_z: e.update(force_z="false", oracle_mask_pos=1)
        emitted.append(e)
        continue
    # ---- blend: result = k ? b : a (the mask is a selector, not a writemask) ----
    if core=="blend":
        if masking!="mask": skip(f"op_not_in_table:{core}", name); continue
        b=pick_binding(mnem, width, "maskz")  # vpblendm* _K form (res=Res, mask)
        if b is None: skip(f"no_blend_binding:{mnem}", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_blend"
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking="none",
            lanes=mask_lanes(tag,width), imm_max=None, force_z="false",
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- within-lane float shuffle (vshufps/vshufpd): 2 inputs + imm8.  Named
    # _shuffle_inlane to avoid colliding with the 128-bit-lane shuffle_f32x4. ----
    if core=="shuffle" and tag in ("ps","pd"):
        b=pick_binding(mnem, width, masking, want_imm=True)
        if b is None: skip(f"no_shuffle_binding:{mnem}/{masking}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("shuffle_arity", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        caml=f"caml_avx512_{vt}_shuffle_inlane"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=imm_max_of(imm_params), bake_imm=None,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    # ---- vpsadbw: sum of absolute differences of bytes into 64-bit lanes ----
    if core=="sad":
        if imm_params or masking!="none": skip(f"op_not_in_table:{core}", name); continue
        in_vt=vectype("epu8",width); in_c=c_vectype("epu8",width)
        out_vt=vectype("epi64",width); out_c=c_vectype("epi64",width)
        b=pick_binding(mnem, width, "none")
        if b is None: skip(f"no_sad_binding:{mnem}/{width}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=2: skip("sad_arity", name); continue
        add(name,f"caml_avx512_{in_vt}_sad",b,mnem,"none",width//64,None,in_vt,in_c,out_vt,out_c)
        continue
    # ---- vpcompress{d,q,ps,pd}: pack mask-selected lanes into a register (a
    # masked-only unary; the source is RM_r, the result is the register dst, so
    # the generic 2-input handler's RM_rm-input picker does not apply).  maskz
    # uses the _K binding (res=res) with ~z:true; merge uses the _K_merge
    # binding (res=arg, dst reused as merge-src). ----
    if core=="compress":
        if masking not in ("mask","maskz"): skip("compress_unmasked", name); continue
        vt=vectype(tag,width); vc=c_vectype(tag,width)
        if masking=="maskz":
            b=next((x for x in by_key.get(mnem.upper(),[])
                    if x.width==width and not x.merge and x.res_kind=="res"
                    and any(a.enc=="Mask" for a in x.args)
                    and not any(a.enc=="RM_rm" for a in x.args)), None)
        else:
            b=next((x for x in by_key.get(mnem.upper(),[])
                    if x.width==width and x.merge and x.res_kind=="arg"
                    and any(a.enc=="Mask" for a in x.args)), None)
        if b is None: skip(f"no_compress_binding:{mnem}/{masking}", name); continue
        caml=f"caml_avx512_{vt}_compress"+masksuffix(masking)
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        specs=[]
        for rk,_ in arg_roles(b):
            if rk=="mask": specs.append({"kind":"mask","ml":"mask","c":"int64","nl":0})
            elif rk=="merge": specs.append({"kind":"merge",**make_spec(vt,vc)})
            else: specs.append({"kind":"vec",**make_spec(vt,vc)})
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=None, bake_imm=None,
            argspecs=specs, out=make_spec(vt,vc)))
        continue
    # ---- regular lane-wise op ----
    if core not in OP: skip(f"op_not_in_table:{core}", name); continue
    _,bits,isf,isu=SUFFIX[tag]
    opname=OP[core]
    if isu and core not in ALWAYS_UNSIGNED: opname+="_unsigned"
    vt=vectype(tag,width); vc=c_vectype(tag,width)
    caml=f"caml_avx512_{vt}_{opname}"+masksuffix(masking)
    b=pick_binding(mnem,width,masking,want_imm=bool(imm_params))
    # Ops whose result reuses the destination as the 1st input (res = Arg[0],
    # e.g. ternarylogic) get no _K_merge binding; their merge form is the _K
    # (zero-masking) binding used with ~z:false, merging into operand 1. Intel
    # then places the writemask right after that operand (oracle_mask_pos = 1).
    merge_via_z=False
    if b is None and masking=="mask":
        bz=pick_binding(mnem,width,"maskz",want_imm=bool(imm_params))
        if bz is not None and bz.res_kind=="arg": b=bz; merge_via_z=True
    if b is None: skip(f"no_binding:{mnem}/{width}/{masking}", name); continue
    has_imm = b.imm in ("Imm_spec","Imm_reg")
    if (len(imm_params)>0) != has_imm: skip(f"imm_mismatch:{core}", name); continue
    imm_max = imm_max_of(imm_params)
    # For merge_via_z the binding's 1st operand is both the merge target and a
    # compute input, so the start=1 input list undercounts -- the maskz binding
    # it was cloned from already validated arity/uniformity.
    if not merge_via_z:
        if not uniform(it, masking, vc): skip(f"mixed_type:{core}", name); continue
        if sum(1 for r in arg_roles(b) if r[0]=="vec")!=len(vec_input_types(it, masking)):
            skip(f"arity_mismatch:{core}", name); continue
    if merge_via_z:
        if caml in seen_caml: skip("dup_caml_name", name); continue
        seen_caml.add(caml)
        emitted.append(dict(intel=name,caml=caml,binding=b,mnem=mnem,masking=masking,
            lanes=mask_lanes(tag,width), imm_max=imm_max, bake_imm=None,
            force_z="false", oracle_mask_pos=1,
            argspecs=build_specs(arg_roles(b),vt,vc,vt,vc), out=make_spec(vt,vc)))
        continue
    add(name,caml,b,mnem,masking,mask_lanes(tag,width),imm_max,vt,vc,vt,vc)

# ---- encode each intrinsic's AVX512 feature subset (f/dq/bw/cd) in its name:
# caml_avx512_<...> -> caml_avx512<ext>_<...>.  The width (VL) is already carried
# by the lane-count in the type, so VL is not a separate name component. ----
CPS = {it["name"]: it["cps"] for it in intrinsics}
def ext_of(cps):
    if "AVX512DQ" in cps: return "dq"
    if "AVX512BW" in cps: return "bw"
    if "AVX512CD" in cps: return "cd"
    return "f"
for e in emitted:
    ext = ext_of(CPS[e["intel"]])
    e["caml"] = e["caml"].replace("caml_avx512_", f"caml_avx512{ext}_", 1)

# ================= EMIT =================
def ml_id(e): return e["caml"][len("caml_"):]
def c_id(e): return "c_"+ml_id(e)
def mmask(lanes): return "__mmask8" if lanes<=8 else f"__mmask{lanes}"

ROUND_DIRS=[(8,"Rnd_near"),(9,"Rnd_down"),(10,"Rnd_up"),(11,"Rnd_zero")]
def imm_values(e):
    if e.get("round_dispatch"): return [v for v,_ in ROUND_DIRS]
    if e.get("sae_dispatch"): return [8]  # _MM_FROUND_NO_EXC
    M=e["imm_max"]; return sorted({0,1,M//2,M})

def emit_arm(e):
    b=e["binding"]
    if e.get("force_z") is not None:
        bexpr=f"({b.name} ~z:{e['force_z']})"
    else:
        bexpr=f"({b.name} ~z:true)" if e["masking"]=="maskz" else b.name
    if e.get("round_dispatch"):
        z=(" ~z:true" if e["masking"]=="maskz"
           else " ~z:false" if e.get("force_z")=="false" else "")
        lines=[f'    | "{e["caml"]}" ->',
               f'      let i, args = extract_constant args ~max:11 op in']
        if e.get("rot3"):  # FMA mask3: rotate so the addend (c) becomes dst (231)
            lines.append('      (match args, i with')
            for v,rnd in ROUND_DIRS:
                lines.append(f'      | a :: b :: c :: rest, {v} -> '
                             f'instr ({b.name} ~rnd:{rnd}{z}) (c :: a :: b :: rest)')
            lines.append('      | _ -> None)')
        elif e.get("swap12"):  # FMA: also swap the 2nd/3rd operands (132 form)
            lines.append('      (match args, i with')
            for v,rnd in ROUND_DIRS:
                lines.append(f'      | dst :: x :: y :: rest, {v} -> '
                             f'instr ({b.name} ~rnd:{rnd}{z}) (dst :: y :: x :: rest)')
            lines.append('      | _ -> None)')
        else:
            lines.append('      (match i with')
            for v,rnd in ROUND_DIRS:
                lines.append(f'      | {v} -> instr ({b.name} ~rnd:{rnd}{z}) args')
            lines.append('      | _ -> None)')
        return "\n".join(lines)
    if e.get("rot3"):
        # FMA mask3: rotate (a, b, c) -> (c, a, b) so the addend c is the dst
        return (f'    | "{e["caml"]}" ->\n'
                f'      (match args with\n'
                f'      | a :: b :: c :: rest -> instr {bexpr} (c :: a :: b :: rest)\n'
                f'      | _ -> None)')
    if e.get("swap01"):
        # permutex2var mask2: swap (a, idx) -> (idx, a) so idx is the dst (vpermi2)
        return (f'    | "{e["caml"]}" ->\n'
                f'      (match args with\n'
                f'      | a :: b :: rest -> instr {bexpr} (b :: a :: rest)\n'
                f'      | _ -> None)')
    if e.get("sae_dispatch"):
        z=" ~z:true" if e["masking"]=="maskz" else ""
        return (f'    | "{e["caml"]}" ->\n'
                f'      let _i, args = extract_constant args ~max:255 op in\n'
                f'      instr ({b.name} ~sae:true{z}) args')
    if e.get("sae_imm"):  # control imm8 is used, plus embedded SAE
        z=(" ~z:true" if e["masking"]=="maskz"
           else " ~z:false" if e.get("force_z")=="false" else "")
        return (f'    | "{e["caml"]}" ->\n'
                f'      let i, args = extract_constant args ~max:{e["imm_max"]} op in\n'
                f'      instr ({b.name} ~sae:true{z}) ~i args')
    if e.get("swap12"):
        # swap the 2nd and 3rd operands (FMA 132 encoding: a*b+c -> (a, c, b))
        return (f'    | "{e["caml"]}" ->\n'
                f'      (match args with\n'
                f'      | dst :: x :: y :: rest -> instr {bexpr} (dst :: y :: x :: rest)\n'
                f'      | _ -> None)')
    bake=e.get("bake_imm")
    if bake is not None:
        line=f'    | "{e["caml"]}" -> instr {bexpr} ~i:{bake} args'
        return line if len(line)<=80 else f'    | "{e["caml"]}" ->\n      instr {bexpr} ~i:{bake} args'
    if e["imm_max"] is not None:
        return (f'    | "{e["caml"]}" ->\n'
                f'      let i, args = extract_constant args ~max:{e["imm_max"]} op in\n'
                f'      instr {bexpr} ~i args')
    line=f'    | "{e["caml"]}" -> instr {bexpr} args'
    # match ocamlformat: wrap arms wider than the 80-column margin
    if len(line)>80:
        return f'    | "{e["caml"]}" ->\n      instr {bexpr} args'
    return line

def emit_external(e):
    specs=e["argspecs"]; out=e["out"]
    oracle_ret="int64" if out.get("mask_result") else out["ml"]
    octys=[("int64" if s["kind"]=="mask" else s["ml"]) for s in specs]
    octy=" -> ".join(octys+[oracle_ret])
    if e["imm_max"] is None:
        tys=[("mask" if s["kind"]=="mask" else s["ml"]) for s in specs]
        s=(f'external {ml_id(e)} : {" -> ".join(tys+[out["ml"]])}\n'
           f'  = "caml_vec512_unreachable" "{e["caml"]}"\n[@@noalloc] [@@unboxed] [@@builtin]\n')
        s+=(f'external {c_id(e)} : {octy}\n  = "" "{c_id(e)}"\n[@@noalloc] [@@unboxed]\n')
        return s
    parts=["(int[@untagged])"]+[f'({"mask" if s["kind"]=="mask" else s["ml"]}[@unboxed])' for s in specs]
    parts.append(f'({out["ml"]}[@unboxed])')
    s=(f'external {ml_id(e)} : {" -> ".join(parts)}\n'
       f'  = "caml_vec512_unreachable" "{e["caml"]}"\n[@@noalloc] [@@builtin]\n')
    for V in imm_values(e):
        s+=(f'external {c_id(e)}_i{V} : {octy}\n  = "" "{c_id(e)}_i{V}"\n[@@noalloc] [@@unboxed]\n')
    return s

def emit_c_oracle(e):
    specs=e["argspecs"]; lanes=e["lanes"]
    params=[]; tagged=[]
    for i,s in enumerate(specs):
        params.append(f'{("int64_t" if s["kind"]=="mask" else s["c"])} p{i}')
        tagged.append((s["kind"],f"p{i}"))
    merge=[n for k,n in tagged if k=="merge"]
    mask=[f"({mmask(lanes)})"+n for k,n in tagged if k=="mask"]
    inputs=[n for k,n in tagged if k in ("vec","gpint")]
    is_mr=e["out"].get("mask_result")
    ret="int64_t" if is_mr else e["out"]["c"]; pl=", ".join(params)
    cast="(int64_t)" if is_mr else ""
    # Most masked intrinsics take (merge-src, writemask, inputs...); a few (FMA
    # mask/mask3) place the writemask at a non-leading position among the inputs.
    mpos=e.get("oracle_mask_pos")
    call_args = (inputs[:mpos]+mask+inputs[mpos:]) if mpos is not None else (merge+mask+inputs)
    if e["imm_max"] is None:
        return f'{ret} {c_id(e)}({pl})\n{{\n    return {cast}{e["intel"]}({", ".join(call_args)});\n}}\n'
    oracle_imm=e.get("oracle_imm")
    o=""
    for V in imm_values(e):
        ia=[s.format(V=V) for s in oracle_imm] if oracle_imm else [str(V)]
        o+=(f'{ret} {c_id(e)}_i{V}({pl})\n{{\n'
            f'    return {cast}{e["intel"]}({", ".join(call_args+ia)});\n}}\n')
    return o

def emit_test(e):
    specs=e["argspecs"]; out=e["out"]; lanes=e["lanes"]; oml=out["ml"]
    setup=[]; cb=[]; cc=[]; vi=0; mi=0; gi=0
    for s in specs:
        if s["kind"]=="mask":
            kv=f'k{mi}'; mi+=1
            setup.append(f'    let {kv} = rand_mask {lanes} in')
            cb.append(f'(mask_of_int64 {kv})'); cc.append(kv)
        elif s["kind"]=="gpint":
            nm=f'g{gi}'; gi+=1
            rv=('Int64.to_int32 (rand_bits ())' if s["ml"]=="int32" else 'rand_bits ()')
            setup.append(f'    let {nm} = {rv} in')
            cb.append(nm); cc.append(nm)
        else:
            nm=f'v{vi}'; vi+=1
            setup.append(f'    let {nm} = {s["ml"]}_of_bits '+" ".join("(rand_bits ())" for _ in range(s["nl"]))+' in')
            cb.append(nm); cc.append(nm)
    cbs=" ".join(cb); ccs=" ".join(cc)
    if out.get("mask_result"):
        fn="cmp1"; gw=lambda c: f"(int64_of_mask ({c}))"; ow=lambda c: f"({c})"
    elif out.get("kind")=="gpint":
        # GP-int result (scalar float->int convert): compare as int64
        fn="cmpg"
        conv=(lambda c: f"(Int64.of_int32 ({c}))") if out["bits"]==32 else (lambda c: f"({c})")
        gw=ow=conv
    else:
        fn="cmp"; gw=lambda c: f"(lanes_{oml} ({c}))"; ow=gw
    if e["imm_max"] is None:
        checks=[f'{fn} "{e["caml"]}" {gw(ml_id(e)+" "+cbs)} {ow(c_id(e)+" "+ccs)}']
    else:
        checks=[f'{fn} "{e["caml"]}#{V}" {gw(f"{ml_id(e)} {V} {cbs}")} '
                f'{ow(f"{c_id(e)}_i{V} {ccs}")}' for V in imm_values(e)]
    body="".join(s+"\n" for s in setup)+"    "+";\n    ".join(checks)
    return f"let () =\n  for _i = 1 to iters do\n"+body+"\n  done\n"

def emit_vt_helpers(vt):
    nl=width_of_vt(vt)//64; w=width_of_vt(vt)
    s=f'external {vt}_of_bits : '+("int64 -> "*nl)+f'{vt} = "" "vec{w}_of_int64s" [@@noalloc] [@@unboxed]\n'
    for i in range(nl):
        s+=f'external {vt}_w{i} : {vt} -> int64 = "" "vec{w}_w{i}" [@@noalloc] [@@unboxed]\n'
    s+=f'let lanes_{vt} v = [| '+"; ".join(f"{vt}_w{i} v" for i in range(nl))+" |]\n"
    return s

C_VEC_HELPERS = r'''
static int64_t ex256(__m256i v, int i){ int64_t t[4]; _mm256_storeu_si256((void*)t, v); return t[i]; }
int64_t vec256_w0(__m256i v){return ex256(v,0);} int64_t vec256_w1(__m256i v){return ex256(v,1);}
int64_t vec256_w2(__m256i v){return ex256(v,2);} int64_t vec256_w3(__m256i v){return ex256(v,3);}
__m256i vec256_of_int64s(int64_t a,int64_t b,int64_t c,int64_t d){ return _mm256_set_epi64x(d,c,b,a); }
static int64_t ex128(__m128i v, int i){ int64_t t[2]; _mm_storeu_si128((void*)t, v); return t[i]; }
int64_t vec128_w0(__m128i v){return ex128(v,0);} int64_t vec128_w1(__m128i v){return ex128(v,1);}
__m128i vec128_of_int64s(int64_t a,int64_t b){ return _mm_set_epi64x(b,a); }
'''

ML_PRELUDE = '''open Stdlib

(* GENERATED by tools/avx512gen/generate.py -- do not edit by hand.
   Each OxCaml AVX512 builtin (lowered to an inline EVEX instruction) is compared
   bit-for-bit against a C oracle calling the very same Intel intrinsic, so
   agreement is exact for every input class.  Success prints nothing. *)

let iters = %d

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64" [@@noalloc] [@@unboxed] [@@builtin]

let seed = ref 0x9e3779b9
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
'''

def write_files():
    arms="\n".join(emit_arm(e) for e in emitted)
    # splice into simd_selection.ml
    txt=open(SELECTION).read()
    B="    (* BEGIN GENERATED AVX512 *)\n"; E="    (* END GENERATED AVX512 *)\n"
    i=txt.index(B)+len(B); j=txt.index(E)
    open(SELECTION,"w").write(txt[:i]+arms+"\n"+txt[j:])
    # canonicalize the spliced arms so the file stays ocamlformat-clean
    try:
        subprocess.run(["ocamlformat","-i",SELECTION], check=True, capture_output=True)
    except Exception as ex:
        print(f"warning: could not ocamlformat {SELECTION}: {ex}")
    # C stubs
    fallbacks="\n".join(f'BUILTIN({e["caml"]});' for e in emitted)
    oracles="\n".join(emit_c_oracle(e) for e in emitted)
    cfile=("/* GENERATED by tools/avx512gen/generate.py -- do not edit. */\n"
        "#include <caml/simd.h>\n#include <stdint.h>\n#include <assert.h>\n"
        "#define BUILTIN(name) void name() { assert(0); }\n"
        +fallbacks+"\n#ifdef ARCH_AVX512\n#include <immintrin.h>\n"
        +C_VEC_HELPERS+"\n"+oracles+"\n#endif\n")
    open(os.path.join(TESTDIR,"ops512_gen_stubs.c"),"w").write(cfile)
    # OCaml test
    used=set()
    for e in emitted:
        for s in e["argspecs"]:
            if s["kind"]=="vec": used.add(s["ml"])   # gpint/mask are not vectors
        if e["out"].get("kind")!="gpint" and not e["out"].get("mask_result"):
            used.add(e["out"]["ml"])
    used=sorted(used, key=width_of_vt)
    ml=(ML_PRELUDE % ITERS)+"\n"
    if any(e["out"].get("mask_result") for e in emitted):
        ml+=('external int64_of_mask : mask -> int64\n'
             '  = "caml_vec512_unreachable" "caml_int64_of_mask" [@@noalloc] [@@unboxed] [@@builtin]\n'
             'let cmp1 name g e =\n'
             '  if Int64.compare g e <> 0 then Printf.printf "%s: got %016Lx exp %016Lx\\n" name g e\n')
    if any(e["out"].get("kind")=="gpint" for e in emitted):
        ml+=('let cmpg name g e =\n'
             '  if Int64.compare g e <> 0 then Printf.printf "%s: got %016Lx exp %016Lx\\n" name g e\n')
    ml+="".join(emit_vt_helpers(vt) for vt in used)+"\n"
    ml+="\n".join(emit_external(e) for e in emitted)+"\n"
    ml+="\n".join(emit_test(e) for e in emitted)
    open(os.path.join(TESTDIR,"ops512_gen.ml"),"w").write(ml)

def report():
    print(f"target AVX-512 intrinsics (CPUID subset of 5 exts): {len(intrinsics)}")
    print(f"EMITTED: {len(emitted)}   SKIPPED: {sum(skips.values())}")
    print("\n-- skip reasons --")
    for r,n in skips.most_common(40):
        print(f"  {n:5d}  {r}   e.g. {', '.join(skip_examples[r][:2])}")

if __name__=="__main__":
    report()
    if "--report" not in sys.argv:
        write_files()
        print(f"\nwrote {len(emitted)} intrinsics: simd_selection.ml arms + "
              f"ops512_gen.ml + ops512_gen_stubs.c")
