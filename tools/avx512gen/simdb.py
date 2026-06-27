"""Parse the generated simdgen instruction DB (tools/simdgen/amd64_simd_instrs.ml)
into a queryable index of bindings, so the AVX512 intrinsic generator can map
each Intel intrinsic to a concrete OxCaml instruction binding."""
import re, collections, os

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.normpath(os.path.join(HERE, "..", ".."))
INSTRS = os.path.join(REPO, "tools", "simdgen", "amd64_simd_instrs.ml")

Arg = collections.namedtuple("Arg", "enc locs")

class Binding:
    __slots__ = ("name","labels","mnem","exts","args","res_kind","res_locs",
                 "imm","evex","evex_b","merge","width")
    def __init__(self, name, labels, mnem, exts, args, res_kind, res_locs,
                 imm, evex, evex_b, merge, width):
        self.name=name; self.labels=labels; self.mnem=mnem; self.exts=exts
        self.args=args; self.res_kind=res_kind; self.res_locs=res_locs
        self.imm=imm; self.evex=evex; self.evex_b=evex_b; self.merge=merge
        self.width=width
    def __repr__(self):
        return (f"<{self.name} {self.mnem} {self.labels} "
                f"args={[(a.enc,a.locs) for a in self.args]} "
                f"res={self.res_kind} imm={self.imm} w={self.width} m={self.merge}>")

_arg_re = re.compile(r'\{ loc = (Temp \[\|[^\]]*\]|Pin \w+); enc = (\w+) \}')
def _parse_args(s):
    out=[]
    for loc, enc in _arg_re.findall(s):
        if loc.startswith("Temp"):
            locs = re.findall(r'[A-Z][A-Za-z0-9]*', loc[loc.index('|')+1:])
        else:
            locs = [loc.split()[1]]
        out.append(Arg(enc, locs))
    return out

def _width_of(args, res_loc_toks):
    toks = []
    for a in args: toks += a.locs
    toks += res_loc_toks
    if "ZMM" in toks or "M512" in toks: return 512
    if "YMM" in toks or "M256" in toks: return 256
    if "XMM" in toks or "M128" in toks: return 128
    return None

_bind_re = re.compile(r'^let (\w+)((?: ~\w+)*) = \{(.*?)\n\}', re.S | re.M)
def load():
    txt = open(INSTRS).read()
    binds = {}
    by_key = collections.defaultdict(list)
    for m in _bind_re.finditer(txt):
        name, labels, body = m.group(1), m.group(2).strip(), m.group(3)
        mnem = re.search(r'mnemonic = "([^"]*)"', body).group(1)
        ex = re.search(r'ext = \[\|([^\]]*)\|\]', body).group(1)
        exts = [e for e in ex.replace(' ','').split(';') if e]
        args_s = re.search(r'args = \[\|(.*?)\|\]\n', body, re.S)
        args = _parse_args(args_s.group(1)) if args_s else []
        res_s = re.search(r'\n  ; res = ([^\n]*)', body)
        rk="none"; res_locs=[]
        if res_s:
            r = res_s.group(1)
            if r.startswith("Res_none"): rk="none"
            elif r.startswith("Arg"): rk="arg"; res_locs=[int(x) for x in re.findall(r'\d+', r)]
            else: rk="res"; res_locs=_parse_args(r)
        imm = re.search(r'imm = (\w+)', body).group(1)
        evex = 'Evex' in body
        evex_b = 'evex_b = true' in body
        merge = name.endswith('_merge')
        res_loc_toks=[]
        if rk=="res":
            for a in res_locs: res_loc_toks += a.locs
        width=_width_of(args, res_loc_toks)
        b=Binding(name,labels,mnem,exts,args,rk,res_locs,imm,evex,evex_b,merge,width)
        binds[name]=b
        by_key[mnem.upper()].append(b)
    return binds, by_key

if __name__=="__main__":
    binds, by_key = load()
    print("total bindings:", len(binds))
