#!/usr/bin/env python3
"""Extract speculative inlining decisions from Flambda 2 `.inlining.org` reports.

Walks the given directories for `*.inlining.org` files and writes a CSV with
one row per call-site decision taken after speculation (inlined, rejected,
aborted, or refused before speculating because of the budget), joined with
the callee's size as recorded at its definition.

  inlining_decisions.py DIR... OUT.csv

Handles the wording of both the threshold and the ratio criteria.
"""
import csv
import os
import re
import sys

TITLE_RE = re.compile(r'^(\**) (Module|Class|Unknown|Definition of|Application of) ?(.*)$')
UID_ANCHOR_RE = re.compile(r' <<([0-9a-f]{32})>>$')
DBG_RE = re.compile(r'^(.*?) at (\S+)$')
DEFINED_RE = re.compile(r'Defined \[\[(?:file:[^\]]*::)?([0-9a-f]{32})\]\[')
SIZE_RE = re.compile(r'Code size was estimated to be (-?\d+) \(x86-64\) / (-?\d+) \(arm64\)')
PASS_RE = re.compile(r'Decision taken \*(after closure conversion|before simplify|after simplify)\*')
BUDGET_RE = re.compile(r'Remaining speculative inlining budget of the enclosing inlined body: (-?[0-9.]+)')
NUM = r'(-?[0-9.]+)'
SZ = r'(-?\d+) \(x86-64\) / (-?\d+) \(arm64\)'
RM = (r'removed: \{call: (\d+) alloc: (\d+) prim: (\d+) branch: (\d+) direct: (\d+)'
      r' poly_cmp: (\d+) requested: (\d+)\}')
LIFTED = r'(?: \(of which lifted constants: size: ' + SZ + ' ' + RM + r'\))?'
# Threshold criterion (new wording, with size before inlining and call-site credit).
THRESHOLD_RE = re.compile(
    r'The function call has( not)? been inlined because the (function|functor) was'
    r'( not)? inlined after speculation as its cost metrics were=size: ' + SZ + ' ' + RM
    + r'(?: \(of which lifted constants: size: ' + SZ + ' ' + RM
    + r'; size before inlining ' + SZ + r'; call-site credit ' + NUM + r'\))?'
    + r', which was evaluated to ' + NUM + r' (<=|>) (threshold|remaining budget) ' + NUM)
# Ratio criterion.
RATIO_RE = re.compile(
    r'The function call has( not)? been inlined because the (function|functor) was'
    r'( not)? inlined after speculation: size before inlining ' + SZ
    + r', cost metrics after speculation=size: ' + SZ + ' ' + RM + LIFTED
    + r', call-site credit ' + NUM + r', bonus for removed operations ' + NUM
    + r', adjusted size ' + NUM + r', ratio ' + NUM + r' (<=|>) maximum ratio ' + NUM
    + r' \((threshold|remaining budget) ' + NUM + r'\)')
ABORTED_RE = re.compile(
    r'the speculation was aborted because the (threshold|remaining budget) \(' + NUM
    + r'\) was exhausted while simplifying the inlined body')
EXHAUSTED_RE = re.compile(
    r'the function was not speculated upon as its code size \(' + SZ + r'\) exceeds the maximum \('
    + NUM + r'\) allowed by the remaining speculative inlining budget \(' + NUM + r'\)')
FIELDS = ['caller_unit', 'caller_path', 'callee', 'dbg', 'callee_uid', 'outcome', 'criterion',
          'inlined', 'functor', 'orig_x86', 'orig_arm64', 'simp_x86', 'simp_arm64',
          'rm_call', 'rm_alloc', 'rm_prim', 'rm_branch', 'rm_direct', 'rm_poly',
          'lifted_x86', 'lifted_arm64', 'call_site_credit', 'bonus', 'adjusted_size',
          'evaluated', 'ratio', 'limit', 'limit_kind', 'threshold', 'remaining_budget',
          'callee_dbg', 'file']


def unit_of_file(path):
    base = os.path.basename(path).split('.', 1)[0]
    return base[:1].upper() + base[1:]


def classify_call_site(text):
    """(outcome, reason) of a call-site entry's first decision, for the
    per-callee tabulation of every call site, speculative or not."""
    if 'unavailable to do a direct call' in text and 'The function call' not in text:
        return 'unknown', 'unknown callee'
    m = re.search(r'The function call has( not)? been inlined because (.{0,160})', text)
    if not m:
        return 'other', 'no decision'
    outcome = 'not inlined' if m.group(1) else 'inlined'
    why = m.group(2)
    if 'after speculation' in why:
        reason = 'speculation'
    elif 'aborted' in why:
        reason = 'aborted'
    elif 'not speculated upon' in why:
        reason = 'budget refused'
    elif 'definition site' in why or 'small enough' in why:
        reason = 'definition'
    elif 'never be inlinable' in why:
        reason = 'never inlinable'
    elif 'no useful information' in why:
        reason = 'arguments not useful'
    elif 'maximum inlining depth' in why or 'recursion depth' in why or 'unrolling depth' in why:
        reason = 'depth'
    elif 'attribute' in why or 'unrolled' in why:
        reason = 'attribute'
    elif 'speculative inlining is in progress' in why:
        reason = 'inside speculation'
    elif 'could not be found' in why:
        reason = 'missing code'
    else:
        reason = 'other'
    return outcome, reason


def parse_file(path, fundecls, calls, unknown=None, callsites=None):
    unit = unit_of_file(path)
    with open(path, errors='replace') as f:
        text_all = f.read()
        lines = text_all.split('\n')
    if unknown is not None:
        # Call sites whose callee was not known, i.e. indirect calls.
        unknown[unit] = text_all.count('unavailable to do a direct call')
    entries, cur = [], None
    for line in lines:
        m = TITLE_RE.match(line)
        if m and (line.startswith('*') or line.startswith(' ')):
            stars, kind, rest = m.groups()
            uid = None
            um = UID_ANCHOR_RE.search(rest)
            if um:
                uid, rest = um.group(1), rest[:um.start()]
            dbg = ''
            dm = DBG_RE.match(rest)
            if dm and kind in ('Definition of', 'Application of'):
                rest, dbg = dm.group(1), dm.group(2)
            cur = [len(stars), kind, rest.strip(), dbg, uid, []]
            entries.append(cur)
        elif cur is not None:
            cur[5].append(line)
    stack = []
    for depth, kind, name, dbg, uid, body in entries:
        while stack and stack[-1][0] >= depth:
            stack.pop()
        prev = stack[-1][1] if stack else ''
        if kind == 'Module':
            printed = prev + name + '.'
        elif kind == 'Class':
            printed = prev + name + '#'
        elif kind == 'Unknown':
            printed = prev + '(???)'
        elif kind == 'Definition of':
            printed = prev + name
        else:
            printed = '(' + prev + '(calling ' + name + ') inlined)'
        stack.append((depth, printed))
        text = ' '.join(' '.join(body).split())
        if kind == 'Definition of':
            blocks = PASS_RE.split(text)
            size = None
            for i in range(1, len(blocks), 2):
                sm = SIZE_RE.search(blocks[i + 1])
                if sm:
                    size = (int(sm.group(1)), int(sm.group(2)))
            if size is not None:
                rec = {'unit': unit, 'path': unit + '::' + prev + name, 'dbg': dbg,
                       'x86': size[0], 'arm64': size[1],
                       'functor': "functor's body" in text or 'functor' in text.split('because', 1)[-1][:120]}
                if uid:
                    fundecls['uid'][uid] = rec
                fundecls['path'][unit + '::' + prev + name] = rec
        elif kind == 'Application of':
            if callsites is not None:
                callsites[(name, *classify_call_site(text))] += 1
            dm = DEFINED_RE.search(text)
            bm = BUDGET_RE.search(text)
            base = {'caller_unit': unit, 'caller_path': prev, 'callee': name, 'dbg': dbg,
                    'callee_uid': dm.group(1) if dm else None,
                    'remaining_budget': bm.group(1) if bm else '', 'file': path}
            for cm in THRESHOLD_RE.finditer(text):
                g = cm.groups()
                calls.append(dict(base, outcome='decided', criterion='threshold',
                    inlined=g[0] is None, functor=g[1] == 'functor',
                    simp_x86=g[3], simp_arm64=g[4],
                    rm_call=g[5], rm_alloc=g[6], rm_prim=g[7], rm_branch=g[8], rm_direct=g[9],
                    rm_poly=g[10], lifted_x86=g[12] or '', lifted_arm64=g[13] or '',
                    orig_x86_decl=g[21] or '', orig_arm64_decl=g[22] or '',
                    call_site_credit=g[23] or '', bonus='', adjusted_size='',
                    evaluated=g[24], ratio='', limit=g[27], limit_kind=g[26], threshold=g[27]))
            for cm in RATIO_RE.finditer(text):
                g = cm.groups()
                calls.append(dict(base, outcome='decided', criterion='ratio',
                    inlined=g[0] is None, functor=g[1] == 'functor',
                    orig_x86_decl=g[3], orig_arm64_decl=g[4], simp_x86=g[5], simp_arm64=g[6],
                    rm_call=g[7], rm_alloc=g[8], rm_prim=g[9], rm_branch=g[10], rm_direct=g[11],
                    rm_poly=g[12], lifted_x86=g[14] or '', lifted_arm64=g[15] or '',
                    call_site_credit=g[23], bonus=g[24], adjusted_size=g[25], evaluated='',
                    ratio=g[26], limit=g[28], limit_kind='maximum ratio',
                    threshold=g[30]))
            for cm in ABORTED_RE.finditer(text):
                calls.append(dict(base, outcome='aborted', criterion='', inlined=False,
                    functor='', limit=cm.group(2), limit_kind=cm.group(1), threshold=cm.group(2)))
            for cm in EXHAUSTED_RE.finditer(text):
                calls.append(dict(base, outcome='budget_exhausted', criterion='', inlined=False,
                    functor='', orig_x86_decl=cm.group(1), orig_arm64_decl=cm.group(2),
                    limit=cm.group(3), limit_kind='max code size', remaining_budget=cm.group(4)))


def collect(roots, out):
    """Parse every report under [roots] into the CSV [out]; return a summary."""
    from collections import Counter
    fundecls, calls, nfiles = {'uid': {}, 'path': {}}, [], 0
    unknown, callsites = {}, Counter()
    for root in roots:
        for d, _, files in os.walk(root):
            for fn in files:
                if fn.endswith('.inlining.org'):
                    nfiles += 1
                    parse_file(os.path.join(d, fn), fundecls, calls, unknown, callsites)
    with open(out.rsplit('.', 1)[0] + '.callsites.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['callee', 'outcome', 'reason', 'count'])
        for (callee, outcome, reason), n in sorted(callsites.items()):
            w.writerow([callee, outcome, reason, n])
    with open(out.rsplit('.', 1)[0] + '.unknown_calls.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['unit', 'unknown_callee_sites'])
        for u in sorted(unknown):
            w.writerow([u, unknown[u]])
    by_uid = by_path = unmatched = 0
    for c in calls:
        rec = None
        if c['callee_uid'] and c['callee_uid'] in fundecls['uid']:
            rec, by_uid = fundecls['uid'][c['callee_uid']], by_uid + 1
        elif c['callee'] in fundecls['path']:
            rec, by_path = fundecls['path'][c['callee']], by_path + 1
        else:
            unmatched += 1
        # Prefer the size printed in the decision itself (the metadata the
        # call site saw); fall back to the definition's recorded size.
        c['orig_x86'] = c.get('orig_x86_decl') or (rec['x86'] if rec else '')
        c['orig_arm64'] = c.get('orig_arm64_decl') or (rec['arm64'] if rec else '')
        c['callee_dbg'] = rec['dbg'] if rec else ''
        if c.get('functor') in ('', None) and rec:
            # Aborted and refused speculations do not say; use the definition.
            c['functor'] = rec['functor']
    with open(out, 'w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=FIELDS, extrasaction='ignore')
        w.writeheader()
        for c in calls:
            w.writerow({k: c.get(k, '') for k in FIELDS})
    outcomes = {}
    for c in calls:
        outcomes[c['outcome']] = outcomes.get(c['outcome'], 0) + 1
    return {'files': nfiles, 'decisions': len(calls), 'outcomes': outcomes,
            'matched_by_uid': by_uid, 'matched_by_path': by_path, 'unmatched': unmatched,
            'unknown_callee_sites': sum(unknown.values())}


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    summary = collect(sys.argv[1:-1], sys.argv[-1])
    print(' '.join(f'{k}={v}' for k, v in summary.items()))


if __name__ == '__main__':
    main()
