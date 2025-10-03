import re
import sys

pattern = re.compile(r'(?:;)?<unknown binary>`\?\?\?')
GC_KEYWORDS = [
    "gc",
    "alloc",
    "oldify",
    "major_collection",
    "minor_heap",
    "mark_",
    "sweep",
    "empty_minor_heap",
    "do_some_marking",
    "bf_allocate",
]


def clean_stack(stack: str) -> str:
    stack = pattern.sub('', stack)
    stack = stack.replace(';0x0', '')
    stack = re.sub(r';{2,}', ';', stack)
    stack = stack.strip(';')
    return stack


def dedup_recursive_towers(frames: list[str]) -> list[str]:
    kept: list[str] = []
    for frame in frames:
        drop = False
        for idx, prev in enumerate(kept):
            if prev == frame and len(kept) - idx >= 2:
                drop = True
                break
        if not drop:
            kept.append(frame)
    return kept


def is_gc_frame(frame: str) -> bool:
    lower = frame.lower()
    return any(keyword in lower for keyword in GC_KEYWORDS)


def extract_gc_suffix(frames: list[str]) -> list[str]:
    for idx, frame in enumerate(frames):
        if is_gc_frame(frame):
            gc_frames = ['GC runtime']
            for gc_frame in frames[idx:]:
                if is_gc_frame(gc_frame):
                    gc_frames.append(gc_frame)
                else:
                    break
            return gc_frames
    return frames


def process_stack(stack: str) -> str:
    cleaned = clean_stack(stack)
    if not cleaned:
        return ''
    frames = [part.strip() for part in cleaned.split(';') if part.strip()]
    if not frames:
        return ''
    frames = dedup_recursive_towers(frames)
    frames = extract_gc_suffix(frames)
    return ';'.join(frames)


def main() -> None:
    for raw in sys.stdin:
        raw = raw.rstrip('\n')
        if not raw:
            continue
        if ' ' in raw:
            stack, count = raw.rsplit(' ', 1)
        else:
            stack, count = raw, ''
        processed = process_stack(stack)
        if not processed:
            continue
        if count:
            sys.stdout.write(f"{processed} {count}\n")
        else:
            sys.stdout.write(processed + '\n')


if __name__ == '__main__':
    main()
