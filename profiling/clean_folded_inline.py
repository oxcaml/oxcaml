import re
import sys

UNKNOWN_PATTERN = re.compile(r'(?:;)?<unknown binary>`\?\?\?')


def clean_stack(stack: str) -> str:
    stack = UNKNOWN_PATTERN.sub('', stack)
    stack = stack.replace(';0x0', '')
    stack = re.sub(r';{2,}', ';', stack)
    return stack.strip(';')


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


def main() -> None:
    for raw in sys.stdin:
        raw = raw.rstrip('\n')
        if not raw:
            continue
        if ' ' in raw:
            stack, count = raw.rsplit(' ', 1)
        else:
            stack, count = raw, ''
        cleaned = clean_stack(stack)
        if not cleaned:
            continue
        frames = [part.strip() for part in cleaned.split(';') if part.strip()]
        if not frames:
            continue
        frames = dedup_recursive_towers(frames)
        result = ';'.join(frames)
        if not result:
            continue
        if count:
            sys.stdout.write(f"{result} {count}\n")
        else:
            sys.stdout.write(result + '\n')


if __name__ == '__main__':
    main()
