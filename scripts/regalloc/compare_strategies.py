#!/usr/bin/env python3

import pandas as pd

N = 7  # keep in sync with shell script

FOLDER = "scripts/regalloc"
STRATEGIES = ["default", "custom"]

for strategy in STRATEGIES:
    durations: list[float] = []
    spills_reloads = 0
    for n in range(1, N + 1):
        csv_file = f"{FOLDER}/compare_strategies_{strategy}_{n}.csv"
        df = pd.read_csv(csv_file, sep=";")
        durations.append(df["duration"].sum())
        spills_reloads = df["out_spills"].sum() + df["out_reloads"].sum()
    durations.remove(max(durations))
    duration = sum(durations) / len(durations)
    print(f"{strategy}: {duration:.2f}s / {spills_reloads} spills and reloads")
