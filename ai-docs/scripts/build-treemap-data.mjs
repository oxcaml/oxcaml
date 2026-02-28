#!/usr/bin/env node
"use strict";

import fs from "node:fs";
import path from "node:path";
import { execSync } from "node:child_process";

const repoRoot = process.argv[2] || process.cwd();
const outputPath =
  process.argv[3] || path.join(repoRoot, "ai-docs", "source-treemap-data.json");

const allowedExtensions = new Set(["ml", "mli", "mll", "mly", "c", "h", "cmm", "s", "asm"]);

const trackedFiles = execSync(`git -C ${JSON.stringify(repoRoot)} ls-files -z`, {
    encoding: "utf8",
  })
  .split("\u0000")
  .map((value) => value.trim())
  .filter(Boolean);

const rows = [];
for (const filePath of trackedFiles) {
  const extension = path.extname(filePath).replace(/^\./, "").toLowerCase();
  if (!allowedExtensions.has(extension)) continue;

  const absPath = path.join(repoRoot, filePath);
  try {
    const stat = fs.statSync(absPath);
    if (!stat.isFile()) continue;
    rows.push({
      path: filePath,
      bytes: stat.size,
      extension,
    });
  } catch (_err) {
    continue;
  }
}

rows.sort((a, b) => a.path.localeCompare(b.path));
fs.writeFileSync(outputPath, `${JSON.stringify(rows, null, 2)}\n`, "utf8");
console.log(`Generated ${rows.length} tracked source rows to ${outputPath}`);
