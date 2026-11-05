#!/usr/bin/env bash
# Read saved Codex response usage; requires Python 3, not a Codex executable.
set -euo pipefail
exec python3 - "$@" <<'PY'
import argparse
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys

parser = argparse.ArgumentParser(
    prog="codex-cache.sh",
    description="Show per-response token/cache counts from a Codex rollout.",
    epilog="Uses $CODEX_HOME (default ~/.codex). 'uncached' = input minus cache reads; "
           "cache writes are shown separately, not added to prompt_total. Model is "
           "the recorded turn-context model. Older token_count logs are best-effort.",
)
parser.add_argument("session", nargs="?", help="full session UUID or rollout JSONL path")
select = parser.add_mutually_exclusive_group()
select.add_argument("--latest", action="store_true", help="read the most recently modified rollout")
select.add_argument("--list", action="store_true", help="list rollout paths, newest first")
parser.add_argument("--home", type=Path, default=Path(os.environ.get("CODEX_HOME") or "~/.codex").expanduser())
parser.add_argument("--json", action="store_true", help="output rows as JSON (including timestamps and response IDs)")
args = parser.parse_args()
if bool(args.session) + bool(args.latest) + bool(args.list) != 1:
    parser.error("choose a session/path, --latest, or --list")


def fail(message):
    parser.exit(1, f"codex-cache: {message}\n")


def rollouts():
    files = set()
    for folder in (args.home / "sessions", args.home / "archived_sessions"):
        files.update(folder.rglob("rollout-*.jsonl"))
        files.update(folder.rglob("rollout-*.jsonl.zst"))
    return sorted(files, key=lambda p: (p.stat().st_mtime_ns, str(p)), reverse=True)


if args.session and Path(args.session).expanduser().is_file():
    path = Path(args.session).expanduser()
else:
    files = rollouts()
    if args.list:
        if not files:
            fail(f"no saved rollouts under {args.home}; run Codex first or use --home")
        for path in files:
            print(path)
        sys.exit(0)
    if args.latest:
        if not files:
            fail(f"no saved rollouts under {args.home}; run Codex first or supply a rollout path")
        path = files[0]
    else:
        if not re.fullmatch(r"[0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}", args.session):
            fail("expected a full session UUID or an existing rollout path")
        pattern = re.compile(r"-" + re.escape(args.session) + r"(?:_[0-9a-f-]+)?\.jsonl(?:\.zst)?$", re.I)
        matches = [p for p in files if pattern.search(p.name)]
        if not matches:
            fail(f"no rollout for {args.session} under {args.home}")
        if len(matches) != 1:
            fail("multiple rollouts match; pass an explicit path:\n" + "\n".join(map(str, matches)))
        path = matches[0]


def events(path):
    process = None
    if path.suffix == ".zst":
        if not shutil.which("zstd"):
            fail("reading compressed rollouts requires zstd")
        process = subprocess.Popen(["zstd", "-dc", "--", str(path)], stdout=subprocess.PIPE, text=True)
        stream = process.stdout
    else:
        stream = path.open(encoding="utf-8")
    try:
        for number, line in enumerate(stream, 1):
            if not line.strip():
                continue
            try:
                yield json.loads(line)
            except json.JSONDecodeError:
                # A live writer may not yet have finished the final JSON line.
                if not line.endswith("\n"):
                    print(f"codex-cache: ignoring unfinished final line {number}", file=sys.stderr)
                    continue
                fail(f"invalid JSON at {path}:{number}")
    finally:
        stream.close()
        if process is not None and process.wait() != 0:
            fail(f"could not decompress {path}")


def number(usage, key):
    value = usage.get(key)
    return value if type(value) is int else None


def row(event, usage, model, response_id=None):
    prompt = number(usage, "input_tokens")
    cached = number(usage, "cached_input_tokens")
    return dict(
        model=model,
        uncached=prompt - cached if prompt is not None and cached is not None else None,
        cache_read=cached,
        cache_write=number(usage, "cache_write_input_tokens"),
        prompt_total=prompt,
        cached_pct=round(100 * cached / prompt, 1) if prompt and cached is not None else None,
        output=number(usage, "output_tokens"),
        reasoning=number(usage, "reasoning_output_tokens"),
        total_tokens=number(usage, "total_tokens"),
        timestamp=event.get("timestamp"),
        response_id=response_id,
    )


direct, legacy = [], []
seen_responses = set()
previous_total = None
model = None
models_by_turn = {}
has_direct = False
try:
    for event in events(path):
        payload = event.get("payload") or {}
        kind = event.get("type")
        if kind == "turn_context":
            model = payload.get("model")
            if payload.get("turn_id"):
                models_by_turn[payload["turn_id"]] = model
        if kind == "token_usage_record":
            has_direct = True
            usage = payload.get("usage")
            if not isinstance(usage, dict):
                continue
            # Response IDs distinguish equal usage on different completions and
            # avoid counting the same response twice after history replay.
            response_id = payload.get("response_id")
            if response_id and response_id in seen_responses:
                continue
            if response_id:
                seen_responses.add(response_id)
            direct.append(row(event, usage, models_by_turn.get(payload.get("turn_id"), model), response_id))
        if kind == "event_msg" and payload.get("type") == "token_count":
            info = payload.get("info") or {}
            total = info.get("total_token_usage")
            if not isinstance(total, dict):
                continue
            # Older clients emit repeated token_count snapshots on rate-limit
            # updates. Only cumulative progress identifies a new usage sample.
            signature = json.dumps(total, sort_keys=True)
            if signature == previous_total:
                continue
            previous_total = signature
            usage = info.get("last_token_usage")
            if not isinstance(usage, dict):
                continue
            # Context-window estimates/resets are not provider completions.
            if not (number(usage, "input_tokens") or number(usage, "output_tokens")):
                continue
            legacy.append(row(event, usage, model))
except OSError as error:
    fail(str(error))

rows = direct if has_direct else legacy
source = "token_usage_record.usage" if has_direct else "token_count.info.last_token_usage (legacy, best-effort)"
print(f"Rollout: {path}\nUsage source: {source}", file=sys.stderr)
if not rows:
    fail("no completed-response usage found")
for call, item in enumerate(rows, 1):
    item["call"] = call
if args.json:
    print(json.dumps(rows, indent=2))
else:
    columns = ("call", "model", "uncached", "cache_read", "cache_write", "prompt_total", "cached_pct", "output", "reasoning", "total_tokens")
    table = [list(columns)] + [["-" if item[key] is None else str(item[key]) for key in columns] for item in rows]
    widths = [max(len(values[i]) for values in table) for i in range(len(columns))]
    for values in table:
        print("  ".join(value.ljust(width) for value, width in zip(values, widths)).rstrip())
PY
