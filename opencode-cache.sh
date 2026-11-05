#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
    printf 'Usage: %s SESSION_ID\n' "$0" >&2
    exit 2
fi

session=$1
# Validate before inserting the ID into SQL.
if [[ ! $session =~ ^ses_[[:alnum:]]+$ ]]; then
    printf 'Invalid session ID: %s\n' "$session" >&2
    exit 2
fi

# Override with OPENCODE=/path/to/opencode if necessary.
opencode=${OPENCODE:-opencode}
if [[ $opencode == opencode ]] && ! command -v opencode >/dev/null 2>&1; then
    opencode="$HOME/.opencode/bin/opencode"
fi

if ! command -v "$opencode" >/dev/null 2>&1; then
    printf 'OpenCode not found. Set OPENCODE to its executable path.\n' >&2
    exit 1
fi

sql="
WITH usage AS (
  SELECT
    id,
    time_created,
    json_extract(data, '\$.providerID') AS provider,
    json_extract(data, '\$.modelID') AS model,
    json_extract(data, '\$.tokens.input') AS uncached,
    json_extract(data, '\$.tokens.cache.read') AS cached,
    json_extract(data, '\$.tokens.cache.write') AS cache_write,
    json_extract(data, '\$.finish') AS finish,
    json_extract(data, '\$.time.completed')
      - json_extract(data, '\$.time.created') AS elapsed_ms
  FROM message
  WHERE session_id = '$session'
    AND json_extract(data, '\$.role') = 'assistant'
)
SELECT
  ROW_NUMBER() OVER (ORDER BY time_created, id) AS call,
  provider,
  model,
  uncached,
  cached AS cache_read,
  cache_write,
  uncached + cached + cache_write AS prompt_total,
  ROUND(
    100.0 * cached /
    NULLIF(uncached + cached + cache_write, 0), 1
  ) AS cached_pct,
  ROUND(elapsed_ms / 1000.0, 1) AS seconds,
  finish
FROM usage
ORDER BY time_created, id;
"

"$opencode" db "$sql" --format tsv | column -t -s $'\t'
