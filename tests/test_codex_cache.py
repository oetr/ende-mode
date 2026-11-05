"""Regression tests for counting Codex responses rather than usage snapshots."""
import json
from pathlib import Path
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "codex-cache.sh"


def usage(prompt, cached, output=10):
    return dict(input_tokens=prompt, cached_input_tokens=cached,
                cache_write_input_tokens=0, output_tokens=output,
                reasoning_output_tokens=2, total_tokens=prompt + output)


def event(kind, payload):
    return dict(timestamp="2026-11-05T12:00:00Z", type=kind, payload=payload)


def snapshot(last, total):
    return event("event_msg", dict(type="token_count", info=dict(
        last_token_usage=last, total_token_usage=total)))


class CodexCacheTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="codex-cache-")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name)
        self.path = self.home / "rollout.jsonl"

    def write(self, records, suffix=""):
        self.path.write_text("".join(json.dumps(r) + "\n" for r in records) + suffix)

    def run_script(self, *args):
        return subprocess.run(["bash", str(SCRIPT), "--home", str(self.home), *map(str, args)],
                              capture_output=True, text=True)

    def rows(self):
        result = self.run_script(self.path, "--json")
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_direct_records_keep_zeros_regressions_and_equal_usage(self):
        records = [event("turn_context", dict(turn_id="turn-1", model="gpt-test"))]
        for i, cached in enumerate([896, 0, 896, 896]):
            records.append(event("token_usage_record", dict(
                turn_id="turn-1", response_id=f"resp_{i}", usage=usage(1024, cached),
                turn_token_usage=usage(90000, 89000), thread_token_usage=usage(90000, 89000))))
            records.append(snapshot(usage(1024, cached), usage(90000 + i, 89000)))
        records.append(records[-2])  # same response persisted twice
        self.write(records)
        rows = self.rows()
        self.assertEqual([r["cache_read"] for r in rows], [896, 0, 896, 896])
        self.assertEqual([r["uncached"] for r in rows], [128, 1024, 128, 128])
        self.assertEqual(rows[0]["cached_pct"], 87.5)
        self.assertEqual(rows[0]["model"], "gpt-test")
        self.assertEqual(rows[-1]["call"], 4)

    def test_legacy_filters_duplicates_and_context_estimates(self):
        first = snapshot(usage(1024, 896), usage(1024, 896))
        zero = snapshot(usage(2048, 0), usage(3072, 896))
        equal = snapshot(usage(2048, 0), usage(5120, 896))
        reset = snapshot(dict(input_tokens=0, output_tokens=0, total_tokens=32000),
                         dict(input_tokens=0, output_tokens=0, total_tokens=32000))
        self.write([event("event_msg", dict(type="token_count", info=None)),
                    first, first, zero, zero, equal, reset, reset])
        self.assertEqual([r["cache_read"] for r in self.rows()], [896, 0, 0])

    def test_missing_cached_usage_is_not_invented_zero(self):
        value = usage(1024, 0)
        del value["cached_input_tokens"]
        del value["cache_write_input_tokens"]
        self.write([event("token_usage_record", dict(response_id="resp_1", usage=value))])
        row = self.rows()[0]
        self.assertIsNone(row["cache_read"])
        self.assertIsNone(row["cache_write"])
        self.assertIsNone(row["uncached"])

    def test_session_lookup_latest_and_list(self):
        session = "0195cda5-433d-7f9a-9d7b-a9f15b60c2e2"
        folder = self.home / "sessions" / "2026" / "11" / "05"
        folder.mkdir(parents=True)
        self.path = folder / f"rollout-2026-11-05T12-00-00-{session}.jsonl"
        self.write([event("token_usage_record", dict(response_id="resp_1", usage=usage(1024, 0)))])
        for selector in [session, "--latest"]:
            result = self.run_script(selector, "--json")
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(json.loads(result.stdout)[0]["prompt_total"], 1024)
        result = self.run_script("--list")
        self.assertEqual(result.stdout.strip(), str(self.path))

    def test_partial_final_line_and_malformed_complete_line(self):
        records = [event("token_usage_record", dict(response_id="resp_1", usage=usage(1024, 0)))]
        self.write(records, '{"type":')
        self.assertEqual(len(self.rows()), 1)
        self.write(records, '{"type":\n')
        result = self.run_script(self.path)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("invalid JSON", result.stderr)


if __name__ == "__main__":
    unittest.main()
