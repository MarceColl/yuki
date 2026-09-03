# yuuki agent instructions

Design: `docs/superpowers/specs/2026-09-03-yuuki-design.md`. Tasks: `docs/superpowers/plans/2026-09-03-yuuki.md`. Read the task you were given in full before writing anything.

## Toolchain

- SBCL 2.6.3 with Quicklisp at `~/quicklisp`. Fasls compile into `~/.cache/common-lisp`.
- `make test` runs the fiveam suite and exits non-zero on any failure. `make` builds `bin/yuuki.core`. Run the app only as `bin/yuuki`.
- No network in tests.

## Code rules

- History is a list of Responses API items as jzon `equal` hash tables with string keys. jzon parses `false` to `nil` and `null` to `'null`, and writes `nil` as `false`.
- The `yuuki` package is locked. The agent's own code lives in `yuuki-user`.
- Lisp kebab-case identifiers, docstrings on public functions, no emojis anywhere, no Markdown in model-facing text.
- Pure functions return new values; the reducer never mutates its input state.

## Working rules

- Touch only the files your task names. Other tasks run in parallel on other branches.
- Follow the task's steps in order: tests first, see them fail, implement, see them pass.
- Do not run `bd`. The orchestrator owns the tracker. Do not open pull requests.
- Commit with git on your branch once `make test` passes, using the task's commit message plus these trailers:

```
Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01Tq7hDzCXmv6bNU67gsavQm
```
