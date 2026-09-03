# yuuki design

A Common Lisp coding agent derived from fx. It keeps fx's agent loop, fx's
Codex transport, and fx's inline terminal form factor. It replaces fx's
twenty tools with one: evaluate Lisp in the running image. The agent builds
whatever else it needs, and the image is saved on exit, so what it builds
stays.

Toolchain: SBCL 2.6.3, ASDF, Quicklisp (dist 2026-01-01). Libraries:
`dexador` (HTTP), `com.inuoe.jzon` (JSON), `bordeaux-threads`, `sb-posix`
and `sb-introspect` (contribs), `fiveam` (tests).

## Scope

In: one agent loop, one provider (OpenAI Codex over ChatGPT OAuth, reusing
fx's session file), one tool, ask/auto/yolo permissions, an inline
line-oriented TUI, image persistence.

Out, for later: Markdown rendering, mid-turn steering, other providers,
saved sessions, MCP, subagents, skills, a `deftool` registry.

## Shape

Three threads, one queue, one reducer, one renderer.

```
 stdin reader ──(:key b)──────┐
 SIGWINCH     ──(:resize)─────┤
 agent thread ──(:text d)     │      ┌──────────┐   state'    ┌────────┐
              ──(:call c)     ├────► │ reducer  │ ──────────► │ render │ ──► tty
              ──(:result r)   │      └──────────┘   effects   └────────┘
              ──(:approve c p)│            │
              ──(:done h)─────┘            └── start-turn / resolve p
```

The reducer is a pure function: state and event in, state and effects out.
State is a struct of immutable fields; every transition returns a copy. The
main thread pops events, reduces, runs effects, renders. Effects are five: start a turn, answer an approval, cancel, evaluate a
slash line, exit.

The agent thread runs one turn as a plain function: history in, history
out. Everything it does to the outside world goes through one `emit`
callback that posts events, and one `approve` callback that blocks on a
promise the reducer fulfils.

## Wire: Codex

File `codex.lisp`. One function:

```lisp
(codex:stream-turn instructions items emit) → output-items, finish
```

`instructions` is the system text. `items` is the full conversation as
Responses API input items. Model and effort come from the specials. `emit`
receives `(:text delta)` and `(:reasoning delta)` as they stream. The return
is the list of output items exactly as the API produced them, plus a finish
keyword (`:stop`, `:length`, `:content-filter`, `:failed`, `:cancelled`).

**Session.** Read the codex CLI's `~/.codex/auth.json` when it exists, else
fx's `~/.fx/chatgpt-auth.json` (`*auth-path*` overrides). fx's shape is
`access_token`, `refresh_token`, `expires_at_ms`, `account_id`, `version`;
the codex CLI keeps the tokens under `tokens` with no expiry, so expiry
comes from the JWT `exp` claim. Both are read into fx's shape and written
back in their own. When `expires_at_ms` is within 60 s, POST JSON `{client_id, grant_type: "refresh_token", refresh_token}` to
`https://auth.openai.com/oauth/token` with fx's client id
`app_EMoamEEZ73f0CkXaXp7hrann`. The response carries `access_token`,
optionally a rotated `refresh_token` and `expires_in`. The account id is the
`chatgpt_account_id` field of the `https://api.openai.com/auth` claim in the
access token's JWT payload; it must match the stored one. Write the file
back atomically (temp file, rename) in its own shape, so the owning tool and
yuuki share one refresh chain.

**Request.** POST `https://chatgpt.com/backend-api/codex/responses`, headers
`Authorization: Bearer`, `chatgpt-account-id`, `originator: yuuki`,
`OpenAI-Beta: responses=experimental`, `accept: text/event-stream`. Body:

```json
{"model": M, "store": false, "stream": true,
 "instructions": SYSTEM, "input": ITEMS,
 "tools": [TOOL], "tool_choice": "auto", "parallel_tool_calls": true,
 "include": ["reasoning.encrypted_content"],
 "text": {"verbosity": "low"},
 "reasoning": {"effort": E, "summary": "auto"}}
```

No `max_output_tokens`; the endpoint rejects it. The tool entry is
`{"type":"function","name","description","parameters","strict":false}`.

**Stream.** SSE lines; each `data:` payload is one JSON event. Reduce:

| event | action |
|---|---|
| `response.output_text.delta`, `response.refusal.delta` | emit `(:text delta)` |
| `response.reasoning_summary_text.delta`, `response.reasoning_text.delta` | emit `(:reasoning delta)` |
| `response.reasoning_summary_part.done` | emit `(:reasoning "\n\n")` |
| `response.output_item.done` | append `item` to output items verbatim |
| `response.completed`, `.done`, `.incomplete` | finish from `response.status` and `incomplete_details.reason`; stop |
| `response.failed`, `error` | finish `:failed`; stop |

Everything else is ignored. Argument deltas are not tracked because the
`output_item.done` for a `function_call` carries the complete `arguments`.

Output items are kept verbatim because with `store: false` the API requires
each `function_call` to be replayed together with the `reasoning` item that
preceded it. Keeping the raw items makes replay a no-op.

## History and the turn

File `agent.lisp`. History is a list of Responses items, append-only:

- user: `{"role":"user","content":[{"type":"input_text","text":T}]}`
- model output items, verbatim: `reasoning`, `message`, `function_call`
- tool result: `{"type":"function_call_output","call_id":ID,"output":TEXT}`

Any provider added later converts this list; the agent never sees another
shape.

```lisp
(run-turn history prompt &key emit approve) → history'
```

1. Append the user item.
2. `stream-turn` the items. Append the output items.
3. Collect `function_call` items. None: return.
4. For each call: emit `(:call id code)`; ask `approve`; run the tool or
   produce `"denied by user"`; emit `(:result id output)`; append the
   output item.
5. Repeat from 2 until the step limit. On the limit, append a user item
   saying so and return.

The agent thread posts `(:done history')` when `run-turn` returns.
Cancellation sets a flag checked between stream lines and between tool
calls, and interrupts the agent thread so a running evaluation or a blocked
read stops at once. An interrupted turn is abandoned: its history is the
one from before the turn.

Instructions are assembled per turn from `context.lisp`: the system prompt
text, a context block with workspace root, OS, date and git branch, and the
workspace `AGENTS.md` if present.

## The tool

File `tool.lisp`. The model sees one function:

```json
{"name": "lisp",
 "description": "Evaluate Common Lisp in the persistent yuuki-user package. Forms are read and evaluated in order; each form's values and everything printed come back. Definitions persist across calls and sessions. (definitions) lists what you have built, (source 'name) shows it. Build the helpers you need, files, shell via uiop:run-program, HTTP via dexador, libraries via ql:quickload, and reuse them.",
 "parameters": {"type":"object",
   "properties": {"code": {"type":"string"},
                  "timeout": {"type":"integer", "description":"seconds, default 60"}},
   "required": ["code"]}}
```

```lisp
(run-lisp code &key (timeout 60)) → string
```

Reads every form from `code` with `*package*` bound to `yuuki-user`, evals
each, and prints its values REPL-style into the same string stream that
captures `*standard-output*` and `*error-output*`. `*standard-input*` is
empty. Any condition ends the run with `error: <condition>` appended. The
whole thing runs under `sb-ext:with-timeout`. The result is cut at
`*max-result-chars*` (64 KiB) with an explicit truncation marker.

Package `yuuki-user` uses `cl` and `uiop`, and preloads two helpers:

- `(definitions)` lists every function, macro and variable defined in the
  package with arglist and docstring.
- `(source name)` pretty-prints the definition form the agent evaluated,
  which `run-lisp` records in an image-resident table for every top-level
  `def*` form; for functions defined some other way it falls back to the
  lambda expression SBCL retains in the default compilation mode.

The `yuuki` package is locked. The agent can read it and call it, but not
redefine it by accident.

## App

File `app.lisp`. State:

```lisp
(defstruct state
  history      ; items, the conversation
  phase        ; :idle | :running | :approving
  queue        ; prompts typed while running
  approval     ; the reply mailbox of the pending call when phase is :approving
  chat         ; pane: log, composer, cursor, scroll
  repl         ; pane
  focus        ; :chat or :repl
  tail         ; partial assistant line
  tail-style)  ; style of the tail; a style change flushes the tail
```

Events and what they do:

| event | transition |
|---|---|
| `(:key k)` | edit the focused composer; Tab switches panes; Enter submits or queues in the chat, evaluates in the REPL; PageUp and PageDown scroll; y/n answer an approval in the chat; Ctrl-C cancels a turn, or exits when idle; Ctrl-D exits, answering no first while approving |
| `(:repl-result form out)` | append the output to the REPL log |
| `(:text d)` | append to tail; complete lines move to committed |
| `(:reasoning d)` | same, dimmed |
| `(:error m)` | same, red; posted by the agent thread when a turn fails |
| `(:call id code)` | commit the code block |
| `(:result id out)` | commit the output block |
| `(:approve id p)` | phase `:approving`, remember `p`; a second one while approving is answered no immediately |
| `(:done history)` | phase `:idle`, take history; if the queue is non-empty, effect start-turn |
| `(:resize)` | repaint |

Effects: `(:start history prompt)` spawns the agent thread on `run-turn`,
`(:resolve p answer)` fulfils an approval promise, `(:cancel)` sets the flag
and interrupts the agent thread, `(:eval code)` runs a slash line through
the tool, `(:exit)`.

The REPL pane is the whole command surface: `(setf *model* "...")`,
`(setf *permission* :yolo)`, `(definitions)`, `(history 'name)`, evaluated
in `yuuki-user`, which uses `yuuki`'s exports.

## Permissions

`*permission*` is `:ask`, `:auto` or `:yolo`. Ask shows the code and waits
for y or n. Yolo runs it. Auto asks the same model, at low effort, through a
forced `permission_decision` tool call carrying the user's request and the
pending code: `clear` runs it, `caution` commits the reason as a dim
transcript line and falls back to the y/n prompt. Any failure or malformed
reply of the review counts as caution. The review request advertises only
the `permission_decision` tool.

## UI

File `ui.lisp`. Two panes on the alternate screen: the chat on the left,
three fifths of the width, and a REPL on the right. Each pane keeps its own
log, composer and scroll offset. One input row under the panes, one status
row at the bottom. Every event batch repaints the whole screen inside
synchronized-output markers; at terminal sizes that is a few kilobytes and
needs no diffing. Resize repaints from scratch.

Keys: Tab moves focus; the focused pane receives typing and Enter. Enter in
the chat submits or queues; Enter in the REPL echoes the form and evaluates
it on its own thread, always, whatever the agent is doing, and the value
arrives as `(:repl-result form output)`. PageUp and PageDown scroll the
focused pane by ten rows; a pane follows its tail until scrolled. Ctrl-C
cancels a running turn or exits when idle; Ctrl-D exits. While approving,
`y` and `n` answer in the chat pane; the REPL keeps working.

Terminal: raw mode through `sb-posix` termios, bracketed paste on, the
alternate screen entered and left with the terminal restored on any exit. A
stdin reader thread turns bytes into keys; SIGWINCH posts `(:resize)`.

Width comes from `sb-unicode`; long lines wrap inside their pane.

## Definitions store

File `store.lisp`, after rekishi. Every definition the agent evaluates
through the tool is recorded in `~/.yuuki/definitions.sqlite3`
(`*store-path*`): an `objects` table keyed by the MD5 of the printed form,
with the form, its name, and the hash of the version it replaced; and a
`bindings` table from name to current object. Recording an identical form
only rebinds, so history never loops. Nothing else persists between runs:
the image is a build artifact.

- `yuuki-user` shadows `defun`, `defmacro`, `defvar`, `defparameter`,
  `defconstant`, `defstruct`, `defclass`, `defgeneric`, `defmethod`,
  `deftype` and `define-condition` with wrappers that expand to the standard
  definer plus `(record form)`, so a definition is recorded wherever it is
  evaluated: top level, inside `progn` or `let`, or from a macro expansion.
  Loading from the store goes through the same wrappers and records nothing
  new. A store failure is a note in the tool result, never an unwind.
- `(source name)` reads the current stored form, falling back to SBCL's
  retained lambda expression for functions defined elsewhere.
- `(history name)` lists versions newest first; `(rollback name)` re-evaluates
  the previous version and rebinds, `(rollback name "hash")` a given one.
  Both are exported to the agent and named in the system prompt.
- `load-definitions` at startup evaluates every current binding into
  `yuuki-user`, macros first, reporting and skipping failures.

## Build

`bin/yuuki` execs `sbcl --core yuuki.core`. `make` loads the system through
Quicklisp and calls `save-lisp-and-die` on `bin/yuuki.core` with `main` as
toplevel. The core is git-ignored and never written at runtime.

## Config

Special variables in `yuuki`, set at startup with the environment first,
then whatever the image remembers, then for the model `models.codex` in
`~/.fx/settings.json`: `*model*` (`YUUKI_MODEL`), `*effort*` (`YUUKI_EFFORT`,
default `high`), `*permission*` (`YUUKI_PERMISSION`, `ask`, `auto` or
`yolo`, default `ask`), `*max-steps*` (100), `*max-result-chars*` (64 KiB).

## Files

| file | owns | est. lines |
|---|---|---|
| `yuuki.asd` | system, test system | 30 |
| `package.lisp` | `yuuki`, `yuuki-user`, specials | 40 |
| `codex.lisp` | session file, refresh, request, SSE reduce | 220 |
| `context.lisp` | system prompt, per-turn context | 80 |
| `tool.lisp` | `run-lisp`, helpers | 90 |
| `agent.lisp` | `run-turn` | 70 |
| `ui.lisp` | termios, keys, render | 220 |
| `app.lisp` | state, reducer, main, save | 200 |
| `t/*.lisp` | fiveam suites | 300 |

## Testing

fiveam, no network. `codex`: JWT account-id extraction, session round trip
through a temp file, request body shape, reducer over recorded SSE event
sequences including a reasoning plus function_call pair. `tool`: values
and output capture, error text, timeout, truncation marker, `source` round
trip. `app`: reducer transitions for every event, queueing while running,
approval resolve. `ui`: key decoding for each supported key, row counting
for wide and wrapped lines.

The binary is exercised by hand before anything is called done: a turn with
a tool call, an approval denial, a cancel, an exit that leaves the terminal
sane, and a relaunch that still has a function the agent defined.
