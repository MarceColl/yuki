# yuuki

A coding agent that lives in a Common Lisp image. It has one tool: evaluate
Lisp in its own package. It builds the rest itself, and every definition it
makes is recorded with its history in `~/.yuuki/definitions.sqlite3`, so
what it builds stays across runs and rebuilds and can be rolled back.

Requires SBCL, Quicklisp, and a Codex login: the codex CLI's
(`~/.codex/auth.json`) is used when present, else fx's
(`~/.fx/chatgpt-auth.json`).

    make        # builds bin/yuuki.core
    make test
    bin/yuuki   # run from the workspace you want the agent in

Two panes: the chat on the left, a REPL in the agent's package on the
right. Tab switches. Type a prompt and press Enter in the chat; Ctrl-C
cancels a running turn, or exits when idle. The REPL evaluates any time,
even mid-turn: `(setf *permission* :yolo)`, `(definitions)`,
`(history 'read-file)`, `(rollback 'read-file)`. PageUp and PageDown scroll
the focused pane.

Configuration is environment first, then whatever the image remembers:
`YUUKI_MODEL`, `YUUKI_EFFORT` (default high), `YUUKI_PERMISSION` (ask, auto
or yolo, default ask). In auto, a second model call reviews each pending
piece of code: clear runs it, caution shows the reason and asks you. With no model from either, the `models.codex` entry
in `~/.fx/settings.json` is used.
