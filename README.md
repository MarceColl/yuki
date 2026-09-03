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

Lifecycle hooks can run arbitrary Lisp at turn, model, and tool boundaries.
Register a function from either REPL or the agent tool; its event plist contains
`:event`, `:context`, `:prompt`, and `:history`, plus phase-specific data. A
returned string or list of strings is included in model context for the rest of
that turn:

    (defun project-context (event)
      "Add current project facts before each model call."
      (declare (ignore event))
      "The deployment target is staging.")
    (add-hook :before-model 'project-context)

Events are `:turn-start`, `:before-model`, `:after-model`, `:before-tool`,
`:after-tool`, and `:turn-end`. Use `(list-hooks)`, `(remove-hook event name)`,
or `(clear-hooks)` to inspect and manage registrations.

Configuration is environment first, then whatever the image remembers:
`YUUKI_MODEL`, `YUUKI_EFFORT` (default high), `YUUKI_PERMISSION` (ask, auto
or yolo, default ask). In auto, a second model call reviews each pending
piece of code: clear runs it, caution shows the reason and asks you. With no model from either, the `models.codex` entry
in `~/.fx/settings.json` is used.
