# yuuki

A coding agent that lives in a Common Lisp image. It has one tool: evaluate
Lisp in its own package. It builds the rest itself, and the image is saved
on exit so what it builds stays.

Requires SBCL, Quicklisp, and a Codex login: the codex CLI's
(`~/.codex/auth.json`) is used when present, else fx's
(`~/.fx/chatgpt-auth.json`).

    make        # builds bin/yuuki.core
    make test
    bin/yuuki   # run from the workspace you want the agent in

Inside, type a prompt and press Enter. Ctrl-C cancels a running turn, or
exits when idle. A line starting with `/` is evaluated by you in the agent's
package: `/(setf *permission* :yolo)`, `/(definitions)`.

Configuration is environment first, then whatever the image remembers:
`YUUKI_MODEL`, `YUUKI_EFFORT` (default high), `YUUKI_PERMISSION` (ask or
yolo, default ask). With no model from either, the `models.codex` entry
in `~/.fx/settings.json` is used.
