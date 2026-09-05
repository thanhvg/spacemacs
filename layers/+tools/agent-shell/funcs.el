;; -*- lexical-binding: t; -*-

(defun spacemacs/agent-shell-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (agent-shell-shell-buffer)))
