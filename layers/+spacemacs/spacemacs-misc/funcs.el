;; wgrep  -*- lexical-binding: t; -*-

(defun spacemacs/grep-change-to-wgrep-mode ()
  (interactive)
  (require 'wgrep)
  (wgrep-change-to-wgrep-mode)
  (evil-normal-state))

(defun spacemacs//set-initial-grep-state ()
  "Set the initial evil state for the grep buffers."
  (if (eq dotspacemacs-editing-style 'emacs)
      (evil-set-initial-state 'grep-mode 'emacs)
    (evil-set-initial-state 'grep-mode 'motion)))

(defun spacemacs/wgrep-finish-edit ()
  "Set back the default evil state when finishing editing."
  (interactive)
  (wgrep-finish-edit)
  (spacemacs//grep-set-evil-state))

(defun spacemacs/wgrep-abort-changes ()
  "Set back the default evil state when aborting editing."
  (interactive)
  (wgrep-abort-changes)
  (spacemacs//grep-set-evil-state))

(defun spacemacs//grep-set-evil-state ()
  "Set the evil state for the read-only grep buffer given the current editing style."
  (if (eq dotspacemacs-editing-style 'emacs)
      (evil-emacs-state)
    (evil-motion-state)))

(defun spacemacs/wgrep-abort-changes-and-quit ()
  "Abort changes and quit."
  (interactive)
  (spacemacs/wgrep-abort-changes)
  (quit-window))

(defun spacemacs/wgrep-save-changes-and-quit ()
  "Save changes and quit."
  (interactive)
  (spacemacs/wgrep-finish-edit)
  (wgrep-save-all-buffers)
  (quit-window))

;; avy deadgrep

(defun spacemacs//ace-link--deadgrep-collect ()
  (let ((candidates (list))
        (prop 'button)
        (pt (window-start)))
    (while (and pt (< pt (window-end)))
      (when (get-text-property pt prop)
        (push pt candidates))
      (setq pt (next-single-property-change pt prop)))
    (nreverse candidates)))

(defun spacemacs//ace-link--deadgrep-action (pt)
  (when (number-or-marker-p pt)
    (goto-char pt)
    (push-button)))

(defun spacemacs/ace-link-deadgrep ()
  "Open a visible buttons in an `deadgrep' buffer."
  (interactive)
  (let ((pt (avy-with spacemacs/ace-link-deadgrep
              (avy-process
               (spacemacs//ace-link--deadgrep-collect)
               (avy--style-fn avy-style)))))
    (spacemacs//ace-link--deadgrep-action pt)))
