;;; funcs.el --- Spacemacs Evil Layer functions File  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defvar spacemacs--evil-iedit-insert-states-default nil
  "Default value of the list of additional states enabled in \
`evil-iedit-insert-state'.")

(defvar spacemacs--evil-iedit-insert-states-hybrid nil
  "List of additional states enabled in `evil-iedit-insert-state' when
`hybrid-mode' is active.")

(defun spacemacs//enable-hs-minor-mode ()
  "Enable hs-minor-mode for code folding when not using tree-sitter backed modes."
  (unless (string-match-p "-ts-" (symbol-name major-mode))
    (ignore-errors
      (hs-minor-mode))))

(defun spacemacs//iedit-insert-state-hybrid (style)
  "If STYLE is hybrid, update `evil-iedit-insert-state' definition to enable
`evil-hybrid-state' instead of `evil-insert-state'.
Otherwise, revert to the default behavior (i.e. enable `evil-insert-state')."
  ;; Populate variables on the first invocation.
  (unless spacemacs--evil-iedit-insert-states-default
    (setq spacemacs--evil-iedit-insert-states-default
          (evil-get-property evil-state-properties 'iedit-insert :enable))
    (setq spacemacs--evil-iedit-insert-states-hybrid
          (mapcar (lambda (item)
                    (if (eq item 'insert) 'hybrid item))
                  spacemacs--evil-iedit-insert-states-default)))
  (let ((states (if (eq style 'hybrid)
                    spacemacs--evil-iedit-insert-states-hybrid
                  spacemacs--evil-iedit-insert-states-default)))
    (evil-put-property 'evil-state-properties 'iedit-insert
                       :enable states)))

(defun spacemacs//iedit-state-TAB-key-bindings (style)
  "Set the action for TAB key in iedit state."
  (if (memq style '(vim hybrid))
      (progn
        (define-key iedit-occurrence-keymap-default
          (kbd "TAB") 'iedit-toggle-selection)
        (define-key iedit-occurrence-keymap-default
          [tab] 'iedit-toggle-selection))
    (define-key iedit-occurrence-keymap-default
      (kbd "TAB") 'iedit-next-occurrence)
    (define-key iedit-occurrence-keymap-default
      [tab] 'iedit-next-occurrence)))

(defun spacemacs//evil-escape-deactivate-in-holy-mode  (style)
  "Deactivate `evil-escape' if STYLE is `emacs' otherwise enable it."
  (if (memq style '(vim hybrid))
      (evil-escape-mode t)
    (evil-escape-mode -1)))


;; vi-tilde-fringe

(defun spacemacs/disable-vi-tilde-fringe ()
  "Disable `vi-tilde-fringe' in the current buffer."
  (vi-tilde-fringe-mode -1))

(defun spacemacs/disable-vi-tilde-fringe-read-only ()
  "Disable `vi-tilde-fringe' in the current buffer if it is read only."
  (when buffer-read-only
    (spacemacs/disable-vi-tilde-fringe)))


;; lisp state

(defun spacemacs//load-evil-lisp-state ()
  "Load evil-lisp-state lazily"
  (require 'evil-lisp-state)
  (remove-hook 'prog-mode-hook #'spacemacs//load-evil-lisp-state))


;; evil-nerd-commenter

;; double all the commenting functions so that the inverse operations
;; can be called without setting a flag
(defun spacemacs/comment-or-uncomment-lines-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-lines arg)))

(defun spacemacs/comment-or-uncomment-lines (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-lines arg)))

(defun spacemacs/copy-and-comment-lines-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-copy-and-comment-lines arg)))

(defun spacemacs/copy-and-comment-lines (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-copy-and-comment-lines arg)))

(defun spacemacs/quick-comment-or-uncomment-to-the-line-inverse
    (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun spacemacs/quick-comment-or-uncomment-to-the-line (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun spacemacs/comment-or-uncomment-paragraphs-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(defun spacemacs/comment-or-uncomment-paragraphs (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-paragraphs arg)))

;;
;; (@* "Fold helpers" )
;;
(defun hs--compute-block-range ()
  "Return (P . Q), the hideable bounds of the block at point, without hiding it.

Point must already be positioned where `hs-looking-at-block-start-p-func'
succeeds (typically `match-beginning' of `hs-block-start-mdata-select'
right after a regexp search for `hs-block-start-regexp').  Return nil if
point is not actually at a valid, hideable block start.

This mirrors the range computation inside `hs-hide-block-at-point', but
is read-only: it never creates or touches an overlay."
  (when (funcall hs-looking-at-block-start-p-func)
    (let ((mdata (match-data t))
          (header-end (match-end 0))
          p q)
      (save-excursion
        (goto-char (funcall (or hs-adjust-block-beginning #'identity) header-end))
        (setq p (line-end-position)))
      (save-excursion
        (hs-forward-sexp mdata 1)
        (setq q (if (looking-back hs-block-end-regexp nil)
                    (match-beginning 0)
                  (point))))
      (cons p q))))


;;
;; (@* "Fold all but point" )
;;
(defun hs--close-all-but-point-1 (minp maxp target)
  "Recursively fold blocks in MINP..MAXP except the chain enclosing TARGET."
  (goto-char minp)
  (while (funcall hs-find-next-block-func hs-block-start-regexp maxp nil)
    (unless (save-match-data (nth 8 (syntax-ppss)))  ; skip comments/strings
      (let ((match-beg (match-beginning hs-block-start-mdata-select))
            (match-end (match-end 0)))
        (goto-char match-beg)
        (let ((range (and (funcall hs-looking-at-block-start-p-func)
                           (hs--compute-block-range))))
          (if (null range)
              ;; Regexp matched something that isn't actually hideable
              ;; (e.g. a false positive); just move past it.
              (goto-char match-end)
            (let ((p (car range)) (q (cdr range)))
              (if (and (<= p target) (<= target q))
                  ;; TARGET lives inside this block: leave it open, but
                  ;; recurse into its body to fold any siblings there
                  ;; that don't contain TARGET.
                  (progn
                    (hs--close-all-but-point-1 p q target)
                    (goto-char q))
                ;; TARGET is elsewhere: fold this block in its entirety.
                (progn
                  (goto-char match-beg)
                  (hs-hide-block-at-point)
                  (goto-char q))))))))))

(defun hs-close-all-but-point ()
  "Fold every hideable block in the buffer except the ones enclosing `point'.

Resets all folding, then folds every top-level and nested block that
does not contain `point', leaving open only the chain of blocks needed
to keep `point' visible.  Analogous to
`treesit-fold-close-all-but-point' for `treesit-fold-mode'."
  (interactive)
  (hs-life-goes-on
   (let ((target (point)))
     (save-excursion
       (unless hs-allow-nesting
         (hs-discard-overlays (point-min) (point-max)))
       (goto-char (point-min))
       (syntax-propertize (point-max))
       (message "Folding all but point ...")
       (hs--close-all-but-point-1 (point-min) (point-max) target)
       (message "Folding all but point ... done")))
   (run-hooks 'hs-hide-hook)))

;;
;; (@* "Fold by level" )
;;

(defun hs--hide-level-at-depth-1 (minp maxp depth level)
  "Recursively fold blocks in MINP..MAXP at fold DEPTH, targeting LEVEL."
  (goto-char minp)
  (while (funcall hs-find-next-block-func hs-block-start-regexp maxp nil)
    (unless (save-match-data (nth 8 (syntax-ppss)))  ; skip comments/strings
      (let ((match-beg (match-beginning hs-block-start-mdata-select))
            (match-end (match-end 0)))
        (goto-char match-beg)
        (let ((range (and (funcall hs-looking-at-block-start-p-func)
                           (hs--compute-block-range))))
          (if (null range)
              (goto-char match-end)
            (let ((p (car range)) (q (cdr range)))
              (if (= depth level)
                  ;; This block is exactly at the target depth: fold it
                  ;; (and everything nested inside it goes with it).
                  (progn
                    (goto-char match-beg)
                    (hs-hide-block-at-point)
                    (goto-char q))
                ;; Not there yet: leave this block open and descend.
                (progn
                  (hs--hide-level-at-depth-1 p q (1+ depth) level)
                  (goto-char q))))))))))

;;;###autoload
(defun hs-hide-level-at-depth (level)
  "Fold all blocks at fold-depth LEVEL, VS Code `Fold Level' style.

LEVEL 1 folds only top-level blocks.  LEVEL 2 leaves top-level blocks
open and folds blocks nested one level inside them, and so on up to
LEVEL 9.  LEVEL 0 unfolds everything (equivalent to `hs-show-all').

Always resets folding state first, so calling this with a new LEVEL
replaces the previous fold state rather than adding to it — matching
VS Code's Cmd+K Cmd+<N> behavior.

Interactively, prompts for LEVEL (0-9)."
  (interactive "nFold level (0-9): ")
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (when (> level 0)
       (goto-char (point-min))
       (syntax-propertize (point-max))
       (message "Folding to level %d ..." level)
       (hs--hide-level-at-depth-1 (point-min) (point-max) 1 level)
       (message "Folding to level %d ... done" level)))
   (run-hooks (if (> level 0) 'hs-hide-hook 'hs-show-hook))))

;; Per-level convenience commands, mirroring VS Code's Cmd+K Cmd+<0-9>.
(dotimes (i 10)
  (defalias (intern (format "hs-hide-level-at-depth-%d" i))
    (lambda () (interactive) (hs-hide-level-at-depth i))
    (format "Fold to level %d (VS Code `Fold Level %d' equivalent)." i i)))

;; ;;
;; ;; (@* "Level keymap" )
;; ;;

;; (defvar-keymap hs-level-map
;;   :doc "Second-keystroke keymap for fold-by-level commands.
;; Digits 1-9 fold to that depth; 0 unfolds everything."
;;   "0" #'hs-hide-level-at-depth-0
;;   "1" #'hs-hide-level-at-depth-1
;;   "2" #'hs-hide-level-at-depth-2
;;   "3" #'hs-hide-level-at-depth-3
;;   "4" #'hs-hide-level-at-depth-4
;;   "5" #'hs-hide-level-at-depth-5
;;   "6" #'hs-hide-level-at-depth-6
;;   "7" #'hs-hide-level-at-depth-7
;;   "8" #'hs-hide-level-at-depth-8
;;   "9" #'hs-hide-level-at-depth-9)

;; ;; Fits alongside hideshow's existing "C-c @" prefix convention.
;; ;; "C-c @ C-p" for "fold all but point", "C-c @ C-k" then a digit for
;; ;; fold-by-level (K for "level", echoing VS Code's Cmd+K chord).
;; (keymap-set hs-minor-mode-map "C-c @ C-p" #'hs-close-all-but-point)
;; (keymap-set hs-minor-mode-map "C-c @ C-k" hs-level-map)
