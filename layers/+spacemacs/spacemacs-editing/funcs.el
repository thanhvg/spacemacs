;;; funcs.el --- Spacemacs editing Layer functions File  -*- lexical-binding: t; -*-
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



;; smartparens

(defun spacemacs/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun spacemacs/smartparens-pair-newline-and-indent (id action context)
  (spacemacs/smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun spacemacs/smart-closing-parenthesis ()
  "Insert a closing pair delimiter or move point past existing delimiter.

If the expression at point is already balanced and there is a
closing delimiter for that expression on the current line, move
point forward past the closing delimiter.

If the expression is balanced but there is no closing delimiter
on the current line, insert a literal ')' character.

If the expression is not balanced, insert a closing delimiter for
the current expression.

This command uses Smartparens navigation commands and therefore
recognizes pair delimiters that have been defined using `sp-pair'
or `sp-local-pair'."
  (interactive)
  (let* ((sp-navigate-close-if-unbalanced t)
         (current-pos (point))
         (current-line (line-number-at-pos current-pos))
         next-pos next-line)
    (save-excursion
      (let ((buffer-undo-list)
            (modified (buffer-modified-p)))
        (unwind-protect
            (progn
              (sp-up-sexp)
              (setq next-pos (point)
                    next-line (line-number-at-pos)))
          (primitive-undo (length buffer-undo-list)
                          buffer-undo-list)
          (set-buffer-modified-p modified))))
    (cond
     ((and (= current-line next-line)
           (not (= current-pos next-pos)))
      (sp-up-sexp))
     (t
      (insert-char ?\))))))

(defun spacemacs//activate-smartparens(&optional global)
  "Enable `smartparens-mode' or strict version.
This either activates `smartparens-mode' or `smartparens-strict-mode'
depending on the respective dotfile setting.

It is not necessary to activate `smartparens-mode' independently as it
is included in `smartparens-strict-mode'.

If `global' is non-nil activate the respective global mode."
  (if dotspacemacs-smartparens-strict-mode
      (if global
          (smartparens-global-strict-mode 1)
        (smartparens-strict-mode 1))
    (if global
        (smartparens-global-mode 1)
      (smartparens-mode 1))))

(defun spacemacs//deactivate-smartparens(&optional global)
  "Deactivate `smartparens-mode'.
This deactivates `smartparens-mode' and `smartparens-strict-mode'.

It is important to disable both to remove all advices.

If `global' is non-nil activate the respective global mode."
  (if global
      (progn
        (when smartparens-global-strict-mode
          (smartparens-global-strict-mode -1))
        (smartparens-global-mode -1))
    (when smartparens-strict-mode
      (smartparens-strict-mode -1))
    (smartparens-mode -1)))

(defun spacemacs//conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (or (eq this-command 'eval-expression)
          (eq this-command 'eldoc-eval-expression))
      (spacemacs//activate-smartparens)))

(defun spacemacs//adaptive-smartparent-pair-overlay-face ()
  (set-face-attribute 'sp-pair-overlay-face nil
                      :inherit 'lazy-highlight
                      :background 'unspecified
                      :foreground 'unspecified))

(defun spacemacs//put-clean-aindent-last ()
  "Put `clean-aindent--check-last-point` to end of `post-command-hook`.
This functions tries to ensure that clean-aindent checks for indent
operations after each indent operations have been done.

See issues #6520 and #13172"
  (when clean-aindent-mode
    (remove-hook 'post-command-hook 'clean-aindent--check-last-point)
    (add-hook 'post-command-hook 'clean-aindent--check-last-point t)))


;; uuidgen
;; TODO spacemacs/uuidgen-3 and spacemacs/uuidgen-5

(defun spacemacs/uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun spacemacs/uuidgen-4 (arg)
  "Return an UUID from random numbers (UUIDv4).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-4)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))


;; avy
;; https://github.com/abo-abo/avy/issues/312
(defun spacemacs/avy-goto-symbol-at-point (&optional arg)
  "Jump to a visible occurance of symbol-at-point.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-symbol-at-point
      (avy-process 
       (avy--regex-candidates (regexp-quote (thing-at-point 'symbol t)))))))


;; origami
(defun spacemacs//enable-origami-on-server-frame ()
  "Hook to run in daemon mode."
  (global-origami-mode)
  (remove-hook 'server-after-make-frame-hook #'spacemacs//enable-origami-on-server-frame))


(defun spacemacs//treesit-get-continuous-region-of-same-node(node node-name offset)
  (when-let* ((node-p ( lambda (n) (and n (string= node-name (treesit-node-type n)))))
              (current (treesit-parent-until node node-p t)))
    (let ((first current)
          (last current))
      ;; Search backwards
      (while-let ((prev (treesit-node-prev-sibling first))
                  ((funcall node-p prev)))
        (setq first prev))
      ;; Search forwards
      (while-let ((next (treesit-node-next-sibling last))
                  ((funcall node-p next)))
        (setq last next))
      ;; Return range
      (treesit-fold--cons-add (cons (treesit-node-start first) (treesit-node-end last)) offset))))




(defun spacemacs//treesit-fold-overlay-at (pos)
  "Return the treesit-fold overlay at POS, if any."
  (seq-find (lambda (ov) (eq (overlay-get ov 'creator) 'treesit-fold))
            (overlays-at pos)))

(defun spacemacs//treesit-fold-next-overlay-pos (pos)
  "Return the position of the next treesit-fold overlay change after POS, or nil.
     If POS is already inside a treesit-fold overlay, first skip past its end."
  (let* ((limit (point-max))
         (cur (or (spacemacs//treesit-fold-overlay-at pos)
                  (spacemacs//treesit-fold-overlay-at (1+ pos))))
         (next (if cur  (overlay-end cur) pos)))
    (catch 'found
      (while (< next limit)
        (setq next (next-overlay-change next))
        (when (>= next limit)
          (throw 'found nil))
        (when (spacemacs//treesit-fold-overlay-at next)
          (throw 'found next)))
      nil)))

(defun spacemacs/treesit-fold-goto-next-overlay ()
  "Move point to the start of the next overlay created by `treesit-fold'.
     If point is currently inside such an overlay, it is skipped."
  (interactive)
  (let ((pos (spacemacs//treesit-fold-next-overlay-pos (point))))
    (if pos
        (goto-char pos)
      (message "No next treesit-fold overlay found"))))

(defun spacemacs//treesit-fold-prev-overlay-pos (pos)
  "Return the position of the previous treesit-fold overlay change before POS, or nil.
     If POS is already inside a treesit-fold overlay, first skip past its start."
  (let* ((limit (point-min))
         (cur (or (spacemacs//treesit-fold-overlay-at pos)
                  (spacemacs//treesit-fold-overlay-at (1- pos))))
         (prev (if cur (overlay-start cur) pos)))
    (catch 'found
      (while (> prev limit)
        (setq prev (previous-overlay-change prev))
        (when (<= prev limit)
          (throw 'found nil))
        (when (spacemacs//treesit-fold-overlay-at prev)
          (throw 'found prev)))
      nil)))

(defun spacemacs/treesit-fold-goto-prev-overlay ()
  "Move point to the start of the previous overlay created by `treesit-fold'.
     If point is currently inside such an overlay, it is skipped."
  (interactive)
  (let ((pos (spacemacs//treesit-fold-prev-overlay-pos (point))))
    (if pos
        (goto-char pos)
      (message "No previous treesit-fold overlay found"))))

(defun spacemacs/treesit-fold-close-all-but-point ()
  "Fold every foldable syntax node in the buffer except the ones needed
to keep `point' visible.

This is like `treesit-fold-close-all', but any fold whose range would
hide `point' is left open (or reopened if it was already folded)."
  (interactive)
  (treesit-fold--ensure-ts
   (let ((pos (point))
         nodes)
     ;; If `point' is currently inside an existing fold, open it first so
     ;; it isn't left hidden by a stale overlay.
     (dolist (ov (treesit-fold--overlays-in 'invisible 'treesit-fold))
       (when (and (<= (overlay-start ov) pos) (<= pos (overlay-end ov)))
         (delete-overlay ov)))
     (let* ((treesit-fold-indicators-mode)  ; temporarily disable, refresh once at the end
            (treesit-fold-on-fold-hook)
            (root (treesit-buffer-root-node))
            (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                                  (alist-get major-mode treesit-fold-range-alist)))
            (query (treesit-query-compile (treesit-node-language root) patterns)))
       (setq nodes (treesit-query-capture root query)
             nodes (mapcar #'cdr nodes)
             nodes (cl-remove-if
                    (lambda (node)
                      (or
                       ;; Same exclusion as `treesit-fold-close-all': skip
                       ;; nodes whose fold range is on a single line.
                       (treesit-fold--node-range-on-same-line node)
                       ;; Skip nodes whose fold range would swallow `point'.
                       (when-let* ((range (treesit-fold--get-fold-range node)))
                         (and (<= (car range) pos) (<= pos (cdr range))))))
                    nodes))
       (mapc #'treesit-fold-close nodes))
     (when nodes
       (run-hooks 'treesit-fold-on-fold-hook)
       t))))

;;
;; (@* "Fold by level" )
;;

(defun spacemacs//treesit-fold--node-fold-depth (node mode-ranges)
  "Return the foldable-nesting depth of NODE.

Depth counts NODE itself (if foldable) plus every ancestor of NODE that
is a registered foldable type in MODE-RANGES and whose range isn't
confined to a single line.  The outermost foldable node has depth 1."
  (let ((depth 0)
        (cur node))
    (while cur
      (when (and (alist-get (intern (treesit-node-type cur)) mode-ranges)
                 (not (treesit-fold--node-range-on-same-line cur)))
        (setq depth (1+ depth)))
      (setq cur (treesit-node-parent cur)))
    depth))

(defun spacemacs/treesit-fold-close-level (level)
  "Fold all foldable nodes at fold-depth LEVEL, VS Code `Fold Level' style.

LEVEL 1 folds only the outermost foldable nodes.  LEVEL 2 folds nodes
nested one foldable-level deeper (parents of those are left open), and
so on up to LEVEL 9.  LEVEL 0 unfolds everything (equivalent to
`treesit-fold-open-all').

Interactively, prompts for LEVEL (0-9)."
  (interactive "nFold level (0-9): ")
  (treesit-fold--ensure-ts
    ;; Always start from a clean slate, same as VS Code: pressing a level
    ;; re-derives the fold state from scratch rather than layering on top
    ;; of whatever is currently folded.
    (treesit-fold-open-all)
    (when (> level 0)
      (let* ((mode-ranges (alist-get major-mode treesit-fold-range-alist))
             (root (treesit-buffer-root-node))
             (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                                   mode-ranges))
             (query (treesit-query-compile (treesit-node-language root) patterns))
             (nodes (mapcar #'cdr (treesit-query-capture root query)))
             (nodes (cl-remove-if #'treesit-fold--node-range-on-same-line nodes))
             (target-nodes
              (cl-remove-if-not
               (lambda (node)
                 (= (spacemacs//treesit-fold--node-fold-depth node mode-ranges) level))
               nodes)))
        (mapc #'treesit-fold-close target-nodes)
        (when target-nodes
          (run-hooks 'treesit-fold-on-fold-hook)
          t)))))

(dotimes (i 10)
  (defalias (intern (format "spacemacs/treesit-fold-close-level-%d" i))
    (lambda () (interactive) (spacemacs/treesit-fold-close-level i))
    (format "Fold to level %d (VS Code `Fold Level %d' equivalent)." i i)))

