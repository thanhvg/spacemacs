;;; holy-mode.el --- Enter the church of Emacs  -*- lexical-binding: t; -*-

;; Copyright (c) 2015-2025 Sylvain Benner
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing
;; Created: 18 Mar 2015
;; Version: 1.00
;; Package-Requires: ((emacs "24") (evil "1.0.9"))
;; URL: https://github.com/syl20bnr/spacemacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'evil)

(defun holy-mode--override-with-emacs-state (f &optional arg &rest args)
  "Advice around some Evil states to force Emacs state."
  (if (equal -1 arg)
      (apply f arg args)
    (evil-emacs-state)))

(defconst holy-mode--overridden-states
  (mapcar
   (lambda (state) (intern (format "evil-%s-state" state)))
   '(insert motion normal))
  "List of Evil state functions that `holy-mode' will override with `evil-emacs-state'.")

;;;###autoload
(define-minor-mode holy-mode
  "Global minor mode to repulse the evil from spacemacs.

The `insert state' is replaced by the `emacs state'."
  :global t
  :lighter " holy"
  :group 'spacemacs
  (if holy-mode
      (in-nomine-patris-et-filii-et-spiritus-sancti)
    (amen)))

(defun in-nomine-patris-et-filii-et-spiritus-sancti ()
  "Enter the church of Emacs (wash your hands)."
  ;; make all buffers' initial state emacs
  (add-to-list 'evil-buffer-regexps '("." . emacs))
  ;; replace evil states by `emacs state'
  (dolist (state holy-mode--overridden-states)
    (advice-add state :around #'holy-mode--override-with-emacs-state))
  ;; key bindings hooks for dynamic switching of editing styles
  (run-hook-with-args 'spacemacs-editing-style-hook 'emacs)
  ;; initiate `emacs state' and enter the church
  (holy-mode//update-states-for-current-buffers 'emacs))

(defun amen ()
  "May the force be with you my son (or not)."
  ;; restore defaults
  (setq evil-buffer-regexps (delete '("." . emacs) evil-buffer-regexps))
  ;; restore evil states
  (dolist (state holy-mode--overridden-states)
    (advice-remove state #'holy-mode--override-with-emacs-state))
  ;; restore key bindings
  (run-hook-with-args 'spacemacs-editing-style-hook 'vim)
  ;; restore the states
  (holy-mode//update-states-for-current-buffers 'vim))

(defun holy-mode//update-states-for-current-buffers (style)
  "Update the active state in all current buffers given current STYLE."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (cond
       ((not evil-local-mode))
       ((eq 'emacs style) (evil-emacs-state))
       ((and (eq 'vim style)
             (eq 'emacs evil-state))
        (cond
         ((memq major-mode evil-evilified-state-modes) (evil-evilified-state))
         ((memq major-mode evil-motion-state-modes) (evil-motion-state))
         (t (evil-normal-state))))))))

(provide 'holy-mode)
;;; holy-mode.el ends here
