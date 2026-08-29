;;; packages.el --- Just layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2026 Sylvain Benner & Contributors
;;
;; Author: Dietrich Daroch <Dietrich@Daroch.me>
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

(defconst just-packages
  '(
    ;; https://github.com/psibi/justl.el
    justl
    ;; https://github.com/leon-barrett/just-ts-mode.el
    just-ts-mode
    ))

(defun just/init-justl ()
  "Initialization for justl (not referenced by another Spacemacs layer)"
  (use-package justl
    :defer t
    :init
    (spacemacs/declare-prefix "pJ" "Just")
    (spacemacs/set-leader-keys
      "pj" 'justl-exec-default-recipe
      "pJj" 'justl-exec-default-recipe
      "pJJ" 'justl-exec-recipe-in-dir
      "pJl" 'justl)))

(defun just/init-just-ts-mode ()
  "Initialization for just-ts-mode (not referenced by another Spacemacs layer)"
  (use-package just-ts-mode
    :defer t))
