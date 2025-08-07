;;; packages.el --- denote layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Choan Gálvez <choan@choan.es>
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

;;; Code:

(defconst denote-packages
  '(denote
    denote-menu
    (consult-denote :toggle (configuration-layer/layer-used-p 'compleseus)))
  "The list of Lisp packages required by the denote layer.")

(defun denote/init-denote ()
  (use-package denote
    :defer t
    :hook
    (dired-mode . denote-dired-mode)
    :init
    (spacemacs/declare-prefix "aD" "denote")
    (spacemacs/set-leader-keys
      "aDn" 'denote
      "aDd" 'denote-dired
      "aDg" 'denote-grep
      "aDr" 'denote-rename-file
      "aDR" 'denote-rename-file-using-front-matter)))

(defun denote/init-denote-menu ()
  (use-package denote-menu
    :defer t
    :after denote
    :init
    (spacemacs/set-leader-keys
      "aDm" 'denote-menu-list-notes)
    (spacemacs/set-leader-keys-for-major-mode 'denote-menu-mode
      "/" 'denote-menu-filter
      "k" 'denote-menu-filter-by-keyword
      "c" 'denote-menu-clear-filters
      "e" 'denote-menu-export-to-dired)))

(defun denote/init-consult-denote ()
  (use-package consult-denote
    :defer t
    :after denote
    :init
    (spacemacs/set-leader-keys
      "aDf" 'consult-denote-find
      "aDg" 'consult-denote-grep)))
