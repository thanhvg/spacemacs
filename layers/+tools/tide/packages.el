;;; packages.el --- Tide Layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
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


(defconst tide-packages
  '(tide
    (helm-tide-nav
     :requires helm
     :location (recipe
                :fetcher github
                :repo "thanhvg/helm-tide-nav"))
    (counsel-tide-nav
     :requires ivy
     :location (recipe
                :fetcher github
                :repo "thanhvg/counsel-tide-nav"))
    popwin)
  "The list of Lisp packages required by the tide layer.")

(defun tide/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :config
    (spacemacs//tide-setup-bindings)
    (add-hook 'tide-mode-hook #'spacemacs//tide-setup-jump-handle)
    :init
    (evilified-state-evilify tide-project-errors-mode tide-project-errors-mode-map
      (kbd "C-k") 'tide-find-previous-error
      (kbd "C-j") 'tide-find-next-error
      (kbd "C-l") 'tide-goto-error)
    (evilified-state-evilify tide-references-mode tide-references-mode-map
      (kbd "C-k") 'tide-find-previous-reference
      (kbd "C-j") 'tide-find-next-reference
      (kbd "C-l") 'tide-goto-reference)))

(defun tide/init-helm-tide-nav ()
  (use-package helm-tide-nav
    :defer t))

(defun tide/init-counsel-tide-nav ()
  (use-package counsel-tide-nav
    :defer t))

(defun tide/post-init-popwin ()
  (push '("*tide-documentation*" :dedicated t :position bottom :stick t :noselect t :height 0.3)
        popwin:special-display-config))
