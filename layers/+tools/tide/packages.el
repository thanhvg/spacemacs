;;; packages.el --- Tide Layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst tide-packages
  '(
    (tide :location (recipe
                    :fetcher github
                    :repo "thanhvg/tide"))
    )
  "The list of Lisp packages required by the tide layer.")

(defun tide/init-tide ()
  (use-package tide
    :defer t
    :commands (typescript/jump-to-type-def)
    :config
    (spacemacs//tide-setup-prefix)
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

