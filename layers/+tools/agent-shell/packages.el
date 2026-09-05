;;; packages.el --- agent-shell layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `agent-shell-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `agent-shell/init-PACKAGE' to load and initialize the package.
;;
;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `agent-shell/pre-init-PACKAGE' and/or
;;   `agent-shell/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst agent-shell-packages
  '(agent-shell))

(defun agent-shell/init-agent-shell ()
  (use-package agent-shell
    :init
    (spacemacs/set-leader-keys
      "aaa" 'agent-shell
      "aab" 'spacemacs/agent-shell-switch-to-buffer)))
