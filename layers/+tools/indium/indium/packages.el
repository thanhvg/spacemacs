;;; packages.el --- indium layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; 
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `indium-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `indium/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `indium/pre-init-PACKAGE' and/or
;;   `indium/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst indium-packages
  '(indium))

(defun indium/init-indium ()
  ;; http://02credits.com/blog/day64-spacemacs-chrome-debugging/
  (use-package indium
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'indium-debugger-mode "d" "debugger")
      (spacemacs/set-leader-keys-for-minor-mode 'indium-debugger-mode
        "dy" 'indium-debugger-previous-frame
        "do" 'indium-debugger-next-frame
        "dl" 'indium-debugger-step-into
        "dj" 'indium-debugger-step-over
        "dh" 'indium-debugger-step-out
        "dr" 'indium-debugger-resume)
      (dolist (mode indium-managed-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "l" 'indium-launch
        "br" 'indium-repl-switch-from-buffer)))))
;;; packages.el ends here
