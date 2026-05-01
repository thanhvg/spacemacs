;;; packages.el --- Go Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
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


(defconst go-packages
  '(
    company
    dap-mode
    (company-go :requires company
                :toggle (eq go-backend 'go-ts-mode))
    eldoc
    flycheck
    (flycheck-golangci-lint :toggle (and go-use-golangci-lint
                                         (configuration-layer/package-used-p 'flycheck)))
    ggtags
    go-eldoc
    go-fill-struct
    go-gen-test
    go-guru
    go-impl
    go-ts-mode
    go-rename
    go-tag
    godoctor
    popwin))

(defun go/init-company-go ()
  (use-package company-go
    :defer t
    :init (spacemacs|add-company-backends
            :backends company-go
            :modes go-ts-mode
            :variables company-go-show-annotation t)))

(defun go/post-init-company ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs//go-setup-company))

(defun go/pre-init-dap-mode ()
  (when (eq go-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'go-ts-mode))
  (add-hook 'go-mode-local-vars-hook #'spacemacs//go-setup-dap))

(defun go/post-init-eldoc ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs//go-setup-eldoc))

(defun go/post-init-flycheck ()
  (spacemacs/enable-flycheck 'go-ts-mode))

(defun go/init-flycheck-golangci-lint ()
  (use-package flycheck-golangci-lint
    :defer t
    :init (add-hook 'go-mode-local-vars-hook 'spacemacs//go-enable-flycheck-golangci-lint)))

(defun go/post-init-ggtags ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun go/init-go-eldoc ()
  (use-package go-eldoc
    :defer t))

(defun go/init-go-fill-struct ()
  (use-package go-fill-struct
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
            "rs" 'go-fill-struct)))

(defun go/init-go-gen-test ()
  (use-package go-gen-test
    :defer t
    :init
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mtg" "generate")
    (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
      "tgg" 'go-gen-test-dwim
      "tgf" 'go-gen-test-exported
      "tgF" 'go-gen-test-all)))

(defun go/init-go-guru ()
  (use-package go-impl
    :defer t
    :init
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mf" "guru")
    (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
      "f<" 'go-guru-callers
      "f>" 'go-guru-callees
      "fc" 'go-guru-peers
      "fd" 'go-guru-describe
      "fe" 'go-guru-whicherrs
      "ff" 'go-guru-freevars
      "fi" 'go-guru-implements
      "fj" 'go-guru-definition
      "fo" 'go-guru-set-scope
      "fp" 'go-guru-pointsto
      "fr" 'go-guru-referrers
      "fs" 'go-guru-callstack)))

(defun go/init-go-impl ()
  (use-package go-impl
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
            "ri" 'go-impl)))

(defun go/init-go-ts-mode ()
  (use-package go-ts-mode
    :hook ((go-mode-local-vars . spacemacs//go-set-tab-width)
           (go-mode-local-vars . spacemacs//go-setup-backend)
           (go-mode-local-vars . spacemacs//go-setup-format)
           (go-ts-mode-hook . treesit-fold-mode))
    :init
    ;; get go packages much faster
    (setq go-packages-function 'spacemacs/go-packages-gopkgs)
    (spacemacs|add-toggle go-test-verbose
      :documentation "Enable verbose test output."
      :status go-test-verbose
      :on (setq go-test-verbose t)
      :off (setq go-test-verbose nil)
      :evil-leader-for-mode (go-ts-mode . "tv"))
    :config
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "me" "playground")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mi" "imports")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mr" "refactoring")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mt" "test")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mT" "toggle")
    (spacemacs/declare-prefix-for-mode 'go-ts-mode "mx" "execute")
    (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
      "="  'gofmt
      "eb" 'go-play-buffer
      "ed" 'go-download-play
      "er" 'go-play-region
      "ga" 'ff-find-other-file
      "gc" 'go-coverage
      "hh" 'godoc-at-point
      "ia" 'go-import-add
      "ig" 'go-goto-imports
      "ir" 'go-remove-unused-imports
      "tP" 'spacemacs/go-run-package-tests-nested
      "tp" 'spacemacs/go-run-package-tests
      "ts" 'spacemacs/go-run-test-current-suite
      "tt" 'spacemacs/go-run-test-current-function
      "xx" 'spacemacs/go-run-main
      "xg" 'spacemacs/go-run-generate-current-buffer
      "xG" 'spacemacs/go-run-generate-current-dir)))

(defun go/init-go-rename ()
  (use-package go-rename
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
            "rN" 'go-rename)))

(defun go/init-go-tag ()
  (use-package go-tag
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
            "rf" 'go-tag-add
            "rF" 'go-tag-remove)))

(defun go/init-godoctor ()
  (use-package godoctor
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-ts-mode
            "rd" 'godoctor-godoc
            "re" 'godoctor-extract
            "rn" 'godoctor-rename
            "rt" 'godoctor-toggle)))

(defun go/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push (cons go-test-buffer-name '(:dedicated t :position bottom :stick t :noselect t :height 0.4)) popwin:special-display-config)))
