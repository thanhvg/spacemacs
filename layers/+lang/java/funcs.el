;;; funcs.el --- Java functions File for Spacemacs  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
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

(defun spacemacs//java-mode ()
  "Return desired java mode."
  (if java-use-ts-mode
      'java-ts-mode
    'java-mode))

(defun spacemacs//java-setup-backend ()
  "Conditionally setup java backend."
  (pcase java-backend
    ('lsp (spacemacs//java-setup-lsp))
    ('lspce (spacemacs//java-setup-lspce))
    ('bridge (lsp-bridge-mode))))

(defun spacemacs//java-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq java-backend 'lsp)
    (spacemacs//java-setup-lsp-dap)))


;; Maven

(defun spacemacs/mvn-clean-compile ()
  "Recompile using maven."
  (interactive)
  (mvn-clean)
  (mvn-compile))


;; Misc

(defun spacemacs//java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))


;; LSP Java

(defun spacemacs//java-setup-lsp ()
  "Setup LSP Java."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (require 'lsp-java)
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//java-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-java)
  (spacemacs/set-leader-keys-for-major-mode 'java-mode
    ;; debug
    "ddj" 'dap-java-debug
    "dtt" 'dap-java-debug-test-method
    "dtc" 'dap-java-debug-test-class
    ;; run
    "tt" 'dap-java-run-test-method
    "tc" 'dap-java-run-test-class))

(defun spacemacs/lsp-java-super-type ()
  "Show super type hierarchy."
  (interactive)
  (lsp-java-type-hierarchy 1))

(defun spacemacs/lsp-java-sub-type ()
  "Show sub type hierarchy."
  (interactive)
  (lsp-java-type-hierarchy 0))

(defun spacemacs//java-setup-lspce ()
  "Setup LSPE Java."
  (lspce-mode))


;; quick and dirty stuff

(defun spacemacs/copy-full-qualified-name ()
  "Coypy package.class.method to kill ring."
  (interactive)
  (let* ((package (car (car (cdr (seq-find (lambda (it) (string= "Packages" (car it)))
                                      (lsp--imenu-create-index))))))
         (qualified-name (format "%s.%s" package (which-function))))
    (message "`%s` has been put in kill ring." qualified-name)
    (kill-new qualified-name)))
