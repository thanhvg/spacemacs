;;; funcs.el --- Javascript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend

(defun spacemacs//js-backend ()
  "Returns selected backend."
  (if js-backend
      js-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'tern))))

(defun spacemacs//js-setup-backend ()
  "Conditionally setup javascript backend."
  (pcase (spacemacs//js-backend)
    (`tern (spacemacs//js-setup-tern))
    (`tide (spacemacs//tide-setup))
    (`lsp (spacemacs//js-setup-lsp))))

(defun spacemacs//js-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//js-backend)
    (`tern (spacemacs//js-setup-tern-company))
    (`tide (spacemacs//tide-setup-company 'js-mode))
    (`lsp (spacemacs//js-setup-lsp-company))))

(defun spacemacs//js-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//js-backend)
    (`lsp (spacemacs//js-setup-lsp-dap))))


;; lsp

(defun spacemacs//js-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (when (not js-lsp-linter)
          (setq-local lsp-diagnostic-package :none))
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//js-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes js-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//js-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-firefox)
  (require 'dap-chrome))


;; tern
(defun spacemacs//js-setup-tern ()
  (if (configuration-layer/layer-used-p 'tern)
      (when (locate-file "tern" exec-path)
        (spacemacs/tern-setup-tern))
    (message (concat "Tern was configured as the javascript backend but "
                     "the `tern' layer is not present in your `.spacemacs'!"))))

(defun spacemacs//js-setup-tern-company ()
  (if (configuration-layer/layer-used-p 'tern)
      (when (locate-file "tern" exec-path)
        (spacemacs/tern-setup-tern-company 'js2-mode))
    (message (concat "Tern was configured as the javascript backend but "
                     "the `tern' layer is not present in your `.spacemacs'!"))))


;; js-doc

(defun spacemacs/js-doc-require ()
  "Lazy load js-doc"
  (require 'js-doc))
(add-hook 'js-mode-hook 'spacemacs/js-doc-require)

(defun spacemacs/js-doc-set-key-bindings (mode)
  "Setup the key bindings for `js2-doc' for the given MODE."
  (spacemacs/declare-prefix-for-mode mode "mrd" "documentation")
  (spacemacs/set-leader-keys-for-major-mode mode
    "rdb" 'js-doc-insert-file-doc
    "rdf" (if (configuration-layer/package-used-p 'yasnippet)
              'js-doc-insert-function-doc-snippet
            'js-doc-insert-function-doc)
    "rdt" 'js-doc-insert-tag
    "rdh" 'js-doc-describe-tag))


;; Others

(defun spacemacs//js-setup-checkers ()
  (when-let* ((found (executable-find "eslint_d")))
    (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))

(defun spacemacs/js-format ()
  "Call formatting tool specified in `js-fmt-tool'."
  (interactive)
  (cond
   ((eq js-fmt-tool 'prettier)
    (call-interactively 'prettier-js))
   ((eq js-fmt-tool 'web-beautify)
    (call-interactively 'web-beautify-js))
   (t (error (concat "%s isn't valid js-fmt-tool value."
                     " It should be 'web-beutify or 'prettier.")
             (symbol-name js-fmt-tool)))))
