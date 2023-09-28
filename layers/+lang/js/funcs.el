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

(defun spacemacs//js-setup-backend ()
  "Conditionally setup javascript backend."
  (pcase js-backend
    ('tide (spacemacs//tide-setup))
    ('lsp (lsp))))

(defun spacemacs//js-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase js-backend
    ('lsp (spacemacs//js-setup-lsp-dap))))


;; lsp
(defun spacemacs//js-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-firefox)
  (require 'dap-chrome))


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


;; company

(defun spacemacs/js-setup-company ()
  (company-mode)
  (when (equal js-backend 'tide)
    (spacemacs/setup-company-backends-for-buffer 'company-tide)
    ;; (spacemacs|add-company-backends
    ;;  :backends company-tide
    ;;  :modes js-based-ts-mode
    ;;  :append-hooks nil
    ;;  :call-hooks t)
    ))

;; Others

;; (defun spacemacs//js-setup-checkers ()
;;   (when-let* ((found (executable-find "eslint_d")))
;;     (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))
(defun spacemacs//js-setup-checkers ())

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
