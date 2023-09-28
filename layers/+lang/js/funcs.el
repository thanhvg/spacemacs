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
