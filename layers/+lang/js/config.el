;;; config.el --- Javascript Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|define-jump-handlers js-mode)

(defvar js-backend 'tern
  "The backend to use for IDE features.
Possible values are `tern', `tide' and `lsp'.
If `nil' then `tern' is the default backend unless `lsp' layer is used.")

(defvar js-fmt-tool 'web-beautify
  "The formatter to format a JavaScript file. Possible values are `web-beautify' and `prettier'.")

(defvar js-import-tool nil
  "The import backend to import modules. Possible values are `import-js' and `nil' to disable.")

(defvar js-repl 'nodejs
  "Repl to be configured by the layer, `skewer' for browser based js, `nodejs' for server based development.")

(defvar js-lsp-linter t
  "If the backend is `lsp', and this variable is non-nil, then
use lsp as the linter, otherwise let flycheck choose the best
linter that's available.")
