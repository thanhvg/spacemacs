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

(defvar js-backend 'tide
  "The backend to use for IDE features.
Possible values are `tide' and `lsp'.")

(defvar js-fmt-tool 'web-beautify
  "The formatter to format a JavaScript file. Possible values are `web-beautify' and `prettier'.")

(defvar js-repl 'nodejs
  "Repl to be configured by the layer, `skewer' for browser based js, `nodejs' for server based development.")

(defvar js-modes
  '(js-ts-mode typescript-ts-mode tsx-ts-mode)
  "list of js based modes.")

(defvar js-modes-hooks
  '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook)
  "list of js based mode hooks.")

(defvar js-modes-local-vars-hooks
  '(js-ts-mode-local-vars-hook typescript-ts-mode-local-vars-hook  tsx-ts-mode-local-vars-hook)
  "list of js based modes local vars hook.")
