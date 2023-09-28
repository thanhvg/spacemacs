;;; packages.el --- JS Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq js-packages
      '(
        add-node-modules-path
        company
        evil-matchit
        flycheck
        (js :location built-in)
        (typescript-ts-mode :location built-in)
        js-doc
        nodejs-repl
        ;; org
        prettier-js
        ;; skewer-mode
        web-beautify))

(defun js/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(css-mode-hook))
  (spacemacs/add-to-hooks #'add-node-modules-path js-modes-hooks))

(defun js/post-init-company ()
  (dolist (hook js-modes-local-vars-hooks)
    (add-hook hook #'spacemacs/js-setup-company)))

(defun js/post-init-evil-matchit ()
  (spacemacs/add-to-hooks #'turn-on-evil-matchit-mode js-modes-hooks))

(defun js/post-init-flycheck ()
  (dolist (mode js-modes)
    (spacemacs/enable-flycheck mode))
  ;; (spacemacs/add-to-hooks #'spacemacs//js-setup-checkers js-modes-local-vars-hooks 'append)
  )

(defun js/init-js-doc ()
  (use-package js-doc
    :defer t
    :init (spacemacs/js-doc-set-key-bindings 'js-ts-mode)))

(defun js/init-js ()
  (put 'js-backend 'safe-local-variable 'symbolp)
  (spacemacs/add-to-hooks #'spacemacs//js-setup-backend js-modes-local-vars-hooks))

(defun js/init-nodejs-repl ()
  (when (eq js-repl 'nodejs)
    (use-package nodejs-repl
      :defer t
      :init
      (spacemacs/register-repl 'nodejs-repl
                               'nodejs-repl
                               "nodejs-repl")
      :config
      (progn
        (spacemacs/declare-prefix-for-mode 'js-mode "ms" "nodejs-repl")
        (spacemacs/set-leader-keys-for-major-mode 'js-mode
          "'" 'nodejs-repl
          "ss" 'nodejs-repl
          "si" 'nodejs-repl-switch-to-repl
          "se" 'nodejs-repl-send-last-expression
          "sE" (lambda ()
                 (interactive)
                 (nodejs-repl-send-last-expression)
                 (nodejs-repl-switch-to-repl))
          "sb" 'nodejs-repl-send-buffer
          "sB" (lambda ()
                 (interactive)
                 (nodejs-repl-send-buffer)
                 (nodejs-repl-switch-to-repl))
          "sl" 'nodejs-repl-send-line
          "sL" (lambda ()
                 (interactive)
                 (nodejs-repl-send-line)
                 (nodejs-repl-switch-to-repl))
          "sr" 'nodejs-repl-send-region
          "sR" (lambda (start end)
                 (interactive "r")
                 (nodejs-repl-send-region start end)
                 (nodejs-repl-switch-to-repl)))
        (spacemacs/declare-prefix-for-mode 'js-mode
          "msE" "nodejs-send-last-expression-and-focus")
        (spacemacs/declare-prefix-for-mode 'js-mode
          "msB" "nodejs-send-buffer-and-focus")
        (spacemacs/declare-prefix-for-mode 'js-mode
          "msL" "nodejs-send-line-and-focus")
        (spacemacs/declare-prefix-for-mode 'js-mode
          "msR" "nodejs-send-region-and-focus")
        ))))

(defun js/init-typescript-ts-mode ())

;; (defun js/pre-init-org ()
;;   (spacemacs|use-package-add-hook org
;;     :post-config (add-to-list 'org-babel-load-languages '(js . t))))

(defun js/pre-init-prettier-js ()
  (when (eq js-fmt-tool 'prettier)
    (dolist (mode js-modes)
      (add-to-list 'spacemacs--prettier-modes mode))))

(defun js/pre-init-web-beautify ()
  (when (eq js-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'js-ts-mode 'web-beautify-js))))
