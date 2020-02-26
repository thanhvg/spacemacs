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
        (js-mode :location built-in)
        js-doc
        nodejs-repl
        ;; org
        prettier-js
        skewer-mode
        tern
        tide
        web-beautify))

(defun js/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(css-mode-hook
                                                    js-mode-hook)))
(defun js/post-init-company ()
  (add-hook 'js-mode-local-vars-hook #'spacemacs//js-setup-company))

(defun js/post-init-evil-matchit ()
  (add-hook `js-mode-hook `turn-on-evil-matchit-mode))

(defun js/post-init-flycheck ()
  (spacemacs/enable-flycheck 'js-mode)
  (add-hook 'js-mode-hook #'spacemacs//js-setup-checkers 'append))

(defun js/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'spacemacs--import-js-modes (cons 'js-mode 'js-mode-hook))))

(defun js/init-js-doc ()
  (use-package js-doc
    :defer t
    :init (spacemacs/js-doc-set-key-bindings 'js-mode)))

(defun js/init-js-mode ()
  (use-package js-mode
    :defer t
    ;; :mode (("\\.m?js\\'"  . js2-mode))
    :init
    (progn
      (add-hook 'js-mode-local-vars-hook #'spacemacs//js-setup-backend)
      ;; (add-hook 'js-mode-local-vars-hook #'spacemacs//javascript-setup-next-error-fn)
      ;; safe values for backend to be used in directory file variables
      (dolist (value '(lsp tern tide))
        (add-to-list 'safe-local-variable-values
                     (cons 'js-backend value))))
    :config
    (progn
      ;; prefixes
      ;; (spacemacs/declare-prefix-for-mode 'js2-mode "mh" "documentation")
      ;; (spacemacs/declare-prefix-for-mode 'js2-mode "mg" "goto")
      ;; (spacemacs/declare-prefix-for-mode 'js2-mode "mr" "refactor")
      ;; (spacemacs/declare-prefix-for-mode 'js2-mode "mz" "folding")
      ;; key bindings
      )))


(defun js/init-nodejs-repl ()
  (when (eq js-repl 'nodejs)
    (use-package nodejs-repl
      :defer nil
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

;; (defun js/pre-init-org ()
;;   (spacemacs|use-package-add-hook org
;;     :post-config (add-to-list 'org-babel-load-languages '(js . t))))

(defun js/pre-init-prettier-js ()
  (when (eq js-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'js-mode)))

(defun js/init-skewer-mode ()
  (when (eq js-repl 'skewer)
    (use-package skewer-mode
      :defer t
      :init
      (progn
        (spacemacs/register-repl 'skewer-mode
                                 'spacemacs/skewer-start-repl
                                 "skewer")
        (add-hook 'js-mode-hook 'skewer-mode))
      :config
      (progn
        (spacemacs|hide-lighter skewer-mode)
        (spacemacs/declare-prefix-for-mode 'js-mode "ms" "skewer")
        (spacemacs/declare-prefix-for-mode 'js-mode "me" "eval")
        (spacemacs/set-leader-keys-for-major-mode 'js2-mode
          "'" 'spacemacs/skewer-start-repl
          "ee" 'skewer-eval-last-expression
          "eE" 'skewer-eval-print-last-expression
          "sb" 'skewer-load-buffer
          "sB" 'spacemacs/skewer-load-buffer-and-focus
          "si" 'spacemacs/skewer-start-repl
          "sf" 'skewer-eval-defun
          "sF" 'spacemacs/skewer-eval-defun-and-focus
          "sr" 'spacemacs/skewer-eval-region
          "sR" 'spacemacs/skewer-eval-region-and-focus
          "ss" 'skewer-repl)))))

(defun js/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'js-mode))

(defun js/post-init-tide ()
  (add-to-list 'tide-managed-modes 'js-mode))

(defun js/pre-init-web-beautify ()
  (when (eq js-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'js-mode 'web-beautify-js))))
