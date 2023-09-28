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
        npm-mode
        ;; org
        prettier-js
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

(defun js/pre-init-js-doc ()
  (spacemacs/add-to-hooks #'spacemacs/node-js-doc-require js-modes-hooks)
  (spacemacs|use-package-add-hook js-doc
    :post-init (dolist (mode js-modes)
                   (spacemacs/node-js-doc-set-key-bindings mode))))

(defun js/init-js ()
  (put 'js-backend 'safe-local-variable 'symbolp)
  (spacemacs/add-to-hooks #'spacemacs//js-setup-backend js-modes-local-vars-hooks))

(defun js/post-init-nodejs-repl ()
  (dolist (mode js-modes)
   (spacemacs/node-nodejs-repl-set-key-bindings mode)))

(defun js/init-typescript-ts-mode ())

;; (defun js/pre-init-org ()
;;   (spacemacs|use-package-add-hook org
;;     :post-config (add-to-list 'org-babel-load-languages '(js . t))))


(defun js/post-init-npm-mode ()
  (spacemacs/add-to-hooks #'npm-mode js-modes-hooks))

(defun js/pre-init-prettier-js ()
  (when (eq js-fmt-tool 'prettier)
    (dolist (mode js-modes)
      (add-to-list 'spacemacs--prettier-modes mode))))

(defun js/pre-init-web-beautify ()
  (when (eq js-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'js-ts-mode 'web-beautify-js))))
