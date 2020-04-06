;;; funcs.el --- Tide  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//tide-setup-prefix ()
  "This one should run only once, otherwise `which-key' will become very slow #12455"
  (dolist (mode tide-managed-modes)
    (spacemacs/declare-prefix-for-mode mode "mE" "errors")
    (spacemacs/declare-prefix-for-mode mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode mode "mh" "help")
    (spacemacs/declare-prefix-for-mode mode "mn" "name")
    (spacemacs/declare-prefix-for-mode mode "mr" "refactor")
    (spacemacs/declare-prefix-for-mode mode "mS" "server")))

(defun spacemacs//tide-setup-bindings ()
  "Define keys bindings for `tide-mode'"
  (spacemacs/set-leader-keys-for-minor-mode 'tide-mode
    "Ee" #'tide-fix
    "Ed" #'tide-add-tslint-disable-next-line
    "Ep" #'tide-project-errors
    "gb" #'tide-jump-back
    "gd" #'tide-jump-to-definition
    "gt" #'spacemacs/typescript-jump-to-type-def
    "gr" #'tide-references
    "hh" #'tide-documentation-at-point
    "ri" #'tide-organize-imports
    "rr" #'tide-rename-symbol
    "rR" #'tide-refactor
    "rf" #'tide-rename-file
    "Sr" #'tide-restart-server
    "Sj" #'spacemacs//tide-create-jsconfig-file))

(defun spacemacs//tide-setup ()
  "Setup tide backend.
Must be called by a layer using tide."
  (tide-hl-identifier-mode +1)
  (tide-setup))


(defun spacemacs//tide-setup-company (&rest modes)
  "Setup tide company for MODES.
Must be called by a layer using tide."
  (eval `(spacemacs|add-company-backends
           :backends company-tide
           :modes ,@modes
           :append-hooks nil
           :call-hooks t))
  (company-mode))

(defun spacemacs//tide-setup-eldoc ()
  "Setup eldoc for tide."
  (eldoc-mode))

(defun spacemacs//tide-setup-jump-handle ()
  "Set tide as top jump handle."
  (add-to-list 'spacemacs-jump-handlers '(tide-jump-to-definition :async t)))
  ;; (setq spacemacs-jump-handlers
  ;;       (append '((tide-jump-to-definition :async t))
  ;;               spacemacs-default-jump-handlers)))

(defun spacemacs//tide-create-jsconfig-file ()
  "Create a jsconfig file at project root."
  (interactive)
  (let ((jsconfig (cdr (project-current))))
    (if jsconfig
        (let ((jsconfig-file (concat jsconfig "jsconfig.json")))
          (if (file-exists-p jsconfig-file)
              (message "File exists")
            (with-temp-file jsconfig-file
              (insert tide-jsconfig-content))))
      (message "Project not found"))))
