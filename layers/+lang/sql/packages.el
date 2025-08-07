;;; packages.el --- sql Layer packages File for Spacemacs  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
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


(setq sql-packages
      '(
        company
        (ejc-sql :location (recipe
                            :fetcher github
                            :repo "dvzubarev/ejc-sql"
                            :files (:defaults "project.clj" "src" "snippets")))
        org
        sql
        (sql-indent :location elpa :toggle sql-auto-indent)
        (sqlfmt :location local)
        (sqlup-mode :toggle sql-capitalize-keywords)
        ))

(defun sql/init-sql ()
  (use-package sql
    :defer t
    :init
    (spacemacs/register-repl 'sql 'spacemacs/sql-start "sql")
    (add-hook 'sql-mode-hook
              'spacemacs//sql-setup-backend)
    :config
    (setq
     ;; should not set this to anything else than nil
     ;; the focus of SQLi is handled by spacemacs conventions
     sql-pop-to-buffer-after-send-region nil)
    (spacemacs/declare-prefix-for-mode 'sql-mode "mb" "buffer")
    (spacemacs/declare-prefix-for-mode 'sql-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'sql-mode "mh" "dialects")
    (spacemacs/declare-prefix-for-mode 'sql-mode "ml" "listing")
    (spacemacs/declare-prefix-for-mode 'sql-mode "ms" "REPL")
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "'" 'spacemacs/sql-start

      ;; sqli buffer
      "bb" 'sql-show-sqli-buffer
      "bc" 'sql-connect
      "bs" 'sql-set-sqli-buffer

      ;; dialects
      "hk" 'spacemacs/sql-highlight

      ;; repl
      "sb" 'sql-send-buffer
      "sB" 'spacemacs/sql-send-buffer-and-focus
      "si" 'spacemacs/sql-start
      ;; paragraph gets "f" here because they can be assimilated to functions.
      ;; If you separate your commands in a SQL file, this key will send the
      ;; command around point, which is what you probably want.
      "sf" 'spacemacs/sql-send-paragraph
      "sF" 'spacemacs/sql-send-paragraph-and-focus
      "sl" 'spacemacs/sql-send-line-and-next
      "sL" 'spacemacs/sql-send-line-and-next-and-focus
      "sq" 'spacemacs/sql-send-string
      "sQ" 'spacemacs/sql-send-string-and-focus
      "sr" 'spacemacs/sql-send-region
      "sR" 'spacemacs/sql-send-region-and-focus

      ;; listing
      "la" 'sql-list-all
      "lt" 'sql-list-table)

    (spacemacs/declare-prefix-for-mode 'sql-interactive-mode "mb" "buffer")
    (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode
      ;; sqli buffer
      "br" 'sql-rename-buffer
      "bS" 'sql-save-connection)

    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))

    ;; lsp-sqls
    (let ((path-config (cond
                        ((equal sql-lsp-sqls-workspace-config-path 'workspace) "workspace")
                        ((equal sql-lsp-sqls-workspace-config-path 'root) "root")
                        (t nil))))
      (setq lsp-sqls-workspace-config-path path-config))))

(defun sql/init-sql-indent ()
  (use-package sql-indent
    :defer t
    :init (add-hook 'sql-mode-hook 'sqlind-minor-mode)
    :config (spacemacs|hide-lighter sqlind-minor-mode)))

(defun sql/init-sqlfmt ()
  (use-package sqlfmt
    :commands sqlfmt-buffer
    :init
    (spacemacs/declare-prefix-for-mode 'sql-mode "m=" "formatting")
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "=r" 'sqlfmt-region
      "==" 'sqlfmt-buffer)))

(defun sql/init-sqlup-mode ()
  (use-package sqlup-mode
    :defer t
    :init
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (unless sql-capitalize-keywords-disable-interactive
      (add-hook 'sql-interactive-mode-hook 'sqlup-mode))
    (spacemacs/set-leader-keys-for-major-mode 'sql-mode
      "c" 'sqlup-capitalize-keywords-in-region)
    :config
    (spacemacs|hide-lighter sqlup-mode)
    (setq sqlup-blacklist (append sqlup-blacklist
                                  sql-capitalize-keywords-blacklist))))
(defun sql/init-ejc-sql ()
  (use-package ejc-sql
    :defer t
    :commands ejc-create-connection
    :init
    (setq clomacs-httpd-default-port 1979) ; Use a port other than 8080.
    (add-hook 'ejc-sql-minor-mode-hook
              (lambda ()
                (ejc-eldoc-setup)))
    :config
    (add-hook 'ejc-sql-minor-mode-hook
              (lambda ()
                (ejc-eldoc-setup)))
    (define-key ejc-command-map
                (kbd "q")
                #'ejc-quit-connection)
    (define-key ejc-command-map
                (kbd "k")
                #'ejc-invalidate-cache)
    (define-key ejc-command-map
                (kbd "m")
                #'spacemacs/sql-ejc-toggle-table-mode)))

(defun sql/post-init-company ()
  (spacemacs//sql-setup-company)
  (setq ejc-complete-on-dot t)
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (require 'ejc-company)
              (push 'ejc-company-backend company-backends)
              (company-mode t))))

(defun sql/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(sql . t))))
