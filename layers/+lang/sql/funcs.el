;;; funcs.el --- SQL Layer functions File for Spacemacs  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(defun spacemacs//sql-setup-company ()
  "Conditionally setup company based on backend."
  (pcase sql-backend
    ('company-sql (spacemacs|add-company-backends
                    :backends company-capf
                    :modes sql-mode))
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes sql-mode))))

(defun spacemacs//sql-setup-backend ()
  "Conditionally setup sql backend."
  (when (eq sql-backend 'lsp)
    (lsp-deferred)))

(defun spacemacs//sql-product-completion-table ()
  "Return an alist of (PRODUCT-PRETTY-NAME . PRODUCT-SYMBOL)."
  (mapcar
   (lambda (product)
     (cons (plist-get (cdr product) :name)
           (car product)))
   sql-product-alist))

(defun spacemacs//sql-read-product (pred)
  "Read an SQL product, with completion limited by predicate PRED."
  (let* ((alist (spacemacs//sql-product-completion-table))
         (input (completing-read
                 "SQL Product: "
                 alist pred 'require-match
                 nil 'sql-product-history)))
    (cdr (assoc input alist))))

(defun spacemacs/sql-highlight (product)
  "Set SQL dialect-specific highlighting."
  (interactive (list (spacemacs//sql-read-product nil)))
  (sql-set-product product))

(defun spacemacs/sql-start (product)
  "Set SQL dialect-specific highlighting and start inferior SQLi process."
  (interactive
   (list (spacemacs//sql-read-product
          (lambda (product) (sql-get-product-feature (cdr product) :sqli-program)))))
  (sql-set-product product)
  (sql-product-interactive))

(defun spacemacs/sql-send-string-and-focus ()
  "Send a string to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (call-interactively 'sql-send-string)
    (evil-insert-state)))

(defun spacemacs/sql-send-buffer-and-focus ()
  "Send the buffer to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-buffer)
    (evil-insert-state)))

(defun spacemacs/sql-send-paragraph-and-focus ()
  "Send the paragraph to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-paragraph)
    (evil-insert-state)))

(defun spacemacs/sql-send-region-and-focus (start end)
  "Send region to SQLi and switch to SQLi in `insert state'."
  (interactive "r")
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-region start end)
    (evil-insert-state)))

(defun spacemacs/sql-send-line-and-next-and-focus ()
  "Send the current line to SQLi and switch to SQLi in `insert state'."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region t))
    (sql-send-line-and-next)))

(defun spacemacs/sql-send-string ()
  "Send a string to SQLi and stays in the same region."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region nil))
    (call-interactively 'sql-send-string)))

(defun spacemacs/sql-send-buffer ()
  "Send the buffer to SQLi and stays in the same region."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region nil))
    (sql-send-buffer)))

(defun spacemacs/sql-send-paragraph ()
  "Send the paragraph to SQLi and stays in the same region."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region nil))
    (sql-send-paragraph)))

(defun spacemacs/sql-send-region (start end)
  "Send region to SQLi and stays in the same region."
  (interactive "r")
  (let ((sql-pop-to-buffer-after-send-region nil))
    (sql-send-region start end)))

(defun spacemacs/sql-send-line-and-next ()
  "Send the current line to SQLi and stays in the same region."
  (interactive)
  (let ((sql-pop-to-buffer-after-send-region nil))
    (sql-send-line-and-next)))

(defun spacemacs/sql-ejc-toggle-table-mode ()
  (interactive)
  (if (equal ejc-result-table-impl 'orgtbl-mode)
      (setq ejc-result-table-impl 'ejc-result-mode)
    (setq ejc-result-table-impl 'orgtbl-mode))
  (message "set %s to handle table result" ejc-result-table-impl))
