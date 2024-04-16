;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(setq spacemacs-misc-packages
      '(evil-collection
        devdocs
        dumb-jump
        request
        deadgrep
        grep
        wgrep))

(defun spacemacs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    ;; Use Helm or Ivy as the selector for dumb-jump.
    (cond
     ((configuration-layer/layer-used-p 'ivy)
      (setq dumb-jump-selector 'ivy))
     ((configuration-layer/layer-used-p 'helm)
      (setq dumb-jump-selector 'helm)))

    ;; Enable xref-backend of dumb-jump. It's chosen only when no better
    ;; options is available
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)))

(defun spacemacs-misc/init-request ()
  (setq request-storage-directory
        (concat spacemacs-cache-directory "request/")))

(defun spacemacs-misc/init-devdocs ()
  (use-package devdocs
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "hbb" #'devdocs-lookup ) 
      (defalias 'spacemacs/browse-docs-online-at-point 'devdocs-search)
      (spacemacs/set-leader-keys "hbd" #'spacemacs/browse-docs-online-at-point)
      (add-hook 'devdocs-mode-hook
                (lambda ()
                  (setq-local imenu-create-index-function #'spacemacs/imenu-eww-headings))))))

(defun spacemacs-misc/post-init-grep ()
  (spacemacs/set-leader-keys-for-major-mode 'grep-mode
    "w" 'spacemacs/grep-change-to-wgrep-mode))

(defun spacemacs-misc/init-wgrep ()
  (add-hook 'spacemacs-editing-style-hook #'spacemacs//set-initial-grep-state)
  (evil-define-key 'normal wgrep-mode-map ",," #'spacemacs/wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" #'spacemacs/wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" #'spacemacs/wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" #'spacemacs/wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",q" #'spacemacs/wgrep-abort-changes-and-quit)
  (evil-define-key 'normal wgrep-mode-map ",s" #'spacemacs/wgrep-save-changes-and-quit))

(defun spacemacs-misc/init-deadgrep ()
  (use-package deadgrep
    :defer t
    :init
    (spacemacs/set-leader-keys "sj" #'deadgrep)
    :config
    (evil-define-key 'normal deadgrep-mode-map "o" #'spacemacs/ace-link-deadgrep)))

(defun spacemacs-misc/pre-init-evil-collection ()
  (add-to-list 'spacemacs-evil-collection-allowed-list 'deadgrep))
