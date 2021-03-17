;;; packages.el --- Language Server Protocol Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
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


(defconst lsp-packages
  '(
    lsp-mode
    (lsp-ui :toggle lsp-use-lsp-ui)
    (helm-lsp :requires helm)
    (lsp-ivy :requires ivy)
    (lsp-treemacs :requires treemacs)
    (lsp-origami :requires lsp-mode)
    popwin))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :config
    (progn
      ;; problem with bind-map for evil key these two bind to "SPC" must go
      ;; first before spacemacs/lsp-bind-upstream-keys which binds to SPC m
      (spacemacs/lps-bind-breadcrumb)
      (spacemacs/lps-bind-breadcrumb-narrow)
      (if lsp-use-upstream-bindings
          (spacemacs/lsp-bind-upstream-keys)
        (spacemacs/lsp-bind-keys))
      (setq lsp-prefer-capf t)
      (setq lsp-keep-workspace-alive nil)
      ;; deal with mutli-root lsp servers (java)
      ;; https://emacs-lsp.github.io/lsp-mode/page/faq/
      (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
      (add-hook 'lsp-after-open-hook (lambda ()
                                       "Setup xref jump handler"
                                       (spacemacs//setup-lsp-jump-handler))))))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :config
    (progn
      (if lsp-remap-xref-keybindings
          (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
                 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

      (spacemacs/lsp-define-key
       lsp-ui-peek-mode-map
       "h" #'lsp-ui-peek--select-prev-file
       "j" #'lsp-ui-peek--select-next
       "k" #'lsp-ui-peek--select-prev
       "l" #'lsp-ui-peek--select-next-file
       )
      )))

(defun lsp/init-helm-lsp ()
  (use-package helm-lsp :defer t))

(defun lsp/init-lsp-ivy ()
  (use-package lsp-ivy :defer t))

(defun lsp/init-lsp-treemacs ()
  (use-package lsp-treemacs :defer t))

(defun lsp/init-lsp-origami ()
  (use-package lsp-origami
    :defer t
    :init
    (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)))

(defun lsp/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*lsp-help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          popwin:special-display-config)))
