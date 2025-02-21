;;; packages.el --- Large Language Model Client for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <mail+spacemacs@codrut.pro>
;; Author: Alexander Matyasko <alexander.matyasko@gmail.com>
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


(defconst llm-client-packages
  '((ellama :toggle llm-client-enable-ellama)
    (gptel :toggle llm-client-enable-gptel)
    (aidermacs :location built-in)
    ;; (aidermacs :location
    ;;        (recipe :fetcher github
    ;;                :repo "MatthewZMD/aidermacs" :files ("*.el")))
    org
    window-purpose))

(defun llm-client/init-ellama ()
  "Initialize the `ellama` package and set up keybindings."
  (use-package ellama
    :defer t
    :ensure t
    :init
    (spacemacs/declare-prefix "$e" "Ellama")
    (spacemacs/set-leader-keys "$e" 'ellama-transient-main-menu)))

(defun llm-client/init-gptel ()
  "Initialize the `gptel` package and set up keybindings."
  (use-package gptel
    :defer t
    :ensure t
    :config
    (spacemacs/set-leader-keys-for-minor-mode 'gptel-mode
      "ge" 'gptel-end-of-response
      "gm" 'gptel-menu)
    :init
    ;; evilify gptel-context-buffer-mode-map
    (require 'gptel-context)
    (evil-set-initial-state 'gptel-context-buffer-mode 'evilified)
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :mode gptel-context-buffer-mode
      :bindings
      "C-c C-c" #'gptel-context-confirm
      "C-c C-k" #'gptel-context-quit
      "RET"     #'gptel-context-visit
      "n"       #'gptel-context-next
      "p"       #'gptel-context-previous
      "d"       #'gptel-context-flag-deletion)
    ;; set up keybindings
    (spacemacs/declare-prefix "ag" "Gptel")
    (spacemacs/set-leader-keys
      "agg" 'gptel                          ; Start a new GPTel session
      "ags" 'spacemacs//gptel-send-wrapper  ; Send a message to GPTel
      "agq" 'spacemacs//gptel-abort-wrapper ; Abort any active GPTel process
      "agm" 'gptel-menu                     ; Open the GPTel menu
      "agc" 'gptel-add                      ; Add context
      "agf" 'gptel-add-file                 ; Add a file
      "ago" 'gptel-org-set-topic            ; Set topic in Org-mode
      "agp" 'gptel-org-set-properties)))    ; Set properties in Org-mode

(defun llm-client/post-init-org ()
  "Set up Org-mode keybindings for GPTel."
  (spacemacs/declare-prefix-for-mode 'org-mode "m$g" "Gptel")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "$go" 'gptel-org-set-topic
    "$gp" 'gptel-org-set-properties))

(defun llm-client/post-init-window-purpose ()
  ;; TODO: Temporary fix to avoid the error when using window-purpose
  ;; see https://github.com/karthink/gptel/issues/237 for details
  ;; (purpose-set-extension-configuration
  ;;  :llm-client-layer
  ;;  (purpose-conf :mode-purposes '((gptel-mode . chat))))
  (defun llm-client/disable-purpose-mode-around-for-gptel (orig-func &rest args)
    "Advice function to disable purpose-mode before calling ORIG-FUNC with ARGS."
    (let ((purpose-mode-was-enabled (bound-and-true-p purpose-mode)))
      (when purpose-mode-was-enabled
        (purpose-mode -1))
      (apply orig-func args)
      (when purpose-mode-was-enabled
        (purpose-mode 1))))
  (advice-add 'gptel :around #'llm-client/disable-purpose-mode-around-for-gptel))

(defun llm-client/init-aidermacs ()
  (use-package aidermacs
    :load-path "~/git/aidermacs"
    :config
    (spacemacs/set-leader-keys
      "aa" 'aidermacs-transient-menu)))
