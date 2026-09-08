;;; packages.el --- Large Language Model Client for Spacemacs  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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
    embark
    eca
    (gptel :toggle llm-client-enable-gptel)
    (gptel-quick :toggle llm-client-enable-gptel
                 :location (recipe :fetcher github
                                   :repo "/karthink/gptel-quick" :files ("*.el")))
    (gptel-agent
     :toggle llm-client-enable-gptel-agent
     :location (recipe :fetcher github
                       :repo "karthink/gptel-agent"
                       :files (:defaults "agents")))
    gptel-magit
    gptel-inline
    macher
    minuet
    org
    window-purpose))

(defun llm-client/init-ellama ()
  "Initialize the `ellama` package and set up keybindings."
  (use-package ellama
    :defer t
    :init
    (spacemacs/declare-prefix "$" "AI")
    (spacemacs/declare-prefix "$e" "Ellama")
    (spacemacs/set-leader-keys "$e" 'ellama-transient-main-menu)))

(defun llm-client/init-gptel ()
  "Initialize the `gptel` package and set up keybindings."
  (use-package gptel
    :defer t
    :config
    (spacemacs/set-leader-keys-for-minor-mode 'gptel-mode
      "ge" 'gptel-end-of-response
      "gm" 'gptel-menu)
    :init
    ;; evilify gptel-context-buffer-mode-map
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :eval-after-load gptel-context
      :mode gptel-context-buffer-mode
      :bindings
      "C-c C-c" #'gptel-context-confirm
      "C-c C-k" #'gptel-context-quit
      "RET"     #'gptel-context-visit
      "n"       #'gptel-context-next
      "p"       #'gptel-context-previous
      "d"       #'gptel-context-flag-deletion)
    ;; set up keybindings
    (spacemacs/declare-prefix "$g" "gptel")
    (spacemacs/set-leader-keys
      "$gd" 'spacemacs/gptel-add-code-doc		; code doc gen
      "$gg" 'gptel                          ; Start a new GPTel session
      "$gs" 'spacemacs//gptel-send-wrapper  ; Send a message to GPTel
      "$gq" 'spacemacs//gptel-abort-wrapper ; Abort any active GPTel process
      "$gm" 'gptel-menu                     ; Open the GPTel menu
      "$gc" 'gptel-add                      ; Add context
      "$gf" 'gptel-add-file                 ; Add a file
      "$go" 'gptel-org-set-topic            ; Set topic in Org-mode
      "$gp" 'gptel-org-set-properties       ; Set properties in Org-mode
      "$gr" 'gptel-rewrite)))               ; Rewrite or refactor test region

(defun llm-client/init-gptel-quick ()
  (use-package gptel-quick
    :defer t))

(defun llm-client/pre-init-embark ()
  (spacemacs|use-package-add-hook embark
    :post-config
    (keymap-set embark-general-map "?" #'gptel-quick)
    (keymap-set embark-url-map "y" #'spacemacs/gptel-yt-summarize)
    (keymap-set embark-region-map "y" #'spacemacs/gptel-summarize-region)))


(defun llm-client/init-gptel-agent ()
  (use-package gptel-agent
    :defer t
    :init
    ;; evilify gptel-context-buffer-mode-map
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :eval-after-load gptel-context
      :mode gptel-context-buffer-mode)
    ;; set up keybindings
    (spacemacs/set-leader-keys
      "$ga" 'gptel-agent                          ; Start a new gptel-agent session
      "$gu" 'gptel-agent-update)                  ; Updates the gptel-agent database
    ;; Config for =gptel-agent=
    :config (gptel-agent-update)))         ;Read files from agents directories

(defun llm-client/post-init-org ()
  "Set up Org-mode keybindings for GPTel."
  (use-package gptel-org
    :commands (gptel-org-set-topic gptel-org-set-properties)
    :after org)
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
      (unwind-protect
          (apply orig-func args)
        (when purpose-mode-was-enabled
          (purpose-mode 1)))))
  (advice-add 'gptel :around #'llm-client/disable-purpose-mode-around-for-gptel))

(defun llm-client/init-minuet ()
  (use-package minuet
    :bind
    (;; ("C-c y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ;; ("C-c i" . #'minuet-show-suggestion) ;; use overlay for completion
     ;; ("C-c m" . #'minuet-configure-provider)
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))

    :init
    ;; if you want to enable auto suggestion.
    ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
    ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

    :config
    ;; You can use M-x minuet-configure-provider to interactively configure provider and model
    (setq minuet-provider 'openai)

    ;; For Evil users: When defining `minuet-ative-mode-map` in insert
    ;; or normal states, the following one-liner is required.

    ;; (add-hook 'minuet-active-mode-hook #'evil-normalize-keymaps)

    ;; This is *not* necessary when defining `minuet-active-mode-map`.

    ;; To minimize frequent overhead, it is recommended to avoid adding
    ;; `evil-normalize-keymaps` to `minuet-active-mode-hook`. Instead,
    ;; bind keybindings directly within `minuet-active-mode-map` using
    ;; standard Emacs key sequences, such as `M-xxx`. This approach should
    ;; not conflict with Evil's keybindings, as Evil primarily avoids
    ;; using `M-xxx` bindings.

    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
    (minuet-set-optional-options minuet-openai-options :max_tokens 64)))

(defun llm-client/init-gptel-magit ()
  (use-package gptel-magit
    :hook (magit-mode . gptel-magit-install)))

(defun llm-client/init-gptel-inline ()
  (use-package gptel-inline
    :init
    (spacemacs/set-leader-keys
      "$gi" 'gptel-inline)
    :after gptel))

(defun llm-client/init-eca ()
  (use-package eca))

(defun llm-client/init-macher ()
  (use-package macher
    :custom
    ;; The org UI has structured conversations and nice content folding.
    (macher-action-buffer-ui 'org)

    :hook
    ;; Set up action buffer behavior to your liking.  Alternately, do
    ;; this more generally in your `gptel-mode-hook'.
    (macher-action-buffer-setup
     . (lambda ()
         ;; Auto-scroll responses.
         (setq-local window-point-insertion-type t)
         ;; Wrap lines.
         (visual-line-mode 1)))

    :config
    ;; Recommended - register macher tools and presets with gptel.
    (macher-install)

    ;; Recommended - enable macher infrastructure for tools/prompts in
    ;; any buffer.  (Actions and presets will still work without this.)
    (macher-enable)

    ;; Adjust buffer positioning to taste.
    ;; (add-to-list
    ;;  'display-buffer-alist
    ;;  '("\\*macher:.*\\*"
    ;;    (display-buffer-in-side-window)
    ;;    (side . bottom)))
    ;; (add-to-list
    ;;  'display-buffer-alist
    ;;  '("\\*macher-patch:.*\\*"
    ;;    (display-buffer-in-side-window)
    ;;    (side . right)))
    ))
