;;; packages.el --- cemosv layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
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

(defconst cemosv-packages
  '(auto-highlight-symbol
    imenu
    marginalia
    (cemosv-spacemacs-help :location local)
    consult
    embark
    embark-consult
    orderless
    (selectrum :toggle (eq cemosv-engine 'selectrum))
    (vertico :toggle (eq cemosv-engine 'vertigo))
    (grep :location built-in)
    wgrep))

(defun cemosv/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] consult-line  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" spacemacs/consult-line :exit t)
        ("f" spacemacs/cemovs-search-auto :exit t)
        ("/" spacemacs/cemovs-search-projectile-auto :exit t)))))

(defun cemosv/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'consult-imenu))

(defun cemosv/init-marginalia ()
  (use-package marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode)))

(defun cemosv/init-consult ()
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c b" . consult-bookmark)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ("<help> a" . consult-apropos)            ;; orig. apropos-command
           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-project-imenu)
           ;; M-s bindings (search-map)
           ("M-s f" . consult-find)
           ("M-s L" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s m" . consult-multi-occur)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch)
           :map isearch-mode-map
           ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
           ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch

    ;; Enable automatic preview at point in the *Completions* buffer.
    ;; This is relevant when you use the default completion UI,
    ;; and not necessary for Selectrum, Vertico etc.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init
    (spacemacs/set-leader-keys
      "bB" 'consult-buffer
      "bb" 'spacemacs/cemovs-switch-to-buffer
      ;; "ji" 'consult-imenu
      "ss" 'consult-line
      "sS" 'spacemacs/consult-line
      "sk" 'consult-keep-lines
      "rL" 'consult-complex-command
      "su" 'consult-focus-lines
      "sf" 'spacemacs/cemovs-search-auto
      "sd" 'spacemacs/cemovs-search-dir
      "sp" 'spacemacs/cemovs-search-projectile
      "*" 'spacemacs/cemovs-search-default
      "/" 'spacemacs/cemovs-search-projectile-auto
      )

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark
     :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; Optionally configure a function which returns the project root directory.
    ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
    ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
    ))

(defun cemosv/init-embark ()
  (use-package embark
    :bind
    (("M-o" . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    ;; (add-to-list 'display-buffer-alist
    ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    ;;                nil
    ;;                (window-parameters (mode-line-format . none))))
    ))

(defun cemosv/init-embark-consult ()
  (use-package embark-consult
    :ensure t
    :after (embark consult)
    :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(defun cemosv/init-orderless ()
  (use-package orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(defun cemosv/init-selectrum ()
  (use-package selectrum
    :init
    (selectrum-mode)
    (spacemacs/set-leader-keys
      dotspacemacs-emacs-command-key 'execute-extended-command
      "rl" 'selectrum-repeat
      "sl" 'selectrum-repeat)
    :config
    ;; TODO can we just use `minibuffer-mode-map'?
    (define-key selectrum-minibuffer-map (kbd "C-j") 'selectrum-next-candidate)
    (define-key selectrum-minibuffer-map (kbd "C-r") 'consult-history)
    (define-key selectrum-minibuffer-map (kbd "C-k") 'selectrum-previous-candidate)))

(defun cemosv/init-vertico ()
  (use-package vertico
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    ;; Grow and shrink minibuffer
    ;;(setq resize-mini-windows t)
    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    ;; (setq vertico-cycle t)

    (vertico-mode)))


(defun spacemacs/cemosv-wgrep-change-to-wgrep-mode ()
  (interactive)
  (wgrep-change-to-wgrep-mode)
  (evil-normal-state))

(defun cemosv/post-init-grep ()
  (spacemacs/set-leader-keys-for-major-mode 'grep-mode
    "w" 'spacemacs/cemosv-wgrep-change-to-wgrep-mode
    "s" 'wgrep-save-all-buffers))

(defun cemosv/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))


(defun cemosv/init-cemosv-spacemacs-help ()
  (use-package cemosv-spacemacs-help
    :init
    (spacemacs/set-leader-keys
      "h ."   'cemosv-spacemacs-help-dotspacemacs
      "h SPC" 'cemosv-spacemacs-help
      "h f"   'cemosv-spacemacs-help-faq
      "h l"   'cemosv-spacemacs-help-layers
      "h p"   'cemosv-spacemacs-help-packages
      "h r"   'cemosv-spacemacs-help-docs
      "h t"   'cemosv-spacemacs-help-toggles)))
