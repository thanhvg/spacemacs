(defun spacemacs//lsp-bridge-turn-off-incompatible-mode ()
  (company-mode -1))

(defun spacemacs//lsp-bridge-setup ()
  (company-mode -1)
  (add-to-list 'spacemacs-jump-handlers '(lsp-bridge-find-def :async t)))



;; extra keymap setting for `acm-mode', inspired by `company-mode''s `company-active-map'

;; two layers of indirection.

;; the idea is to has a (t extra-acm-mode) added in `emulation-mode-map-alists'.
;; extra-acm-mode is local buffer value, it is nill when acm-mode is not on.
;; Otherwise it binds to `spacemacs-acm-active-map'

(defvar-local spacemacs--acm-my-keymap nil
  "Local variable to hold the custom keymap for ACM (Auto Completion Mode).")

(defvar spacemacs--acm-emulation-alist '((t . nil))
  "Emulation alist for managing keymaps in ACM in Spacemacs.")

(defvar spacemacs-acm-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-j") 'acm-select-next)
    (define-key keymap (kbd "C-k") 'acm-select-prev)
    keymap)
  "Keymap that is enabled during an active completion.")

(defun spacemacs--acm-enable-overriding-keymap (keymap)
  "Enable the provided KEYMAP for overriding the default keybindings in ACM."
  (spacemacs--acm-uninstall-map)
  (setq spacemacs--acm-my-keymap keymap))

(defun spacemacs--acm-ensure-emulation-alist ()
  "Ensure that the emulation alist includes `spacemacs--acm-emulation-alist` at the head."
  (unless (eq 'spacemacs--acm-emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'spacemacs--acm-emulation-alist
                (delq 'spacemacs--acm-emulation-alist emulation-mode-map-alists)))))

(defun spacemacs--acm-install-map ()
  "Install the current keymap into the emulation alist if it is not already present."
  (if (and (null (cdar spacemacs--acm-emulation-alist))
           spacemacs--acm-my-keymap)
      (setq-local spacemacs--acm-emulation-alist `((t . ,spacemacs--acm-my-keymap)))))

(defun spacemacs--acm-uninstall-map ()
  "Uninstall the current keymap from the emulation alist."
  (kill-local-variable 'spacemacs--acm-emulation-alist))

(defun spacemacs--acm ()
  "Activate or deactivate the ACM mode based on the current state of `acm-mode`."
  (if acm-mode
      (progn (spacemacs--acm-enable-overriding-keymap spacemacs-acm-active-map)
             (spacemacs--acm-install-map))
    (spacemacs--acm-uninstall-map)))
