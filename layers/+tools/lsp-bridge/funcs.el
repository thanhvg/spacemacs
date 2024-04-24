(defun spacemacs//lsp-bridge-turn-off-incompatible-mode ()
  (company-mode -1))

(defun spacemacs//lsp-bridge-setup ()
  (company-mode -1)
  (add-to-list 'spacemacs-jump-handlers '(lsp-bridge-find-def :async t)))
