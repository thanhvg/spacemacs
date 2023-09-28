(defun spacemacs/node-nodejs-repl-set-key-bindings (mode)
  (spacemacs/declare-prefix-for-mode mode "ms" "nodejs-repl")
  (spacemacs/set-leader-keys-for-major-mode mode
    "'" 'nodejs-repl
    "ss" 'nodejs-repl
    "si" 'nodejs-repl-switch-to-repl
    "se" 'nodejs-repl-send-last-expression
    "sE" (lambda ()
           (interactive)
           (nodejs-repl-send-last-expression)
           (nodejs-repl-switch-to-repl))
    "sb" 'nodejs-repl-send-buffer
    "sB" (lambda ()
           (interactive)
           (nodejs-repl-send-buffer)
           (nodejs-repl-switch-to-repl))
    "sl" 'nodejs-repl-send-line
    "sL" (lambda ()
           (interactive)
           (nodejs-repl-send-line)
           (nodejs-repl-switch-to-repl))
    "sr" 'nodejs-repl-send-region
    "sR" (lambda (start end)
           (interactive "r")
           (nodejs-repl-send-region start end)
           (nodejs-repl-switch-to-repl)))
  (spacemacs/declare-prefix-for-mode mode
    "msE" "nodejs-send-last-expression-and-focus")
  (spacemacs/declare-prefix-for-mode mode
    "msB" "nodejs-send-buffer-and-focus")
  (spacemacs/declare-prefix-for-mode mode
    "msL" "nodejs-send-line-and-focus")
  (spacemacs/declare-prefix-for-mode mode
    "msR" "nodejs-send-region-and-focus"))

(defun spacemacs/node-js-doc-set-key-bindings (mode)
  "Setup the key bindings for `js-doc' for the given MODE."
  (spacemacs/declare-prefix-for-mode mode "mrd" "documentation")
  (spacemacs/set-leader-keys-for-major-mode mode
    "rdb" 'js-doc-insert-file-doc
    "rdf" (if (configuration-layer/package-used-p 'yasnippet)
              'js-doc-insert-function-doc-snippet
            'js-doc-insert-function-doc)
    "rdt" 'js-doc-insert-tag
    "rdh" 'js-doc-describe-tag))


(defun spacemacs/node-js-doc-require ()
  "Lazy load js-doc"
  (require 'js-doc))
