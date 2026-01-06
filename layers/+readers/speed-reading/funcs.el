;; -*- lexical-binding: t; -*-

(defun spread-reading/spray-region (start end)
  "Create a temporary buffer with the selected region's content and run spray-mode on it.
The temporary buffer will be killed when spray-mode is quit
(e.g., by pressing 'q' or '<return>' within spray-mode)."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (temp-buffer (generate-new-buffer "*Spray Region*")))
    ;; Switch to the temporary buffer to set it up
    (with-current-buffer temp-buffer
      (insert region-text)
      (goto-char (point-min)) ; Start spraying from the beginning of the text

      ;; Add this lambda to 'spray-mode-hook' locally for this buffer.
      ;; The 'nil 'local' arguments ensure the hook entry is specific to this buffer
      ;; and will be automatically removed when the buffer is killed.
      (add-hook 'spray-mode-hook (lambda ()
                                   ;; The 'spray-mode' variable is nil when the mode is being disabled.
                                   (unless spray-mode
                                     (message "Killing temporary spray buffer...")
                                     ;; Kill the buffer and its window.
                                     ;; If it's the only window, it kills the buffer.
                                     ;; If there are other windows, it just closes the spray window.
                                     (kill-buffer)))
                nil 'local)

      ;; Switch to the new buffer and its window to give it focus.
      (switch-to-buffer temp-buffer)

      ;; Activate spray-mode in the temporary buffer.
      (speed-reading/start-spray))))

(defun speed-reading/start-quickread ()
  "Start quickread speed reading on current buffer at current point."
  (interactive)
  (quickread-mode t)
  (internal-show-cursor (selected-window) nil))

(defun spacemacs//restore-cursor ()
  (unless quickread-mode
    (internal-show-cursor (selected-window) t)))
