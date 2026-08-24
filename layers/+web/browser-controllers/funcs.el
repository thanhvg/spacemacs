(defun spacemacs/browser-gt-start ()
  (interactive)
  (require 'browser-gt-www)         ; SAVE_PAGE
  (require 'browser-gt-chatgpt)     ; CHATGPT
  (require 'browser-gt-youtube)     ; YOUTUBE + YOUTUBE_TRANSCRIPT
  (require 'browser-gt-babel)       ; org-babel browser-gt-js blocks
  (require 'browser-gt-tab-manager) ; M-x browser-gt-tab-manager
  (require 'browser-gt-url-handler) ; browser-gt-browse-url + browser-gt-url-routes))
  (browser-gt-start))



