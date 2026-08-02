;; -*- lexical-binding: t; -*-

(defun spacemacs/elfeed-open-with-eww ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (elfeed-show-visit)))
