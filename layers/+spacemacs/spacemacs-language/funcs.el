;; -*- lexical-binding: t; -*-

(defun spacemacs//get-all-words-in-active-window ()
  "Get text within the visible window, return list of unique words from the text."
  (let ((start (window-start))  
        (end (window-end)))
    (let* ((visible-text (buffer-substring-no-properties start end)) 
           (words (split-string visible-text "\\W+" t)) 
           (unique-words (delete-dups words)))
      unique-words))) 

(defun spacemacs/dictionary-search (word)
  "Search for WORD in the dictionary.
 If there is a word under cursor, use it; otherwise, prompt to choose from visible words."
  (interactive
   (if-let* ((a-word (current-word)))
       (list a-word)
     (list
      (completing-read "Search: " (spacemacs//get-all-words-in-active-window)))))
  (dictionary-search word))
