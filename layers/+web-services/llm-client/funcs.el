;; -*- lexical-binding: t; -*-
(defun spacemacs//gptel-send-wrapper ()
  "Wrapper function for gptel-send that sets the flag."
  (interactive)
  (call-interactively 'gptel-send)
  (setq llm-client--gptel-send-called t))

(defun spacemacs//gptel-abort-wrapper ()
  "Wrapper function for gptel-abort that checks if gptel-send has been called."
  (interactive)
  (if llm-client--gptel-send-called
      (call-interactively 'gptel-abort)))


(defun spacemacs/gptel-add-code-doc (bounds &optional directive)
  "Add documentation to the code within BOUNDS using GPT-el.

BOUNDS is a cons cell of (START . END) indicating the region
to be documented.  If the region is active, the region's
boundaries are used.  Otherwise, the current defun is used.

Optional DIRECTIVE is a string providing specific instructions
to GPT-el about how to generate the documentation.  If not
provided, a default directive is used that instructs GPT-el to
add documentation in the style of the current major mode.

The code within BOUNDS is replaced with the documented version
returned by GPT-el.  The process involves sending a request to
GPT-el, and upon receiving a response, inserting the documented
code into the buffer."
  (interactive
   (list
    (cond
     ((use-region-p) (cons (region-beginning) (region-end)))
     (t (cons (save-excursion (beginning-of-defun) (point))
              (save-excursion (end-of-defun) (point)))))
    (and current-prefix-arg
         (read-string "Directive: "
                      "You are a programmer."))))

  (let ((code-to-update (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (kill-region (car bounds) (cdr bounds))
    (gptel-request code-to-update
      :system (or directive
                  (format "You are a programmer.
Update the function(s), method(s) or class in the prompt with the documentation according to the style of %s.
Use the following guidelines:
- Respect the original indentation style. Do not change any indetation or code.
- Only add documentation don't change code.
- Respond concisely ONLY the code itself, no explanation or summary,
- Do not surround it with markdown format."
                          (replace-regexp-in-string "\\(-ts\\)?-mode$" "" (symbol-name major-mode))))
      :buffer (current-buffer)
      :stream gptel-stream
      :callback
      (lambda (response info)
        (if (not response)
            (message "Response failed with: %s" (plist-get info :status))
          (let* ((buf (plist-get info :buffer)))
            (with-current-buffer buf
              (when (stringp response)
                (insert response)))))))))


(defun spacemacs//gptel-to-new-buffer (input directive)
  "Summarize STRING using GPT-el and display the summary in a dedicated buffer.

DIRECTIVE is a string providing specific instructions
for how the summary should be generated. If not provided, a
default directive is used that instructs GPT-el to summarize
the code concisely in the style of the current major mode."
  (let* ((buf (get-buffer-create "*GPTel Temp*")))
    (with-current-buffer buf
      (erase-buffer))
    (gptel-request input
      :system directive
      :buffer buf
      ;; no stream, to avoid cursor messed up
      ;; :stream gptel-stream
      :stream nil
      :callback (lambda (response info)
                  (when (stringp response)
                    (let ((buf (plist-get info :buffer)))
                      (with-current-buffer buf
                        (insert response))
                      (display-buffer buf)))))
    ;; Return the buffer for convenience
    buf))

(defun spacemacs/gptel-summarize-region (start end)
  "Summarize the region with gptel."
  (interactive "r")
  (spacemacs//gptel-to-new-buffer
   (buffer-substring-no-properties start end)
   "You are a concise summarizer. Below is a piece of text.

Provide a clear, well-structured summary that includes:
1. **Topic** – What the video is about (1–2 sentences).
2. **Key Points** – The main ideas, arguments, or steps covered.
3. **Takeaways** – Practical insights or conclusions worth remembering.

Keep it skimmable but complete enough to stand in for reading the text."
   ;; "Provide a bullet-point summary of the main arguments in under 50 words"
   ))


;; ---------------------------------------------------------------------------
;; URL → video ID

(defun spacemacs//yt-summarize--video-id (url)
  "Extract the YouTube video ID from URL, or signal an error."
  (cond
   ((string-match "youtube\\.com/watch\\?.*v=\\([A-Za-z0-9_-]\\{11\\}\\)" url)
    (match-string 1 url))
   ((string-match "youtu\\.be/\\([A-Za-z0-9_-]\\{11\\}\\)" url)
    (match-string 1 url))
   ((string-match "youtube\\.com/shorts/\\([A-Za-z0-9_-]\\{11\\}\\)" url)
    (match-string 1 url))
   ((string-match "youtube\\.com/embed/\\([A-Za-z0-9_-]\\{11\\}\\)" url)
    (match-string 1 url))
   (t (error "spacemacs//yt-summarize: cannot extract video ID from URL: %s" url))))

;; ---------------------------------------------------------------------------
;; Transcript via youtube-transcript-api CLI

(defun spacemacs//yt-summarize--fetch-transcript (video-id)
  "Fetch plain-text transcript for VIDEO-ID using the youtube-transcript-api CLI.
Calls: youtube_transcript_api <id> --languages <langs> --format json"
  (unless (executable-find "youtube_transcript_api")
    (error "spacemacs//yt-summarize: youtube_transcript_api not found — run: pip install youtube-transcript-api"))
  (message "spacemacs//yt-summarize: fetching transcript for %s …" video-id)
  (let* (;; Escape IDs starting with "-" per CLI docs
         (safe-id  (if (string-prefix-p "-" video-id)
                       (concat "\\-" (substring video-id 1))
                     video-id))
         (lang-args (mapcan (lambda (l) (list "--languages" l))
                            yt-summarize-languages))
         (args     (append (list safe-id) lang-args (list "--format" "text")))
         (output   (with-output-to-string
                     (apply #'call-process "youtube_transcript_api"
                            nil (list standard-output nil) nil args))))
    output))

(defun spacemacs/gptel-yt-summarize (url)
  "Summarize the youtube URL at point with gtel.
Or prompt for it."
  (interactive
   (let* ((url (let ((url (get-text-property (point) 'shr-url)))
                 (if url
                     url
                   (browse-url-url-at-point))))
          (prompt (if url
                      (format "Youtube link to summarize (default: %s): " url)
                    "Youtube link to summarize: ")))
     (list
      (read-string prompt nil nil url))))

  (spacemacs//gptel-to-new-buffer
   (spacemacs//yt-summarize--fetch-transcript (spacemacs//yt-summarize--video-id url))
   "You are a concise summarizer. Below is the transcript of a YouTube video.

Provide a clear, well-structured summary that includes:
1. **Topic** – What the video is about (1–2 sentences).
2. **Key Points** – The main ideas, arguments, or steps covered.
3. **Takeaways** – Practical insights or conclusions worth remembering.

Keep it skimmable but complete enough to stand in for watching the video."))
