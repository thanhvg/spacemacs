;; -*- lexical-binding: t; -*-
(defvar llm-client--gptel-send-called nil
  "Flag to track if gptel-send has been called at least once.")

(defcustom llm-client-enable-gptel t
  "If non-nil, enable the =gptel= package."
  :type 'boolean
  :group 'llm-client)

(defcustom llm-client-enable-gptel-agent t
  "If non-nil, enable the =gptel-agent= package for agentic capabilities in =gptel=."
  :type 'boolean
  :group 'llm-client)

(defcustom llm-client-enable-ellama t
  "If non-nil, enable the =ellama= package."
  :type 'boolean
  :group 'llm-client)


(defcustom yt-summarize-languages '("en")
  "Preferred transcript languages, tried in order."
  :type '(repeat string)
  :group 'llm-client)
