;;; funcs.el --- cemosv Layer functions File for Spacemacs -*- lexical-binding: t; -*-
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


(defun spacemacs//cemosv-selectrum-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-style-enable-hjkl-bindings))

    (dolist (map (list selectrum-minibuffer-map))
      (define-key map (kbd "C-j") 'selectrum-next-candidate)
      (define-key map (kbd "C-k") 'selectrum-previous-candidate)))
   (t
    (define-key selectrum-minibuffer-map (kbd "C-j") 'selectrum-next-candidate)
    (define-key selectrum-minibuffer-map (kbd "C-k") 'selectrum-previous-candidate))))


(defun spacemacs/cemovs-switch-to-buffer ()
  "`consult-buffer' with buffers provided by persp."
  (interactive)
  (with-persp-buffer-list ()
                          (consult-buffer)))

(defun spacemacs/cemovs-search (use-initial-input initial-directory)
  (interactive)
  (let* ((initial-input (if use-initial-input
                            (if (region-active-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t))
                          ""))
         (default-directory
           (or initial-directory (read-directory-name "Start from directory: "))))
    (consult-ripgrep default-directory initial-input)))

(defun spacemacs/consult-line ()
  (interactive)
  (consult-line
   (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))

(defun spacemacs/cemovs-search-auto ()
  "Choose folder to search."
  (interactive)
  (spacemacs/cemovs-search t nil))

(defun spacemacs/cemovs-search-dir ()
  "Search current folder."
  (interactive)
  (spacemacs/cemovs-search t default-directory))

(defun spacemacs/cemovs-search-projectile ()
  "Search in current project."
  (interactive)
  (spacemacs/cemovs-search t (projectile-project-root)))

(defun spacemacs/cemovs-search-default ()
  "Search."
  (interactive)
  (spacemacs/cemovs-search-projectile))

(defun spacemacs/cemovs-search-projectile-auto ()
  "Search in current project."
  (interactive)
  (spacemacs/cemovs-search nil (projectile-project-root)))


