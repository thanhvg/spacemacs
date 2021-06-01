;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq spacemacs-misc-packages
      '(devdocs-browser
        dumb-jump
        request))

(defun spacemacs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      ;; Use Helm or Ivy as the selector for dumb-jump.
      (cond
       ((configuration-layer/layer-used-p 'ivy)
        (setq dumb-jump-selector 'ivy))
       ((configuration-layer/layer-used-p 'helm)
        (setq dumb-jump-selector 'helm)))

      ;; Enable xref-backend of dumb-jump. It's chosen only when no better
      ;; options is available
      (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))))

(defun spacemacs-misc/init-request ()
  (setq request-storage-directory
        (concat spacemacs-cache-directory "request/")))

(defun spacemacs-misc/init-devdocs-browser ()
  (use-package devdocs-browser
    :defer t
    :init
    (spacemacs/set-leader-keys
      "hbb" #'devdocs-browser-open
      "hbs" #'devdocs-browser-open-in
      "hbu" #'devdocs-browser-update-docs
      "hbi" #'devdocs-browser-install-doc
      "hbr" #'devdocs-browser-uninstall-doc
      "hbU" #'devdocs-browser-upgrade-doc
      "hbo" #'devdocs-browser-download-offline-data
      "hbR" #'devdocs-browser-remove-offline-data))
    (spacemacs/set-leader-keys-for-minor-mode 'devdocs-browser-eww-mode
      "j" #'devdocs-browser-eww-goto-target
      "o" #'devdocs-browser-eww-open-in-default-browser))
