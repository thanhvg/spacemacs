;;; packages.el --- node layer packages file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Juan Placencia <juan.placencia.512@gmail.com>
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


(setq node-packages
      '((add-node-modules-path :toggle node-add-modules-path)
        js-doc
        nodejs-repl
        npm-mode))

(defun node/init-add-node-modules-path ()
  (use-package add-node-modules-path :defer t))

(defun node/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
    :init
    (spacemacs/register-repl 'nodejs-repl
                             'nodejs-repl
                             "nodejs-repl")))

(defun node/init-js-doc ()
  (use-package js-doc
    :defer t))
    
(defun node/init-npm-mode ()
  (use-package npm-mode
    :defer t
    :config
    (spacemacs/set-leader-keys-for-minor-mode 'npm-mode
      "n" "npm"
      "ni" 'npm-mode-npm-install
      "nr" 'npm-mode-npm-run
      "ns" 'npm-mode-npm-install-save
      "nd" 'npm-mode-npm-install-save-dev
      "nn" 'npm-mode-npm-init
      "nu" 'npm-mode-npm-uninstall
      "nl" 'npm-mode-npm-list
      "np" 'npm-mode-visit-project-file)))
