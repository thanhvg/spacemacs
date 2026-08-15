;;; packages.el --- browser-controllers layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanh@gmail.com>
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

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;

;;; Code:

(defconst browser-controllers-packages
  '((browsel :location (recipe
                        :fetcher github
                        :repo "dmgerman/browsel"))))

(defun browser-controllers/init-browsel ()
  (use-package browsel
    :config
    (require 'browsel-www)         ; SAVE_PAGE
    (require 'browsel-chatgpt)     ; CHATGPT
    (require 'browsel-youtube)     ; YOUTUBE + YOUTUBE_TRANSCRIPT
    (require 'browsel-babel)       ; org-babel browsel-js blocks
    (require 'browsel-tab-manager) ; M-x browsel-tab-manager
    (require 'browsel-url-handler) ; browsel-browse-url + browsel-url-routes
    (browsel-start)))
