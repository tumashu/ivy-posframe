;;; ivy-posframe.el --- Using posframe to show Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/ivy-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching, ivy
;; Package-Requires: ((emacs "26.0")(posframe "0.1.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; * ivy-posframe README                                :README:

;;; Code:
;; * ivy-posframe's code
(require 'cl-lib)
(require 'posframe)

(defgroup ivy-posframe nil
  "Using posframe to show ivy"
  :group 'ivy
  :prefix "ivy-posframe")

(defcustom ivy-posframe-font nil
  "The font used by ivy-posframe's frame.
Using current frame's font if it it nil."
  :group 'ivy-posframe)

(defface ivy-posframe
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face used for the ivy-posframe."
  :group 'ivy-posframe)

(defvar ivy-posframe-buffer " *ivy-posframe-buffer*"
  "The buffer which used by ivy-posframe.")

(defun ivy-posframe-display (str)
  "Show ivy's posframe."
  (with-selected-window (ivy--get-window ivy-last)
    (posframe-show
     ivy-posframe-buffer
     :string (concat ivy--prompt ivy-text str)
     :poshandler #'posframe-poshandler-window-bottom-left-corner
     :background-color (face-attribute 'ivy-posframe :background)
     :foreground-color (face-attribute 'ivy-posframe :foreground)
     :height ivy-height
     :min-height 10
     :min-width 50)))

(defun ivy-posframe-cleanup ()
  "Clean ivy's posframe."
  (posframe-hide ivy-posframe-buffer))

;;;###autoload
(defun ivy-posframe-enable ()
  (interactive)
  (require 'ivy)
  (setq ivy-display-function #'ivy-posframe-display)
  (push '(t . ivy-posframe-display)
        ivy-display-functions-alist)
  (push '(ivy-posframe-display
          :cleanup ivy-posframe-cleanup)
        ivy-display-function-props)
  (message "Ivy-posframe is enabled, disable it need restart emacs."))



(provide 'ivy-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ivy-posframe.el ends here
