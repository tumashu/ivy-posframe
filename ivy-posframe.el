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
;; ** What is ivy-posframe
;; ivy-posframe is a ivy extension, which let ivy use posframe
;; to show its candidate menu.

;; [[./snapshots/ivy-posframe1.gif]]

;; [[./snapshots/ivy-posframe2.gif]]

;; ** How to use ivy-posframe

;; #+BEGIN_EXAMPLE
;; (require 'ivy-posframe)
;; (ivy-posframe-mode 1)
;; #+END_EXAMPLE

;;; Code:
;; * ivy-posframe's code
(require 'cl-lib)
(require 'posframe)
(require 'ivy)

(push '(ivy-posframe-display
        :check ivy-posframe-workable-p
        :cleanup ivy-posframe-cleanup)
      ivy-display-functions-props)

(defgroup ivy-posframe nil
  "Using posframe to show ivy"
  :group 'ivy
  :prefix "ivy-posframe")

(defcustom ivy-posframe-font nil
  "The font used by ivy-posframe.
Using current frame's font if it it nil."
  :group 'ivy-posframe)

(defcustom ivy-posframe-poshandler nil
  "The posframe poshandler used by ivy-posframe.
When nil, use `posframe-poshandler-window-bottom-left-corner'
as fallback.

More details can be found in docstring of `posframe-show'."
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
     :font ivy-posframe-font
     :string (concat ivy--prompt ivy-text str)
     :position (point)
     :poshandler (or ivy-posframe-poshandler
                     #'posframe-poshandler-window-bottom-left-corner)
     :background-color (face-attribute 'ivy-posframe :background)
     :foreground-color (face-attribute 'ivy-posframe :foreground)
     :height ivy-height
     :min-height 10
     :min-width 50)))

(defun ivy-posframe-cleanup ()
  "Clean ivy's posframe."
  (posframe-hide ivy-posframe-buffer))

(defun ivy-posframe-workable-p ()
  "Test ivy-posframe workable or not."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

;;;###autoload
(define-minor-mode ivy-posframe-mode
  "ivy-posframe minor mode."
  :global t
  :require 'ivy-posframe
  :group 'ivy-posframe
  :lighter " ivy-posframe"
  (if ivy-posframe-mode
      (setq ivy-display-function #'ivy-posframe-display)
    (setq ivy-display-function nil)))

(provide 'ivy-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ivy-posframe.el ends here
