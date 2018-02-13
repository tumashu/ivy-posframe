;;; ivy-posframe.el --- Using posframe to show Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/ivy-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching, ivy
;; Package-Requires: ((emacs "26.0")(posframe "0.1.0")(ivy "0.10.0"))

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

;; ** How to enable ivy-posframe
;; 1. Global mode
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (setq ivy-display-function #'ivy-posframe-display)
;;    #+END_EXAMPLE
;; 2. Per-command mode.
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (push '(counsel-M-x . ivy-posframe-display) ivy-display-functions-alist)
;;    #+END_EXAMPLE
;; 3. Fallback mode
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (push '(t . ivy-posframe-display) ivy-display-functions-alist)
;;    #+END_EXAMPLE

;; ** How to set the style of ivy-posframe
;; 1. window-buttom-left style
;;    #+BEGIN_EXAMPLE
;;    (setq ivy-posframe-style 'window-buttom-left)
;;    #+END_EXAMPLE
;;    [[./snapshots/ivy-posframe1.gif]]
;; 2. Window-center style
;;    #+BEGIN_EXAMPLE
;;    (setq ivy-posframe-style 'window-center)
;;    #+END_EXAMPLE
;;    [[./snapshots/ivy-posframe2.gif]]
;; 3. Point style
;;    #+BEGIN_EXAMPLE
;;    (setq ivy-posframe-style 'point)
;;    #+END_EXAMPLE
;;    [[./snapshots/ivy-posframe3.gif]]


;;; Code:
;; * ivy-posframe's code
(require 'cl-lib)
(require 'posframe)
(require 'ivy)

(push '(ivy-posframe-display
        :cleanup ivy-posframe-cleanup)
      ivy-display-functions-props)

(defgroup ivy-posframe nil
  "Using posframe to show ivy"
  :group 'ivy
  :prefix "ivy-posframe")

(defcustom ivy-posframe-font nil
  "The font used by ivy-posframe.
When nil, Using current frame's font as fallback."
  :group 'ivy-posframe)

(defcustom ivy-posframe-style 'window-buttom-left
  "The style of ivy-posframe."
  :group 'ivy-posframe)

(defface ivy-posframe
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face used by the ivy-posframe."
  :group 'ivy-posframe)

(defvar ivy-posframe-buffer " *ivy-posframe-buffer*"
  "The posframe-buffer used by ivy-posframe.")

(defvar ivy-posframe-style-alist
  '((window-center . posframe-poshandler-window-center)
    (frame-center  . posframe-poshandler-frame-center)
    (window-buttom-left . posframe-poshandler-window-bottom-left-corner)
    (frame-buttom-left . posframe-poshandler-frame-bottom-left-corner)
    (point . posframe-poshandler-point-bottom-left-corner))
  "Alist of ivy posframe styles.")

(defun ivy-posframe-display (str)
  "Show STR in ivy's posframe."
  (if (not (ivy-posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-selected-window (ivy--get-window ivy-last)
      (posframe-show
       ivy-posframe-buffer
       :font ivy-posframe-font
       :string
       (with-current-buffer (get-buffer-create " *Minibuf-1*")
         (concat (buffer-string) "  " str))
       :position (point)
       :poshandler (cdr (assq ivy-posframe-style
                              ivy-posframe-style-alist))
       :background-color (face-attribute 'ivy-posframe :background)
       :foreground-color (face-attribute 'ivy-posframe :foreground)
       :height ivy-height
       :min-height 10
       :min-width 50))))

(defun ivy-posframe-cleanup ()
  "Cleanup ivy's posframe."
  (when (ivy-posframe-workable-p)
    (posframe-hide ivy-posframe-buffer)))

(defun ivy-posframe-workable-p ()
  "Test ivy-posframe workable status."
  (and (>= emacs-major-version 26)
       (not (or noninteractive
                emacs-basic-display
                (not (display-graphic-p))))))

(provide 'ivy-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ivy-posframe.el ends here
