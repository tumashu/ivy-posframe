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

;; NOTE: ivy-posframe requires Emacs 26

;; ** Display functions

;; 1. ivy-posframe-display
;; 2. ivy-posframe-display-at-frame-center
;; 3. ivy-posframe-display-at-window-center
;; 4. ivy-posframe-display-at-frame-buttom-left
;; 5. ivy-posframe-display-at-window-buttom-left
;; 6. ivy-posframe-display-at-point

;; ** How to enable ivy-posframe
;; 1. Global mode
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (setq ivy-display-function #'ivy-posframe-display)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-buttom-left)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-window-buttom-left)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;;    #+END_EXAMPLE
;; 2. Per-command mode.
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    ;; Different command can use different display function.
;;    (push '(counsel-M-x . ivy-posframe-display-at-window-buttom-left) ivy-display-functions-alist)
;;    (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
;;    #+END_EXAMPLE
;; 3. Fallback mode
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (push '(t . ivy-posframe-display) ivy-display-functions-alist)
;;    #+END_EXAMPLE

;; If you use `ivy-posframe-display', you can use `ivy-posframe-style'
;; to set show style.

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

(dolist (f '(ivy-posframe-display
             ivy-posframe-display-at-frame-center
             ivy-posframe-display-at-window-center
             ivy-posframe-display-at-frame-buttom-left
             ivy-posframe-display-at-window-buttom-left
             ivy-posframe-display-at-point))
  (push `(,f :cleanup ivy-posframe-cleanup)
        ivy-display-functions-props))

(defgroup ivy-posframe nil
  "Using posframe to show ivy"
  :group 'ivy
  :prefix "ivy-posframe")

(defcustom ivy-posframe-font nil
  "The font used by ivy-posframe.
When nil, Using current frame's font as fallback."
  :group 'ivy-posframe
  :type 'string)

(defcustom ivy-posframe-style 'window-buttom-left
  "The style of ivy-posframe."
  :group 'ivy-posframe
  :type 'string)

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

;; Fix warn
(defvar emacs-basic-display)

(defun ivy-posframe-display (str &optional style)
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
       :poshandler (cdr (assq (or style ivy-posframe-style)
                              ivy-posframe-style-alist))
       :background-color (face-attribute 'ivy-posframe :background)
       :foreground-color (face-attribute 'ivy-posframe :foreground)
       :height ivy-height
       :min-height 10
       :min-width 50))))

(defun ivy-posframe-display-at-window-center (str)
  (ivy-posframe-display str 'window-center))

(defun ivy-posframe-display-at-frame-center (str)
  (ivy-posframe-display str 'frame-center))

(defun ivy-posframe-display-at-window-buttom-left (str)
  (ivy-posframe-display str 'window-buttom-left))

(defun ivy-posframe-display-at-frame-buttom-left (str)
  (ivy-posframe-display str 'frame-buttom-left))

(defun ivy-posframe-display-at-point (str)
  (ivy-posframe-display str 'point))

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
