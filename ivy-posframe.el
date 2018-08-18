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
;;    [[./snapshots/ivy-posframe-display-at-window-center.gif]]
;; 4. ivy-posframe-display-at-frame-bottom-left
;; 5. ivy-posframe-display-at-window-bottom-left
;;    [[./snapshots/ivy-posframe-display-at-window-bottom-left.gif]]
;; 6. ivy-posframe-display-at-frame-bottom-window-center
;; 7. ivy-posframe-display-at-point
;;    [[./snapshots/ivy-posframe-display-at-point.gif]]

;; ** How to enable ivy-posframe
;; 1. Global mode
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (setq ivy-display-function #'ivy-posframe-display)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;;    ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;;    (ivy-posframe-enable)
;;    #+END_EXAMPLE
;; 2. Per-command mode.
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    ;; Different command can use different display function.
;;    (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
;;    (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
;;    (ivy-posframe-enable)
;;    #+END_EXAMPLE
;; 3. Fallback mode
;;    #+BEGIN_EXAMPLE
;;    (require 'ivy-posframe)
;;    (push '(t . ivy-posframe-display) ivy-display-functions-alist)
;;    (ivy-posframe-enable)
;;    #+END_EXAMPLE

;; ** Tips

;; *** How to show fringe to ivy-posframe
;; ;; #+BEGIN_EXAMPLE
;; (setq ivy-posframe-parameters
;;       '((left-fringe . 10)
;;         (right-fringe . 10)))
;; ;; #+END_EXAMPLE

;; By the way, User can set *any* parameters of ivy-posframe with
;; the help of `ivy-posframe-parameters'.

;; *** How to custom your ivy-posframe style

;; The simplest way is:
;; ;; #+BEGIN_EXAMPLE
;; (defun ivy-posframe-display-at-XXX (str)
;;   (ivy-posframe--display str #'your-own-poshandler-function))
;; (ivy-posframe-enable) ; This line is needed.
;; ;; #+END_EXAMPLE

;;; Code:
;; * ivy-posframe's code
(require 'cl-lib)
(require 'posframe)
(require 'ivy)

(defgroup ivy-posframe nil
  "Using posframe to show ivy"
  :group 'ivy
  :prefix "ivy-posframe")

(defcustom ivy-posframe-style 'window-bottom-left
  "The style of ivy-posframe."
  :group 'ivy-posframe
  :type 'string)

(defcustom ivy-posframe-font nil
  "The font used by ivy-posframe.
When nil, Using current frame's font as fallback."
  :group 'ivy-posframe
  :type 'string)
(defcustom ivy-posframe-width nil
  "The width of ivy-posframe."
  :group 'ivy-posframe
  :type 'number)

(defcustom ivy-posframe-height nil
  "The height of ivy-posframe."
  :group 'ivy-posframe
  :type 'number)

(defcustom ivy-posframe-min-width nil
  "The width of ivy-min-posframe."
  :group 'ivy-posframe
  :type 'number)

(defcustom ivy-posframe-min-height nil
  "The height of ivy-min-posframe."
  :group 'ivy-posframe
  :type 'number)

(defcustom ivy-posframe-border-width 0
  "The border width used by ivy-posframe.
When 0, no border is showed."
  :group 'ivy-posframe
  :type 'number)

(defcustom ivy-posframe-parameters nil
  "The frame parameters used by ivy-posframe."
  :group 'ivy-posframe
  :type 'string)

(defface ivy-posframe
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face used by the ivy-posframe."
  :group 'ivy-posframe)

(defface ivy-posframe-cursor
  '((t (:inherit cursor)))
  "Face used by the ivy-posframe's fake cursor."
  :group 'ivy-posframe)

(defvar ivy-posframe-buffer " *ivy-posframe-buffer*"
  "The posframe-buffer used by ivy-posframe.")

(defvar ivy-posframe--ignore-prompt nil
  "When non-nil, ivy-posframe will ignore prompt.
This variable is useful for `ivy-posframe-read-action' .")

;; Fix warn
(defvar emacs-basic-display)

(defun ivy-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe."
  (if (not (ivy-posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-selected-window (ivy--get-window ivy-last)
      (posframe-show
       ivy-posframe-buffer
       :font ivy-posframe-font
       :string
       (with-current-buffer (get-buffer-create " *Minibuf-1*")
         (let ((point (point))
               (string (if ivy-posframe--ignore-prompt
                           str
                         (concat (buffer-string) "  " str))))
           (add-text-properties (- point 1) point '(face ivy-posframe-cursor) string)
           string))
       :position (point)
       :poshandler poshandler
       :background-color (face-attribute 'ivy-posframe :background)
       :foreground-color (face-attribute 'ivy-posframe :foreground)
       :height (or ivy-posframe-height ivy-height)
       :width (or ivy-posframe-width (/ (window-width) 2))
       :min-height (or ivy-posframe-min-height 10)
       :min-width (or ivy-posframe-min-width 50)
       :internal-border-width ivy-posframe-border-width
       :override-parameters ivy-posframe-parameters))))

(defun ivy-posframe-display (str)
  (let ((func (intern (format "ivy-posframe-display-at-%s"
                              ivy-posframe-style))))
    (if (functionp func)
        (funcall func str)
      (ivy-posframe-display-at-frame-bottom-left str))))

(defun ivy-posframe-display-at-window-center (str)
  (ivy-posframe--display str #'posframe-poshandler-window-center))

(defun ivy-posframe-display-at-frame-center (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-center))

(defun ivy-posframe-display-at-window-bottom-left (str)
  (ivy-posframe--display str #'posframe-poshandler-window-bottom-left-corner))

(defun ivy-posframe-display-at-frame-bottom-left (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-bottom-left-corner))

(defun ivy-posframe-display-at-frame-bottom-window-center (str)
  (ivy-posframe--display
   str #'(lambda (info)
           (cons (car (posframe-poshandler-window-center info))
                 (cdr (posframe-poshandler-frame-bottom-left-corner info))))))

(defun ivy-posframe-display-at-point (str)
  (ivy-posframe--display str #'posframe-poshandler-point-bottom-left-corner))

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

(defun ivy-posframe-dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (when (ivy-posframe-read-action)
    (ivy-done)))

(defun ivy-posframe-read-action ()
  "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise."
  (interactive)
  (let* ((actions (ivy-state-action ivy-last))
         (caller (ivy-state-caller ivy-last))
         (display-function
          (or ivy-display-function
              (cdr (or (assq caller ivy-display-functions-alist)
                       (assq t ivy-display-functions-alist))))))
    (if (not (ivy--actionp actions))
        t
      (let* ((hint (funcall ivy-read-action-format-function (cdr actions)))
             (resize-mini-windows t)
             (key "")
             action-idx)
        (while (and (setq action-idx (cl-position-if
                                      (lambda (x)
                                        (string-prefix-p key (car x)))
                                      (cdr actions)))
                    (not (string= key (car (nth action-idx (cdr actions))))))
          (setq key (concat key (string
                                 (read-key
                                  (if (functionp display-function)
                                      (let ((ivy-posframe--ignore-prompt t))
                                        (funcall display-function hint)
                                        "Please type a key: ")
                                    hint))))))
        (cond ((member key '("" ""))
               nil)
              ((null action-idx)
               (message "%s is not bound" key)
               nil)
              (t
               (message "")
               (setcar actions (1+ action-idx))
               (ivy-set-action actions)))))))

(defun ivy-posframe-avy ()
  "Jump to one of the current ivy candidates."
  (interactive)
  (message "ivy-posframe: ivy-avy is not supported at the moment."))

;;;###autoload
(defun ivy-posframe-enable ()
  "Enable ivy-posframe."
  (interactive)
  (require 'ivy)
  (ivy-posframe-setup)
  (define-key ivy-minibuffer-map (kbd "C-M-a") 'ivy-posframe-read-action)
  (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-posframe-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "C-'") 'ivy-posframe-avy)
  (message "ivy-posframe is enabled."))

(defun ivy-posframe-setup ()
  "Add all display functions of ivy-posframe to
`ivy-display-functions-props'."
  (mapatoms
   #'(lambda (func)
       (when (and (functionp func)
                  (string-match-p "^ivy-posframe-display" (symbol-name func))
                  (not (assq func ivy-display-functions-props)))
         (push `(,func :cleanup ivy-posframe-cleanup)
               ivy-display-functions-props)))))

(ivy-posframe-setup)

(provide 'ivy-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ivy-posframe.el ends here
