;;; ivy-posframe.el --- Using posframe to show Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;;         Naoya Yamashita <conao3@gmail.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/ivy-posframe
;; Version: 0.6.3
;; Keywords: abbrev, convenience, matching, ivy
;; Package-Requires: ((emacs "26.0") (posframe "1.0.0") (ivy "0.13.0"))

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

;; ivy-posframe is an ivy extension, which lets ivy use posframe to show
;; its candidate menu.

;; NOTE: ivy-posframe requires Emacs 26 and do not support mouse
;; click.

;; ** Display functions

;; 1. ivy-posframe-display
;; 2. ivy-posframe-display-at-frame-center
;; 3. ivy-posframe-display-at-window-center
;;    [[./snapshots/ivy-posframe-display-at-window-center.png]]
;; 4. ivy-posframe-display-at-frame-bottom-left
;; 5. ivy-posframe-display-at-window-bottom-left
;;    [[./snapshots/ivy-posframe-display-at-window-bottom-left.png]]
;; 6. ivy-posframe-display-at-frame-bottom-window-center
;; 7. ivy-posframe-display-at-point
;;    [[./snapshots/ivy-posframe-display-at-point.png]]

;; ** How to enable ivy-posframe
;; *** Global mode
;; #+BEGIN_EXAMPLE
;; (require 'ivy-posframe)
;; ;; display at `ivy-posframe-style'
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;; (ivy-posframe-mode 1)
;; #+END_EXAMPLE
;; *** Per-command mode.
;; #+BEGIN_EXAMPLE
;; (require 'ivy-posframe)
;; ;; Different command can use different display function.
;; (setq ivy-posframe-display-functions-alist
;;       '((swiper          . ivy-posframe-display-at-point)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display)))
;; (ivy-posframe-mode 1)
;; #+END_EXAMPLE
;;
;; You can use ivy original display function on specify function.
;; You may want to use the original display function because display
;; of Swiper at point hides the contents of the buffer.
;; #+BEGIN_EXAMPLE
;; (require 'ivy-posframe)
;; ;; Different command can use different display function.
;; (setq ivy-posframe-display-functions-alist
;;       '((swiper          . ivy-display-function-fallback)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display)))
;; (ivy-posframe-mode 1)
;; #+END_EXAMPLE
;;
;; You may want to change the height of ivy by a function only while
;; using posframe. This is possible with the code below.
;;
;; The following example displays swiper on 20 lines by default for ivy,
;; and displays other functions in posframe at the location specified on
;; 40 lines.
;; #+BEGIN_EXAMPLE
;; (require 'ivy-posframe)
;; ;; Different command can use different display function.
;; (setq ivy-posframe-height-alist '((swiper . 20)
;;                                   (t      . 40)))
;;
;; (setq ivy-posframe-display-functions-alist
;;       '((swiper          . ivy-display-function-fallback)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display)))
;; (ivy-posframe-mode 1)
;; #+END_EXAMPLE
;;
;; NOTE: Using swiper as example: swiper's display function *only*
;; take effect when you call swiper command with global keybinding, if
;; you call swiper command with 'M-x' (for example: counsel-M-x),
;; counsel-M-x's display function will take effect instead of
;; swiper's.

;; The value of variable `this-command' will be used as the search key
;; by ivy to find display function in `ivy-display-functions-alist',
;; "C-h v this-command" is a good idea.

;; ** Tips

;; *** How to show fringe to ivy-posframe
;; #+BEGIN_EXAMPLE
;; (setq ivy-posframe-parameters
;;       '((left-fringe . 8)
;;         (right-fringe . 8)))
;; #+END_EXAMPLE

;; By the way, User can set *any* parameters of ivy-posframe with
;; the help of `ivy-posframe-parameters'.

;; *** How to custom your ivy-posframe style

;; The simplest way is:
;; #+BEGIN_EXAMPLE
;; (defun ivy-posframe-display-at-XXX (str)
;;   (ivy-posframe--display str #'your-own-poshandler-function))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-XXX)))
;; (ivy-posframe-mode 1) ; This line is needed.
;; #+END_EXAMPLE

;;; Code:
;; * ivy-posframe's code
(require 'cl-lib)
(require 'posframe)
(require 'ivy)

(defgroup ivy-posframe nil
  "Using posframe to show ivy"
  :group 'ivy
  :prefix "ivy-posframe")

(defcustom ivy-posframe-style 'frame-center
  "The style of ivy-posframe."
  :type 'string)

(defcustom ivy-posframe-font nil
  "The font used by ivy-posframe.
When nil, Using current frame's font as fallback."
  :type 'string)

(defcustom ivy-posframe-width nil
  "The width of ivy-posframe."
  :type 'number)

(defcustom ivy-posframe-height nil
  "The height of ivy-posframe."
  :type 'number)

(defcustom ivy-posframe-min-width nil
  "The width of ivy-min-posframe."
  :type 'number)

(defcustom ivy-posframe-min-height nil
  "The height of ivy-min-posframe."
  :type 'number)

(defcustom ivy-posframe-refposhandler #'ivy-posframe-refposhandler-default
  "The refposhandler use by ivy-posframe.

NOTE: This variable is very useful to EXWM users."
  :type 'function)

(defcustom ivy-posframe-size-function #'ivy-posframe-get-size
  "The function which is used to deal with posframe's size."
  :type 'function)

(defcustom ivy-posframe-border-width 1
  "The border width used by ivy-posframe.
When 0, no border is showed."
  :type 'number)

(defcustom ivy-posframe-hide-minibuffer t
  "Hide input of minibuffer when using ivy-posframe."
  :type 'boolean)

(defcustom ivy-posframe-parameters nil
  "The frame parameters used by ivy-posframe."
  :type 'string)

(defcustom ivy-posframe-height-alist nil
  "The `ivy-height-alist' while working ivy-posframe."
  :type 'sexp)

(defcustom ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
  "The `ivy-display-functions-alist' while working ivy-posframe."
  :type 'sexp)

(defcustom ivy-posframe-lighter " ivy-posframe"
  "The lighter string used by `ivy-posframe-mode'."
  :type 'string)

(defface ivy-posframe
  '((t (:inherit default)))
  "Face used by the ivy-posframe."
  :group 'ivy-posframe)

(defface ivy-posframe-border
  '((t (:inherit default :background "gray50")))
  "Face used by the ivy-posframe's border."
  :group 'ivy-posframe)

(defface ivy-posframe-cursor
  '((t (:inherit cursor)))
  "Face used by the ivy-posframe's fake cursor."
  :group 'ivy-posframe)

(defun ivy-posframe-buffer-setter (sym val)
  "Set SYM as VAL and create buffer named `ivy-posframe-buffer'."
  (set-default sym val)
  (get-buffer-create val))

(defcustom ivy-posframe-buffer " *ivy-posframe-buffer*"
  "The posframe-buffer used by ivy-posframe."
  :set #'ivy-posframe-buffer-setter
  :type 'string)

(defvar ivy-posframe--ignore-prompt nil
  "When non-nil, ivy-posframe will ignore prompt.
This variable is useful for `ivy-posframe-read-action' .")

;; Fix warn
(defvar emacs-basic-display)
(defvar ivy--display-function)

(defvar exwm--connection)
(defvar exwm-workspace--workareas)
(defvar exwm-workspace-current-index)

(defun ivy-posframe-refposhandler-default (&optional frame)
  "The default posframe refposhandler used by ivy-posframe."
  (cond
   ;; EXWM environment
   ((bound-and-true-p exwm--connection)
    (or (ignore-errors
          (let ((info (elt exwm-workspace--workareas
                           exwm-workspace-current-index)))
            (cons (elt info 0)
                  (elt info 1))))
        ;; Need user install xwininfo.
        (ignore-errors
          (posframe-refposhandler-xwininfo frame))
        ;; Fallback, this value will incorrect sometime, for example: user
        ;; have panel.
        (cons 0 0)))
   (t nil)))

(defun ivy-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-ivy-window
     (apply #'posframe-show
            ivy-posframe-buffer
            :font ivy-posframe-font
            :string str
            :position (point)
            :poshandler poshandler
            :background-color (face-attribute 'ivy-posframe :background nil t)
            :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
            :border-width ivy-posframe-border-width
            :border-color (face-attribute 'ivy-posframe-border :background nil t)
            :override-parameters ivy-posframe-parameters
            :refposhandler ivy-posframe-refposhandler
            :hidehandler #'ivy-posframe-hidehandler
            (funcall ivy-posframe-size-function))
     (ivy-posframe--add-prompt 'ignore)))
  (with-current-buffer ivy-posframe-buffer
    (setq-local truncate-lines ivy-truncate-lines)))

(defun ivy-posframe-hidehandler (_)
  "Hidehandler used by ivy-posframe."
  (and (not (minibufferp))
       ;; Note: when run ivy-avy, buffer will be temp changed, make
       ;; sure do not autohide posframe at this situation.
       ;; More detail: https://github.com/tumashu/ivy-posframe/issues/114
       (not (equal (current-buffer) (window-buffer (ivy-posframe--window))))))

(defun ivy-posframe-get-size ()
  "The default functon used by `ivy-posframe-size-function'."
  (list
   :height ivy-posframe-height
   :width ivy-posframe-width
   :min-height (or ivy-posframe-min-height
                   (let ((height (+ ivy-height 1)))
                     (min height (or ivy-posframe-height height))))
   :min-width (or ivy-posframe-min-width
                  (let ((width (round (* (frame-width) 0.62))))
                    (min width (or ivy-posframe-width width))))))

(defun ivy-posframe-display (str)
  "Display STR via `posframe' by `ivy-posframe-style'."
  (let ((func (intern (format "ivy-posframe-display-at-%s" ivy-posframe-style))))
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
   str (lambda (info)
         (cons (car (posframe-poshandler-window-center info))
               (cdr (posframe-poshandler-frame-bottom-left-corner info))))))

(defun ivy-posframe-display-at-point (str)
  (ivy-posframe--display str #'posframe-poshandler-point-bottom-left-corner))

(defun ivy-posframe-display-at-frame-top-center (str)
  (ivy-posframe--display str #'posframe-poshandler-frame-top-center))

(defun ivy-posframe-cleanup ()
  "Cleanup ivy's posframe."
  (when (posframe-workable-p)
    (posframe-hide ivy-posframe-buffer)))

(defvar avy-all-windows)
(defvar avy-keys)
(defvar avy-style)
(defvar avy-pre-action)
(defvar swiper-faces)
(defvar swiper-background-faces)
(defvar swiper-min-highlight)

(declare-function avy--make-backgrounds "avy")
(declare-function avy-window-list "avy")
(declare-function avy-read-de-bruijn "avy")
(declare-function avy-read "avy")
(declare-function avy-tree "avy")
(declare-function avy--overlay-post "avy")
(declare-function avy--remove-leading-chars "avy")
(declare-function avy-push-mark "avy")
(declare-function avy--done "avy")
(declare-function avy-action-goto "avy")
(declare-function avy-candidate-beg "avy")
(declare-function ivy-avy "avy")
(declare-function swiper--avy-candidate "swiper")
(declare-function swiper-avy "swiper")
(declare-function swiper--update-input-ivy "swiper")

(defun ivy-posframe--dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let ((ivy-exit 'ivy-posframe--dispatching-done))
    (when (ivy-read-action)
      (ivy-done)))
  (ivy-posframe-shrink-after-dispatching))

(defun ivy-posframe-dispatching-done ()
  "Ivy-posframe's `ivy-dispatching-done'."
  (interactive)
  (let ((ivy-read-action-function #'ivy-posframe-read-action-by-key))
    (ivy-posframe--dispatching-done)))

(defun ivy-posframe--dispatching-call ()
  "Select one of the available actions and call `ivy-call'."
  (interactive)
  (setq ivy-current-prefix-arg current-prefix-arg)
  (let ((actions (copy-sequence (ivy-state-action ivy-last)))
        (old-ivy-text ivy-text))
    (unwind-protect
        (when (ivy-read-action)
          (ivy-set-text old-ivy-text)
          (ivy-call))
      (ivy-set-action actions)))
  (ivy-posframe-shrink-after-dispatching))

(defun ivy-posframe-dispatching-call ()
  "Ivy-posframe's `ivy-dispatching-call'."
  (interactive)
  (let ((ivy-read-action-function #'ivy-posframe-read-action-by-key))
    (ivy-posframe--dispatching-call)))

(defun ivy-posframe-read-action ()
  "Ivy-posframe version `ivy-read-action'"
  (interactive)
  (let ((ivy-read-action-function #'ivy-posframe-read-action-by-key))
    (call-interactively #'ivy-read-action)))

(defun ivy-posframe-read-action-by-key (actions)
  "Ivy-posframe's `ivy-read-action-by-key'."
  (let* ((set-message-function nil)
         (caller (ivy-state-caller ivy-last))
         (display-function
          (or ivy--display-function
              (cdr (or (assq caller ivy-display-functions-alist)
                       (assq t ivy-display-functions-alist)))))
         (hint (funcall ivy-read-action-format-function (cdr actions)))
         (resize-mini-windows t)
         (key "")
         action-idx)
    (while (and (setq action-idx (cl-position-if
                                  (lambda (x)
                                    (string-prefix-p key (car x)))
                                  (cdr actions)))
                (not (string= key (car (nth action-idx (cdr actions))))))
      (setq key (concat key (key-description
                             (vector
                              (read-key
                               (if (functionp display-function)
                                   (let ((ivy-posframe--ignore-prompt t))
                                     (funcall display-function hint)
                                     "Please type a key: ")
                                 hint)))))))
    (ivy-posframe-shrink-after-dispatching)
    (cond ((member key '("ESC" "C-g" "M-o"))
           nil)
          ((null action-idx)
           (message "%s is not bound" key)
           nil)
          (t
           (message "")
           (setcar actions (1+ action-idx))
           (ivy-set-action actions)))))

(defun ivy-posframe-shrink-after-dispatching ()
  "Shrink the minibuffer to the minimum size after dispatching."
  (when (window-minibuffer-p)
    (window-resize nil (- (window-size)))))

(defun ivy-posframe--window ()
  "Return the posframe window displaying `ivy-posframe-buffer'."
  (frame-selected-window
   (buffer-local-value 'posframe--frame
                       (get-buffer ivy-posframe-buffer))))

(defun ivy-posframe-avy ()
  "Ivy-posframe's `ivy-avy'."
  (interactive)
  (let ((avy-pre-action #'ignore))
    (with-selected-window (ivy-posframe--window)
      (ivy-avy))))

(defun ivy-posframe--swiper-avy-candidates ()
  "Ivy-posframe's `swiper-avy-candidates'."
  (let* (
         ;; We'll have overlapping overlays, so we sort all the
         ;; overlays in the visible region by their start, and then
         ;; throw out non-Swiper overlays or overlapping Swiper
         ;; overlays.
         (visible-overlays (cl-sort (with-ivy-window
                                      (overlays-in (window-start)
                                                   (window-end)))
                                    #'< :key #'overlay-start))
         (min-overlay-start 0)
         (overlays-for-avy
          (cl-remove-if-not
           (lambda (ov)
             (when (and (>= (overlay-start ov)
                            min-overlay-start)
                        (memq (overlay-get ov 'face)
                              (append swiper-faces swiper-background-faces)))
               (setq min-overlay-start (overlay-start ov))))
           visible-overlays))
         (offset (if (eq (ivy-state-caller ivy-last) 'swiper) 1 0)))
    (nconc
     (mapcar (lambda (ov)
               (cons (overlay-start ov)
                     (overlay-get ov 'window)))
             overlays-for-avy)
     ;; NOTE: This line should be the *only* difference from
     ;; `swiper-avy-candidates'.
     (with-current-buffer ivy-posframe-buffer
       (save-excursion
         (save-restriction
           (narrow-to-region (window-start) (window-end))
           (goto-char (point-min))
           (forward-line)
           (let ((win (selected-window))
                 cands)
             (while (not (eobp))
               (push (cons (+ (point) offset) win)
                     cands)
               (forward-line))
             cands)))))))

(defun ivy-posframe--swiper-avy-candidate ()
  "Ivy-posframe's `swiper--avy-candidate'."
  (let ((candidates (ivy-posframe--swiper-avy-candidates))
        (avy-all-windows nil))
    (unwind-protect
        (prog2
            (avy--make-backgrounds
             (append (avy-window-list)
                     (list (ivy-state-window ivy-last))))
            (if (eq avy-style 'de-bruijn)
                (avy-read-de-bruijn candidates avy-keys)
              (avy-read (avy-tree candidates avy-keys)
                        #'avy--overlay-post
                        #'avy--remove-leading-chars))
          (avy-push-mark))
      (avy--done))))

(defun ivy-posframe--swiper-avy-goto (candidate)
  "Ivy-posframe's `swiper--avy-goto'."
  (cond ((eq (cdr-safe candidate)
             (ivy-posframe--window))
         (let ((cand-text (with-current-buffer ivy-posframe-buffer
                            (save-excursion
                              (goto-char (car candidate))
                              (buffer-substring
                               (line-beginning-position)
                               (line-end-position))))))
           (ivy-set-index
            ;; cand-text may include "> ", using a hack way
            ;; to deal with it.
            (or (cl-some (lambda (n)
                           (cl-position (substring cand-text n)
                                        ivy--old-cands :test #'string=))
                         '(0 1 2 3 4))
                0))
           (ivy--exhibit)
           (ivy-done)
           (ivy-call)))
        ((or (consp candidate)
             (number-or-marker-p candidate))
         (ivy-quit-and-run
           (avy-action-goto (avy-candidate-beg candidate))))))

(defun ivy-posframe-swiper-avy ()
  "Ivy-posframe's `swiper-avy'."
  (interactive)
  (if (not (string-match-p "^ivy-posframe-display"
                           (or (ignore-errors
                                 (symbol-name ivy--display-function))
                               "")))
      ;; if swiper is not use ivy-posframe's display function.
      ;; call `swiper-avy'.

      ;; FIXME: This assume all ivy-posframe display functions are
      ;; prefixed with ivy-posframe-display.
      (swiper-avy)
    (unless (require 'avy nil 'noerror)
      (error "Package avy isn't installed"))
    (cl-case (length ivy-text)
      (0
       (user-error "Need at least one char of input"))
      (1
       (let ((swiper-min-highlight 1))
         (swiper--update-input-ivy))))
    (unless (string= ivy-text "")
      (ivy-posframe--swiper-avy-goto
       (ivy-posframe--swiper-avy-candidate)))))

;;; Variables

(defvar ivy-posframe-advice-alist
  '((ivy--minibuffer-setup      . ivy-posframe--minibuffer-setup)
    (ivy--display-function-prop . ivy-posframe--display-function-prop)
    (ivy--height                . ivy-posframe--height)
    (ivy-read                   . ivy-posframe--read)))

;;; Advice

(defun ivy-posframe--minibuffer-setup (fn &rest args)
  "Advice function of FN, `ivy--minibuffer-setup' with ARGS."
  (if (not (display-graphic-p))
      (apply fn args)
    (let ((ivy-fixed-height-minibuffer nil))
      (apply fn args))
    (when (and ivy-posframe-hide-minibuffer
               (posframe-workable-p)
               ;; if display-function is not a ivy-posframe style display-function.
               ;; do not hide minibuffer.
               ;; The hypothesis is that all ivy-posframe style display functions
               ;; have ivy-posframe as name prefix, need improve!
               (string-match-p "^ivy-posframe" (symbol-name ivy--display-function)))
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'ivy-posframe t)
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `( :background ,bg-color :foreground ,bg-color
                          :box nil :underline nil
                          :overline nil :strike-through nil)))
        (setq-local cursor-type nil)))))

(defun ivy-posframe--add-prompt (fn &rest args)
  "Add the ivy prompt to the posframe.  Advice FN with ARGS."
  (apply fn args)
  (when (and (display-graphic-p)
             (not ivy-posframe--ignore-prompt))
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (let ((point (point))
            (prompt (buffer-string)))
        (remove-text-properties 0 (length prompt) '(read-only nil) prompt)
        (with-current-buffer ivy-posframe-buffer
          (goto-char (point-min))
          (delete-region (point) (line-beginning-position 2))
          (insert prompt "  \n")
          (add-text-properties point (1+ point) '(face ivy-posframe-cursor)))))))

(defun ivy-posframe--display-function-prop (fn &rest args)
  "Around advice of FN with ARGS."
  (if (not (display-graphic-p))
      (apply fn args)
    (let ((ivy-display-functions-props
           (append ivy-display-functions-props
                   (mapcar
                    (lambda (elm)
                      `(,elm :cleanup ivy-posframe-cleanup))
                    (mapcar #'cdr ivy-posframe-display-functions-alist)))))
      (apply fn args))))

(defun ivy-posframe--height (fn &rest args)
  "Around advide of FN with ARGS."
  (if (not (display-graphic-p))
      (apply fn args)
    (let ((ivy-height-alist
           (append ivy-posframe-height-alist ivy-height-alist)))
      (apply fn args))))

(defun ivy-posframe--read (fn &rest args)
  "Around advice of FN with AGS."
  (if (not (display-graphic-p))
      (apply fn args)
    (let ((ivy-display-functions-alist
           (append ivy-posframe-display-functions-alist ivy-display-functions-alist)))
      (apply fn args))))

;;;###autoload
(define-minor-mode ivy-posframe-mode
  "Display ivy via posframe."
  :init-value nil
  :global t
  :require 'ivy-posframe
  :lighter ivy-posframe-lighter
  :keymap '(([remap ivy-avy]              . ivy-posframe-avy)
            ([remap swiper-avy]           . ivy-posframe-swiper-avy)
            ([remap ivy-read-action]      . ivy-posframe-read-action)
            ([remap ivy-dispatching-done] . ivy-posframe-dispatching-done)
            ([remap ivy-dispatching-call] . ivy-posframe-dispatching-call))
  (if ivy-posframe-mode
      (mapc (lambda (elm)
              (advice-add (car elm) :around (cdr elm)))
            ivy-posframe-advice-alist)
    (mapc (lambda (elm)
            (advice-remove (car elm) (cdr elm)))
          ivy-posframe-advice-alist)))

(provide 'ivy-posframe)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ivy-posframe.el ends here
