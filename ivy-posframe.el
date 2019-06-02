;;; ivy-posframe.el --- Using posframe to show Ivy  -*- lexical-binding: t -*-


;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/ivy-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching, ivy
;; Package-Requires: ((emacs "26.0")(posframe "0.1.0")(ivy "0.11.0"))

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

;; ivy-posframe is a ivy extension, which let ivy use posframe to show
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
;; (ivy-posframe-mode t)
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
;; (ivy-posframe-mode t)
;; #+END_EXAMPLE
;;
;; You can use ivy original display function on specify function.
;; You may want to use the original display function because display
;; of Swiper at point hides the contents of the buffer.
;; #+BEGIN_EXAMPLE
;; (require 'ivy-posframe)
;; ;; Different command can use different display function.
;; (setq ivy-posframe-display-functions-alist
;;       '((swiper          . nil)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display)))
;; (ivy-posframe-mode t)
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
;;       '((swiper          . nil)
;;         (complete-symbol . ivy-posframe-display-at-point)
;;         (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;;         (t               . ivy-posframe-display)))
;; (ivy-posframe-mode t)
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
;; (push 'ivy-posframe-display-at-XXX ivy-posframe-display-function-list) ; This line is needed.
;; (ivy-posframe-mode t) ; This line is needed.
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

(defcustom ivy-posframe-border-width 1
  "The border width used by ivy-posframe.
When 0, no border is showed."
  :group 'ivy-posframe
  :type 'number)

(defcustom ivy-posframe-hide-minibuffer t
  "Hide input of minibuffer when using ivy-posframe."
  :group 'ivy-posframe
  :type 'boolean)

(defcustom ivy-posframe-parameters nil
  "The frame parameters used by ivy-posframe."
  :group 'ivy-posframe
  :type 'string)

(defcustom ivy-posframe-height-alist nil
  "The `ivy-height-alist' while working ivy-posframe."
  :group 'ivy-posframe
  :type 'sexp)

(defcustom ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
  "The `ivy-display-functions-alist' while working ivy-posframe."
  :group 'ivy-posframe
  :type 'sexp)

(defcustom ivy-posframe-additional-display-functions nil
  "The additional display functions"
  :group 'ivy-posframe
  :type 'sexp)

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

(defvar ivy-posframe-buffer " *ivy-posframe-buffer*"
  "The posframe-buffer used by ivy-posframe.")

(defvar ivy-posframe--ignore-prompt nil
  "When non-nil, ivy-posframe will ignore prompt.
This variable is useful for `ivy-posframe-read-action' .")

(defvar ivy-posframe--display-p nil
  "The status of `ivy-posframe--display'.")

;; Fix warn
(defvar emacs-basic-display)

(defun ivy-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (ivy-display-function-fallback str)
    (setq ivy-posframe--display-p t)
    (with-ivy-window
      (posframe-show
       ivy-posframe-buffer
       :font ivy-posframe-font
       :string str
       :position (point)
       :poshandler poshandler
       :background-color (face-attribute 'ivy-posframe :background nil t)
       :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
       :height ivy-posframe-height
       :width ivy-posframe-width
       :min-height (or ivy-posframe-min-height (+ ivy-height 1))
       :min-width (or ivy-posframe-min-width (round (* (frame-width) 0.62)))
       :internal-border-width ivy-posframe-border-width
       :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
       :override-parameters ivy-posframe-parameters))
    (ivy-posframe--add-prompt 'ignore)))

(defun ivy-posframe-display (str)
  "Display STR via `posframe' by `ivy-posframe-style'."
  (let ((func (intern (format "ivy-posframe-display-at-%s" ivy-posframe-style))))
    (if (functionp func)
        (funcall func str)
      (ivy-posframe-display-at-frame-bottom-left str))))

(defvar ivy-posframe-display-function-alist
  '((window-center      . window-center)
    (frame-center       . frame-center)
    (window-bottom-left . window-bottom-left-corner)
    (frame-bottom-left  . frame-bottom-left-corner)
    (point              . point-bottom-left-corner)))

(eval
 `(progn
    ,@(mapcar
       (lambda (elm)
         `(defun ,(intern (format "ivy-posframe-display-at-%s" (car elm))) (str)
            ,(format "Display STR via `posframe' at %s." (car elm))
            (ivy-posframe--display str #',(intern (format "posframe-poshandler-%s" (cdr elm))))))
       ivy-posframe-display-function-alist)))

(defun ivy-posframe-display-at-frame-bottom-window-center (str)
  "Display STR via `posframe' at frame-bottom-window-center."
  (ivy-posframe--display
   str (lambda (info)
         (cons (car (posframe-poshandler-window-center info))
               (cdr (posframe-poshandler-frame-bottom-left-corner info))))))

(defun ivy-posframe-cleanup ()
  "Cleanup ivy's posframe."
  (when (posframe-workable-p)
    (posframe-hide ivy-posframe-buffer)
    (setq ivy-posframe--display-p nil)))

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

(defun ivy-posframe--window ()
  "Return the posframe window displaying `ivy-posframe-buffer'."
  (frame-selected-window
   (buffer-local-value 'posframe--frame
                       (get-buffer ivy-posframe-buffer))))

(defvar avy-all-windows)
(defvar avy-keys)
(defvar avy-style)

(defun ivy-posframe-avy ()
  "Jump to one of the current ivy candidates."
  (interactive)
  (let ((avy-pre-action #'ignore))
    (with-selected-window (ivy-posframe--window)
      (ivy-avy))))

(declare-function avy--make-backgrounds "avy")
(declare-function avy-window-list "avy")
(declare-function avy-read-de-bruijn "avy")
(declare-function avy-read "avy")
(declare-function avy-tree "avy")
(declare-function avy--overlay-post "avy")
(declare-function avy--remove-leading-chars "avy")
(declare-function avy-push-mark "avy")
(declare-function avy--done "avy")
(defun ivy-posframe--swiper-avy-candidate ()
  (let* ((avy-all-windows nil)
         ;; We'll have overlapping overlays, so we sort all the
         ;; overlays in the visible region by their start, and then
         ;; throw out non-Swiper overlays or overlapping Swiper
         ;; overlays.
         (visible-overlays (cl-sort (with-ivy-window
                                      (overlays-in (window-start)
                                                   (window-end)))
                                    #'< :key #'overlay-start))
         (min-overlay-start 0)
         (overlays-for-avy (cl-remove-if-not
                            (lambda (ov)
                              (when (and (>= (overlay-start ov)
                                             min-overlay-start)
                                         (memq (overlay-get ov 'face)
                                               swiper-faces))
                                (setq min-overlay-start (overlay-start ov))))
                            visible-overlays))
         (offset (if (eq (ivy-state-caller ivy-last) 'swiper) 1 0))
         (window (ivy-posframe--window))
         (candidates (nconc
                      (mapcar (lambda (ov)
                                (cons (overlay-start ov)
                                      (overlay-get ov 'window)))
                              overlays-for-avy)
                      (with-current-buffer ivy-posframe-buffer
                        (save-excursion
                          (save-restriction
                            (narrow-to-region (window-start window)
                                              (window-end window))
                            (goto-char (point-min))
                            (forward-line)
                            (let (cands)
                              (while (not (eobp))
                                (push (cons (+ (point) offset) window)
                                      cands)
                                (forward-line))
                              cands)))))))
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

(declare-function avy-action-goto "avy")
(declare-function avy-candidate-beg "avy")
(defun ivy-posframe-swiper-avy ()
  "Jump to one of the current swiper candidates."
  (interactive)
  (unless (require 'avy nil 'noerror)
    (error "Package avy isn't installed"))
  (unless (string= ivy-text "")
    (let ((candidate (ivy-posframe--swiper-avy-candidate)))
      (if (eq (cdr candidate) (ivy-posframe--window))
          (let ((cand-text (with-current-buffer ivy-posframe-buffer
                             (save-excursion
                               (goto-char (car candidate))
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))))
            (ivy-set-index (cl-position cand-text ivy--old-cands :test #'string=))
            (ivy--exhibit)
            (ivy-done)
            (ivy-call))
        (ivy-quit-and-run
          (avy-action-goto (avy-candidate-beg candidate)))))))

;;; Variables

(defvar ivy-posframe-display-function-list
  (append
   ivy-posframe-additional-display-functions
   (mapcar (lambda (elm) (intern (format "ivy-posframe-display-at-%s" (car elm))))
           ivy-posframe-display-function-alist)
   '(ivy-posframe-display
     ivy-posframe-display-at-frame-bottom-window-center)))

(defvar ivy-posframe-advice-alist
  '((ivy--minibuffer-setup      . ivy-posframe--minibuffer-setup)
    (ivy--queue-exhibit         . ivy-posframe--add-prompt)
    (ivy--display-function-prop . ivy-posframe--display-function-prop)
    (ivy--height                . ivy-posframe--height)
    (ivy-read                   . ivy-posframe--read)))

;;; Advice

(defun ivy-posframe--minibuffer-setup (fn &rest args)
  "Advice function of FN, `ivy--minibuffer-setup' with ARGS."
  (let ((ivy-fixed-height-minibuffer nil))
    (apply fn args))
  (when (and ivy-posframe-hide-minibuffer
             ;; only hide minibuffer's info when posframe is showed.
             ivy-posframe--display-p)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun ivy-posframe--add-prompt (fn &rest args)
  "Add the ivy prompt to the posframe.  Advice FN with ARGS."
  (apply fn args)
  (unless ivy-posframe--ignore-prompt
    (with-current-buffer (window-buffer (active-minibuffer-window))
      (let ((point (point))
            (prompt (buffer-string)))
        (remove-text-properties 0 (length prompt) '(read-only nil) prompt)
        (with-current-buffer ivy-posframe-buffer
          (goto-char (point-min))
          (delete-region (point) (save-excursion (line-move 1 'noerror) (point)))
          (insert prompt "  \n")
          (add-text-properties point (1+ point) '(face ivy-posframe-cursor)))))))

(defun ivy-posframe--display-function-prop (fn &rest args)
  "Around advice of FN with ARGS."
  (let ((ivy-display-functions-props
         (append ivy-display-functions-props
                 (mapcar (lambda (elm) `(,elm :cleanup ivy-posframe-cleanup))
                         ivy-posframe-display-function-list))))
    (apply fn args)))

(defun ivy-posframe--height (fn &rest args)
  "Around advide of FN with ARGS."
  (let ((ivy-height-alist
         (append ivy-posframe-height-alist ivy-height-alist)))
    (apply fn args)))

(defun ivy-posframe--read (fn &rest args)
  "Around advice of FN with AGS."
  (let ((ivy-display-functions-alist
         (append ivy-posframe-display-functions-alist ivy-display-functions-alist)))
    (apply fn args)))

;;;###autoload
(define-minor-mode ivy-posframe-mode
  "Display ivy via posframe."
  :init-value nil
  :global t
  :lighter " ivy-pf"
  :group 'ivy-posframe
  :keymap '(([remap ivy-read-action] ivy-posframe-read-action)
            ([remap ivy-dispatching-done] ivy-posframe-dispatching-done)
            ([remap ivy-avy] ivy-posframe-avy)
            ([remap swiper-avy] ivy-posframe-swiper-avy))
  (let ((advices ivy-posframe-advice-alist))
    (if ivy-posframe-mode
        (eval
         `(progn
            ,@(mapcar (lambda (elm) `(advice-add ',(car elm) :around #',(cdr elm))) advices)))
      (eval
       `(progn
          ,@(mapcar (lambda (elm) `(advice-remove ',(car elm) #',(cdr elm))) advices))))))

;;;###autoload
(defun ivy-posframe-demo ()
  "Toggle a demo config of ivy-posframe.
This function is ONLY used to test ivy-posframe."
  (interactive)
  (ivy-posframe-mode t)
  (let ((config '(t . ivy-posframe-display-at-frame-center)))
    (if (member config ivy-display-functions-alist)
        (progn
          (setq ivy-display-functions-alist
                (remove config ivy-display-functions-alist))
          (message "Ivy-posframe: Demo is disabled."))
      (push config ivy-display-functions-alist)
      (message "Ivy-posframe: Demo is enabled."))))

(provide 'ivy-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ivy-posframe.el ends here
