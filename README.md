
# &#30446;&#24405;

1.  [ivy-posframe README](#orgad72029)
    1.  [What is ivy-posframe](#orgf3fce48)
    2.  [Display functions](#org1420034)
    3.  [How to enable ivy-posframe](#orgcd8666c)
    4.  [How to custom your ivy-posframe style](#org7c1f241)


<a id="orgad72029"></a>

# ivy-posframe README


<a id="orgf3fce48"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="org1420034"></a>

## Display functions

1.  ivy-posframe-display
2.  ivy-posframe-display-at-frame-center
3.  ivy-posframe-display-at-window-center
    ![img](./snapshots/ivy-posframe-display-at-window-center.gif)
4.  ivy-posframe-display-at-frame-bottom-left
5.  ivy-posframe-display-at-window-bottom-left
    ![img](./snapshots/ivy-posframe-display-at-window-bottom-left.gif)
6.  ivy-posframe-display-at-frame-bottom-window-center
7.  ivy-posframe-display-at-point
    ![img](./snapshots/ivy-posframe-display-at-point.gif)


<a id="orgcd8666c"></a>

## How to enable ivy-posframe

1.  Global mode

        (require 'ivy-posframe)
        (setq ivy-display-function #'ivy-posframe-display)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
2.  Per-command mode.

        (require 'ivy-posframe)
        ;; Different command can use different display function.
        (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
        (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
3.  Fallback mode

        (require 'ivy-posframe)
        (push '(t . ivy-posframe-display) ivy-display-functions-alist)


<a id="org7c1f241"></a>

## How to custom your ivy-posframe style

The simplest way is:

    (defun ivy-posframe-display-at-XXX (str)
      (ivy-posframe-display str 'your-own-poshandler-function))
    (ivy-posframe-setup) ; This line is need.



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .