
# &#30446;&#24405;

1.  [ivy-posframe README](#org39783d3)
    1.  [What is ivy-posframe](#orgd46557b)
    2.  [Display functions](#orge009677)
    3.  [How to enable ivy-posframe](#org5a3f479)
    4.  [How to custom your ivy-posframe style](#org333b256)


<a id="org39783d3"></a>

# ivy-posframe README


<a id="orgd46557b"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="orge009677"></a>

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


<a id="org5a3f479"></a>

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

If you use \`ivy-posframe-display', you can use \`ivy-posframe-style'
to set show style.

1.  window-bottom-left style

        (setq ivy-posframe-style 'window-bottom-left)
2.  Window-center style

        (setq ivy-posframe-style 'window-center)
3.  Point style

        (setq ivy-posframe-style 'point)


<a id="org333b256"></a>

## How to custom your ivy-posframe style

The simplest way is:

    (defun ivy-posframe-display-at-XXX (str)
      (ivy-posframe-display str 'your-own-poshandler-function))
    (ivy-posframe-setup) ; This line is need.



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .