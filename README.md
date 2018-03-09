Note: this file is converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [ivy-posframe README](#org7f9a5fe)
    1.  [What is ivy-posframe](#org5057226)
    2.  [Display functions](#orgbde602c)
    3.  [How to enable ivy-posframe](#org6bfc45d)
    4.  [Tips](#orgd52c7c9)
        1.  [How to show fringe to ivy-posframe](#orga286c1f)
        2.  [How to custom your ivy-posframe style](#orgf8a38da)


<a id="org7f9a5fe"></a>

# ivy-posframe README


<a id="org5057226"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="orgbde602c"></a>

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


<a id="org6bfc45d"></a>

## How to enable ivy-posframe

1.  Global mode

        (require 'ivy-posframe)
        (setq ivy-display-function #'ivy-posframe-display)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
        (ivy-posframe-enable)
2.  Per-command mode.

        (require 'ivy-posframe)
        ;; Different command can use different display function.
        (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
        (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
        (ivy-posframe-enable)
3.  Fallback mode

        (require 'ivy-posframe)
        (push '(t . ivy-posframe-display) ivy-display-functions-alist)
        (ivy-posframe-enable)


<a id="orgd52c7c9"></a>

## Tips


<a id="orga286c1f"></a>

### How to show fringe to ivy-posframe

    (setq ivy-posframe-parameters
          '((left-fringe . 10)
            (right-fringe . 10)))

By the way, User can set **any** parameters of ivy-posframe with
the help of \`ivy-posframe-parameters'.


<a id="orgf8a38da"></a>

### How to custom your ivy-posframe style

The simplest way is:

    (defun ivy-posframe-display-at-XXX (str)
      (ivy-posframe--display str #'your-own-poshandler-function))
    (ivy-posframe-enable) ; This line is needed.
