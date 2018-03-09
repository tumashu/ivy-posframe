Note: this file is converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [ivy-posframe README](#orgcfa545c)
    1.  [What is ivy-posframe](#org3c6ad89)
    2.  [Display functions](#orgda8366d)
    3.  [How to enable ivy-posframe](#org4d648e7)
    4.  [Tips](#orgcef74d7)
        1.  [How to show fringe to ivy-posframe](#org7a32697)
        2.  [How to custom your ivy-posframe style](#org06a0215)


<a id="orgcfa545c"></a>

# ivy-posframe README


<a id="org3c6ad89"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="orgda8366d"></a>

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


<a id="org4d648e7"></a>

## How to enable ivy-posframe

1.  Global mode

        (require 'ivy-posframe)
        (ivy-posframe-setup)
        (setq ivy-display-function #'ivy-posframe-display)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
2.  Per-command mode.

        (require 'ivy-posframe)
        (ivy-posframe-setup)
        ;; Different command can use different display function.
        (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
        (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
3.  Fallback mode

        (require 'ivy-posframe)
        (ivy-posframe-setup)
        (push '(t . ivy-posframe-display) ivy-display-functions-alist)


<a id="orgcef74d7"></a>

## Tips


<a id="org7a32697"></a>

### How to show fringe to ivy-posframe

    (setq ivy-posframe-parameters
          '((left-fringe . 10)
            (right-fringe . 10)))

By the way, User can set **any** parameters of ivy-posframe with
the help of \`ivy-posframe-parameters'.


<a id="org06a0215"></a>

### How to custom your ivy-posframe style

The simplest way is:

    (defun ivy-posframe-display-at-XXX (str)
      (ivy-posframe--display str #'your-own-poshandler-function))
    (ivy-posframe-setup) ; This line is needed.
