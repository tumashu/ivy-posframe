
# &#30446;&#24405;

1.  [ivy-posframe README](#orgb15c99c)
    1.  [What is ivy-posframe](#org921bc1e)
    2.  [Display functions](#org0339f42)
    3.  [How to enable ivy-posframe](#org8ca033e)


<a id="orgb15c99c"></a>

# ivy-posframe README


<a id="org921bc1e"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="org0339f42"></a>

## Display functions

1.  ivy-posframe-display
2.  ivy-posframe-display-at-frame-center
3.  ivy-posframe-display-at-window-center
4.  ivy-posframe-display-at-frame-bottom-left
5.  ivy-posframe-display-at-window-bottom-left
6.  ivy-posframe-display-at-point


<a id="org8ca033e"></a>

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

    ![img](./snapshots/ivy-posframe1.gif)
2.  Window-center style

        (setq ivy-posframe-style 'window-center)

    ![img](./snapshots/ivy-posframe2.gif)
3.  Point style

        (setq ivy-posframe-style 'point)

    ![img](./snapshots/ivy-posframe3.gif)



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .