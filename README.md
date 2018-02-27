
# &#30446;&#24405;

1.  [ivy-posframe README](#org7b266e1)
    1.  [What is ivy-posframe](#org1c5b744)
    2.  [Display functions](#org2ebfad6)
    3.  [How to enable ivy-posframe](#orgf7e39d4)


<a id="org7b266e1"></a>

# ivy-posframe README


<a id="org1c5b744"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="org2ebfad6"></a>

## Display functions

1.  ivy-posframe-display
2.  ivy-posframe-display-at-frame-center
3.  ivy-posframe-display-at-window-center
4.  ivy-posframe-display-at-frame-buttom-left
5.  ivy-posframe-display-at-window-buttom-left
6.  ivy-posframe-display-at-point


<a id="orgf7e39d4"></a>

## How to enable ivy-posframe

1.  Global mode

        (require 'ivy-posframe)
        (setq ivy-display-function #'ivy-posframe-display)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-buttom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-window-buttom-left)
        ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
2.  Per-command mode.

        (require 'ivy-posframe)
        ;; Different command can use different display function.
        (push '(counsel-M-x . ivy-posframe-display-at-window-buttom-left) ivy-display-functions-alist)
        (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
3.  Fallback mode

        (require 'ivy-posframe)
        (push '(t . ivy-posframe-display) ivy-display-functions-alist)

If you use \`ivy-posframe-display', you can use \`ivy-posframe-style'
to set show style.

1.  window-buttom-left style

        (setq ivy-posframe-style 'window-buttom-left)

    ![img](./snapshots/ivy-posframe1.gif)
2.  Window-center style

        (setq ivy-posframe-style 'window-center)

    ![img](./snapshots/ivy-posframe2.gif)
3.  Point style

        (setq ivy-posframe-style 'point)

    ![img](./snapshots/ivy-posframe3.gif)



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .