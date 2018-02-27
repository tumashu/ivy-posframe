
# &#30446;&#24405;

1.  [ivy-posframe README](#org31b7799)
    1.  [What is ivy-posframe](#orge59727b)
    2.  [Display functions](#orgf8e0e78)
    3.  [How to enable ivy-posframe](#orgd1a99ab)


<a id="org31b7799"></a>

# ivy-posframe README


<a id="orge59727b"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26


<a id="orgf8e0e78"></a>

## Display functions

1.  ivy-posframe-display
2.  ivy-posframe-display-at-frame-center
3.  ivy-posframe-display-at-window-center
    ![img](./snapshots/ivy-posframe-display-at-window-center.gif)
4.  ivy-posframe-display-at-frame-bottom-left
5.  ivy-posframe-display-at-window-bottom-left
    ![img](./snapshots/ivy-posframe-display-at-window-bottom-left.gif)
6.  ivy-posframe-display-at-point
    ![img](./snapshots/ivy-posframe-display-at-point.gif)


<a id="orgd1a99ab"></a>

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



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .