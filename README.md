
# &#30446;&#24405;

1.  [ivy-posframe README](#orgeae4edc)
    1.  [What is ivy-posframe](#org16b85a0)
    2.  [How to enable ivy-posframe](#org6f59c0a)
    3.  [How to set the style of ivy-posframe](#orge1fc1e0)


<a id="orgeae4edc"></a>

# ivy-posframe README


<a id="org16b85a0"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

Package Requires: Emacs 26.0


<a id="org6f59c0a"></a>

## How to enable ivy-posframe

1.  Global mode

        (require 'ivy-posframe)
        (setq ivy-display-function #'ivy-posframe-display)
2.  Per-command mode.

        (require 'ivy-posframe)
        (push '(counsel-M-x . ivy-posframe-display) ivy-display-functions-alist)
3.  Fallback mode

        (require 'ivy-posframe)
        (push '(t . ivy-posframe-display) ivy-display-functions-alist)


<a id="orge1fc1e0"></a>

## How to set the style of ivy-posframe

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
