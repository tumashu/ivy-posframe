
# &#30446;&#24405;

1.  [ivy-posframe README](#org56f4250)
    1.  [What is ivy-posframe](#orge48b949)
    2.  [How to enable ivy-posframe](#orgf9d7738)
    3.  [How to set the style of ivy-posframe](#org3923198)


<a id="org56f4250"></a>

# ivy-posframe README


<a id="orge48b949"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.


<a id="orgf9d7738"></a>

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


<a id="org3923198"></a>

## How to set the style of ivy-posframe

1.  window-buttom-left style

        (setq ivy-posframe-style 'window-buttom-left)

    ![img](./snapshots/ivy-posframe1.gif)
2.  Window-center style

        (setq ivy-posframe-style 'window-center)

    ![img](./snapshots/ivy-posframe2.gif)



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .