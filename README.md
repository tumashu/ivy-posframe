
# &#30446;&#24405;

1.  [ivy-posframe README](#org0fcd3f6)
    1.  [What is ivy-posframe](#org5941743)
    2.  [How to use ivy-posframe](#orgb557f45)
        1.  [How to enable ivy-posframe](#org9056741)
        2.  [Window-buttom-left style](#org3ce6114)
        3.  [Window-center style](#orgadbea28)


<a id="org0fcd3f6"></a>

# ivy-posframe README


<a id="org5941743"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.


<a id="orgb557f45"></a>

## How to use ivy-posframe


<a id="org9056741"></a>

### How to enable ivy-posframe

1.  Global mode

        (require 'ivy-posframe)
        (setq ivy-display-function #'ivy-posframe-display)
2.  Per-command mode.

        (require 'ivy-posframe)
        (push '(counsel-M-x . ivy-posframe-display) ivy-display-functions-alist)
3.  Fallback mode

        (require 'ivy-posframe)
        (push '(t . ivy-posframe-display) ivy-display-functions-alist)


<a id="org3ce6114"></a>

### Window-buttom-left style

    (setq ivy-posframe-style 'window-buttom-left)

![img](./snapshots/ivy-posframe1.gif)


<a id="orgadbea28"></a>

### Window-center style

    (setq ivy-posframe-style 'window-center)

![img](./snapshots/ivy-posframe2.gif)



Converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org) .