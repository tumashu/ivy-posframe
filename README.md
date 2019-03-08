Note: this file is auto converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [ivy-posframe README](#org68c65a1)
    1.  [What is ivy-posframe](#org03624ec)
    2.  [Display functions](#org99df55a)
    3.  [How to enable ivy-posframe](#org2040f8e)
        1.  [Global mode](#org6cc7219)
        2.  [Per-command mode.](#orgcd58b06)
        3.  [Fallback mode](#org7941a0a)
    4.  [Tips](#org5cfed99)
        1.  [How to let ivy-posframe show border](#orgb370bbe)
        2.  [How to show fringe to ivy-posframe](#orgb28f068)
        3.  [How to custom your ivy-posframe style](#org96e6893)


<a id="org68c65a1"></a>

# ivy-posframe README


<a id="org03624ec"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe to show
its candidate menu.

NOTE: ivy-posframe requires Emacs 26 and do not support mouse
click.


<a id="org99df55a"></a>

## Display functions

1.  ivy-posframe-display
2.  ivy-posframe-display-at-frame-center
3.  ivy-posframe-display-at-window-center
    ![img](./snapshots/ivy-posframe-display-at-window-center.png)
4.  ivy-posframe-display-at-frame-bottom-left
5.  ivy-posframe-display-at-window-bottom-left
    ![img](./snapshots/ivy-posframe-display-at-window-bottom-left.png)
6.  ivy-posframe-display-at-frame-bottom-window-center
7.  ivy-posframe-display-at-point
    ![img](./snapshots/ivy-posframe-display-at-point.png)


<a id="org2040f8e"></a>

## How to enable ivy-posframe


<a id="org6cc7219"></a>

### Global mode

    (require 'ivy-posframe)
    (setq ivy-display-function #'ivy-posframe-display)
    (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
    (setq ivy-display-function #'ivy-posframe-display-at-window-center)
    (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
    (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
    (setq ivy-display-function #'ivy-posframe-display-at-point)
    (ivy-posframe-enable)


<a id="orgcd58b06"></a>

### Per-command mode.

    (require 'ivy-posframe)
    Different command can use different display function.
    (push '(counsel-M-x . ivy-posframe-display-at-window-bottom-left) ivy-display-functions-alist)
    (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
    (push '(swiper . ivy-posframe-display-at-point) ivy-display-functions-alist)
    (ivy-posframe-enable)

NOTE: Using swiper as example: swiper's display function **only**
take effect when you call swiper command with global keybinding, if
you call swiper command with 'M-x' (for example: counsel-M-x),
counsel-M-x's display function will take effect instead of
swiper's.

The value of variable \`this-command' will be used as the search key
by ivy to find display function in \`ivy-display-functions-alist',
"C-h v this-command" is a good idea.


<a id="org7941a0a"></a>

### Fallback mode

    (require 'ivy-posframe)
    (push '(t . ivy-posframe-display) ivy-display-functions-alist)
    (ivy-posframe-enable)


<a id="org5cfed99"></a>

## Tips


<a id="orgb370bbe"></a>

### How to let ivy-posframe show border

    (set-face-attribute 'internal-border nil :background "gray50")
    (setq ivy-posframe-border-width 1)


<a id="orgb28f068"></a>

### How to show fringe to ivy-posframe

    (setq ivy-posframe-parameters
          '((left-fringe . 8)
            (right-fringe . 8)))

By the way, User can set **any** parameters of ivy-posframe with
the help of \`ivy-posframe-parameters'.


<a id="org96e6893"></a>

### How to custom your ivy-posframe style

The simplest way is:

    (defun ivy-posframe-display-at-XXX (str)
      (ivy-posframe--display str #'your-own-poshandler-function))
    (ivy-posframe-enable) ; This line is needed.

