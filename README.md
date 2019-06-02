Note: this file is auto converted from ivy-posframe.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# Table of Contents

1.  [ivy-posframe README](#orgd3c904b)
    1.  [What is ivy-posframe](#org0585c98)
    2.  [Display functions](#org33d88e8)
    3.  [How to enable ivy-posframe](#org1951a71)
        1.  [Global mode](#orga4b2102)
        2.  [Per-command mode.](#org5502e77)
    4.  [Tips](#orgaaa4442)
        1.  [How to show fringe to ivy-posframe](#org84cabb8)
        2.  [How to custom your ivy-posframe style](#orgc5ae827)


<a id="orgd3c904b"></a>

# ivy-posframe README


<a id="org0585c98"></a>

## What is ivy-posframe

ivy-posframe is a ivy extension, which let ivy use posframe to show
its candidate menu.

NOTE: ivy-posframe requires Emacs 26 and do not support mouse
click.


<a id="org33d88e8"></a>

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


<a id="org1951a71"></a>

## How to enable ivy-posframe


<a id="orga4b2102"></a>

### Global mode

    (require 'ivy-posframe)
    (setq ivy-posframe-configure-alist
          '((ivy-display-functions-alist . ((t . ivy-posframe-display)))))
    ;; (setq ivy-posframe-configure-alist
    ;;       '((ivy-display-functions-alist . ((t . ivy-posframe-display-at-frame-center)))))
    ;; (setq ivy-posframe-configure-alist
    ;;       '((ivy-display-functions-alist . ((t . ivy-posframe-display-at-window-center)))))
    ;; (setq ivy-posframe-configure-alist
    ;;       '((ivy-display-functions-alist . ((t . ivy-posframe-display-at-frame-bottom-left)))))
    ;; (setq ivy-posframe-configure-alist
    ;;       '((ivy-display-functions-alist . ((t . ivy-posframe-display-at-window-bottom-left)))))
    ;; (setq ivy-posframe-configure-alist
    ;;       '((ivy-display-functions-alist . ((t . ivy-posframe-display-at-point)))))
    (ivy-posframe-mode t)


<a id="org5502e77"></a>

### Per-command mode.

    (require 'ivy-posframe)
    ;; Different command can use different display function.
    (setq ivy-posframe-configure-alist
          '((ivy-display-functions-alist . ((swiper          . ivy-posframe-display-at-point)
                                            (complete-symbol . ivy-posframe-display-at-point)
                                            (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
                                            (t               . ivy-posframe-display)))))
    (ivy-posframe-mode t)

You can use ivy original display function on specify function.
You may want to use the original display function because display
of Swiper at point hides the contents of the buffer.

    (require 'ivy-posframe)
    ;; Different command can use different display function.
    (setq ivy-posframe-configure-alist
          '((ivy-display-functions-alist . ((swiper          . nil)
                                            (complete-symbol . ivy-posframe-display-at-point)
                                            (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
                                            (t               . ivy-posframe-display)))))
    (ivy-posframe-mode t)

NOTE: Using swiper as example: swiper's display function **only**
take effect when you call swiper command with global keybinding, if
you call swiper command with 'M-x' (for example: counsel-M-x),
counsel-M-x's display function will take effect instead of
swiper's.

The value of variable \`this-command' will be used as the search key
by ivy to find display function in \`ivy-display-functions-alist',
"C-h v this-command" is a good idea.


<a id="orgaaa4442"></a>

## Tips


<a id="org84cabb8"></a>

### How to show fringe to ivy-posframe

    (setq ivy-posframe-parameters
          '((left-fringe . 8)
            (right-fringe . 8)))

By the way, User can set **any** parameters of ivy-posframe with
the help of \`ivy-posframe-parameters'.


<a id="orgc5ae827"></a>

### How to custom your ivy-posframe style

The simplest way is:

    (defun ivy-posframe-display-at-XXX (str)
      (ivy-posframe--display str #'your-own-poshandler-function))
    (push 'ivy-posframe-display-at-XXX ivy-posframe-display-function-list) ; This line is needed.
    (ivy-posframe-mode t) ; This line is needed.

