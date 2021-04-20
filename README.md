# electric-cursor
## emacs minor-mode to automatically change the cursor

`electric-cursor-mode` is a global minor mode that sets up hooks to change the `cursor-type` in Emacs in certain modes.
The default is to change the cursor to a block in `overwrite-mode`, and a vertical bar in every other mode.

The modes and cursor types are defined in the variable `electric-cursor-alist`, an alist with modes in the `car` position and cursor shapes in the `cdr` positions.
For example, the default looks like this:

```lisp
((overwrite-mode . box))
```

When you enable `electric-cursor-mode`, it adds 
