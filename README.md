# electric-cursor
## An Emacs minor-mode to automatically change the cursor

`electric-cursor-mode` is a global minor mode that sets up hooks to change the `cursor-type` in Emacs in certain modes.
The default is to change the cursor to a block in `overwrite-mode`, and a vertical bar in every other mode.

The modes and cursor types are defined in the variable `electric-cursor-alist`, an alist with modes in the `car` position and cursor shapes in the `cdr` positions.
For example, the default looks like this:

```lisp
((overwrite-mode . box))
```

When you enable `electric-cursor-mode`, it adds hooks to each mode in `electric-cursor-alist` to set the cursor according to `electric-cursor-alist`.
The hooks are removed when you disable `electric-cursor-mode`.

## License

The code in this repository is licensed under the ISC license.  See LICENSE for details.

## Contributing

Open an issue or send me an email (my email address is in electric-cursor.el's header).
