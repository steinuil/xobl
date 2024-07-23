# xobl

The X11 OCaml Bindings Library. A Pure OCaml implementation of the X11 protocol.

## TODO

- Tweaks to HIR
  - [ ] GetProperty reply: remove value_len. It's left in because the length field is calculated using both value_len and format.
  - [ ] prune unused enums and masks after converting them to variants and optional fields.
  - [ ] I disabled warning 11 in the ocaml backend because of some questionable enums that include 0 for more than one case. We should fix those enums.
    - [x] xproto.GRAVITY should really be split into two enums like this: https://x.org/releases/X11R7.7/doc/xproto/x11protocol.html#Encoding::Common_Types
    - [ ] Atom_enum is not really an enum, it's just a list of constants + None and Any. It should be turned into another kind of type.
  - [ ] xkb requires a lot of fixes.
- Bindings
  - [ ] come up with a better API for sending multiple requests at once.
    - how do we encode stuff which needs a reply?
  - [ ] events/variant structs/errors that are only made of one field should be converted to that one field.
    - notify_variant
- OCaml backend

## Documentation

- https://www.x.org/releases/X11R7.7/doc/
- https://www.x.org/wiki/Development/
- Many of the reasons behind decisions in the XCB protocol spec were documented in the commit messages on https://cgit.freedesktop.org/xcb/proto/log/

## Other implementations generated from the spec

- Guile: [guile-xcb](https://github.com/mwitmer/guile-xcb)
- Emacs Lisp: [xelb](https://github.com/ch11ng/xelb)
- Haskell: [XHB](https://github.com/aslatter/xhb) ([parser](https://github.com/aslatter/xcb-types))
- Elixir: [XEB](https://github.com/chrys-h/XEB)
- Javascript (Node): [node-x11](https://github.com/sidorares/node-x11)
- Clojure: [xcljb](https://github.com/geremih/xcljb)
- Rust: [rust-xcb](https://github.com/sstewartgallus/rust-xcb), [x11rb](https://github.com/psychon/x11rb)
- Ruby: [alembic](https://github.com/nbaum/alembic)
- Go: [xgb](https://github.com/BurntSushi/xgb)

## Xlib implementations (not generated) of note

- Python: [python-xlib](https://github.com/python-xlib/python-xlib)
- Common Lisp: [CLX](https://github.com/sharplispers/clx)

## Further reading/watching

- [The real story behind Wayland and X](https://www.youtube.com/watch?v=GWQh_DmDLKQ) (Daniel Stone)
