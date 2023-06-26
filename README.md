# xobl

The X11 OCaml Bindings Library. A Pure OCaml implementation of the X11 protocol.

## TODO

- Tweaks to HIR
  - [ ] GetProperty reply: remove value_len. It's left in because the length field is calculated using both value_len and format.
  - [ ] prune unused enums and masks after converting them to variants and optional fields.

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
