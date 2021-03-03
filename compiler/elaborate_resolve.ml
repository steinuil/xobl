(** Resolve identifiers to the module they belong. *)

(** The declarations have a few separate namespaces:
    - types
    - enums
    - events and generic events
    - errors
    - requests

    Name clashes within these namespaces are allowed between different modules.
    To disambiguate in the case that a declaration references a
    name exported by two or more modules currently in scope, the modules's
    [file_name] (in {!constructor:Parsetree.xcb.Extension}) is prefixed to the
    name with a colon, such as [xproto:PIXMAP].

    Some modules don't follow this rule, in which case I assumed that the names
    defined in the current module take precedence over the rest. *)

(* let rec resolve_in_expr  *)
