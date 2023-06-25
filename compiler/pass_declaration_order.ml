open Ext

let fix_declaration_order fixes decls =
  List.fold_left
    (fun decls (enum_name, before) ->
      let decl, decls =
        ListExt.find_remove
          ~pred:(fun d ->
            match d with
            | Parsetree.Enum { name; _ } -> name = enum_name
            | _ -> false)
          decls
      in
      ListExt.insert_before ~item:decl
        ~pred:(fun d ->
          match (d, before) with
          | Parsetree.Event { name; _ }, `Event other_name
          | Request { name; _ }, `Request other_name ->
              name = other_name
          | _ -> false)
        decls)
    decls fixes

let fix_xproto_declaration_order =
  fix_declaration_order
    [
      ("StackMode", `Event "ConfigureRequest");
      ("Pixmap", `Request "CreateWindow");
      ("Cursor", `Request "CreateWindow");
      ("AccessControl", `Request "ListHosts");
      ("Font", `Request "CreateGC");
      ("ConfigWindow", `Event "ConfigureRequest");
    ]
