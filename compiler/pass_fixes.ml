open Ext

(* Fix an allowed_alt_enum that actually refers to a mask.
   TODO is this correct? Maybe this is on purpose. *)
let fix_xinput_modifier_mask = function
  | Parsetree.Struct { name = "GrabModifierInfo"; fields } ->
      Parsetree.Struct
        {
          name = "GrabModifierInfo";
          fields =
            List.map
              (function
                | Parsetree.Field
                    {
                      name = "modifiers";
                      type_ =
                        { ft_allowed = Some (Allowed_alt_enum mask); _ } as t;
                    } ->
                    Parsetree.Field
                      {
                        name = "modifiers";
                        type_ =
                          { t with ft_allowed = Some (Allowed_alt_mask mask) };
                      }
                | item -> item)
              fields;
        }
  | item -> item

let fix_dri2_attachments_length = function
  | Parsetree.Request
      {
        name = ("GetBuffers" | "GetBuffersWithFormat") as name;
        fields;
        opcode;
        combine_adjacent;
        reply;
        doc;
      } ->
      let fields =
        fields
        |> List.map (function
             | Parsetree.Field_list
                 { name = "attachments"; type_; length = None } ->
                 Parsetree.Field_list
                   {
                     name = "attachments";
                     type_;
                     length = Some (Parsetree.Field_ref "count");
                   }
             | f -> f)
      in
      Parsetree.Request { name; fields; opcode; combine_adjacent; reply; doc }
  | item -> item

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

let apply_fixes = function
  | Parsetree.Extension
      { name; file_name = "dri2"; query_name; multiword; version; declarations }
    ->
      let declarations = List.map fix_dri2_attachments_length declarations in
      Parsetree.Extension
        {
          name;
          file_name = "dri2";
          query_name;
          multiword;
          version;
          declarations;
        }
  | Extension
      {
        name;
        file_name = "xinput";
        query_name;
        multiword;
        version;
        declarations;
      } ->
      let declarations = List.map fix_xinput_modifier_mask declarations in
      Parsetree.Extension
        {
          name;
          file_name = "xinput";
          query_name;
          multiword;
          version;
          declarations;
        }
  | Core declarations ->
      Parsetree.Core (fix_xproto_declaration_order declarations)
  | xcb -> xcb
