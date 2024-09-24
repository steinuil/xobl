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

(* This field just didn't have its length specified for some reason. *)
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
                     length =
                       Some
                         (Parsetree.Field_ref
                            {
                              field = "count";
                              type_ = Some (Type_primitive Card32);
                            });
                   }
             | f -> f)
      in
      Parsetree.Request { name; fields; opcode; combine_adjacent; reply; doc }
  | item -> item

(* We consider event structs as an enum, whatever. *)
let fix_xinput_event_struct = function
  | Parsetree.Request
      {
        name = "SendExtensionEvent" as name;
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
                 {
                   name = "events";
                   type_ =
                     { ft_type = Type_ref t as ft_type; ft_allowed = None };
                   length;
                 } ->
                 Parsetree.Field_list
                   {
                     name = "events";
                     type_ = { ft_type; ft_allowed = Some (Allowed_enum t) };
                     length;
                   }
             | f -> f)
      in
      Parsetree.Request { name; fields; opcode; combine_adjacent; reply; doc }
  | item -> item

(* These declarations in xproto are unexplicably out of order. *)
let reorder_enum_declarations fixes decls =
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
  reorder_enum_declarations
    [
      ("StackMode", `Event "ConfigureRequest");
      ("Pixmap", `Request "CreateWindow");
      ("Cursor", `Request "CreateWindow");
      ("AccessControl", `Request "ListHosts");
      ("Font", `Request "CreateGC");
      ("ConfigWindow", `Event "ConfigureRequest");
    ]

(* Gravity is actually two different enums that got merged into one for some reason:
   https://x.org/releases/X11R7.7/doc/xproto/x11protocol.html#Encoding::Common_Types *)
let fix_split_xproto_gravity = function
  | Parsetree.Enum { name = "Gravity"; items = forget :: unmap :: rest; doc } ->
      [
        Parsetree.Enum { name = "BitGravity"; items = forget :: rest; doc };
        Parsetree.Enum { name = "WinGravity"; items = unmap :: rest; doc };
      ]
  | item -> [ item ]

let rec fix_bit_win_gravity_in_field = function
  | Parsetree.Field
      {
        name = "bit_gravity" as name;
        type_ =
          {
            ft_type;
            ft_allowed = Some (Allowed_enum { id_module; id_name = "Gravity" });
          };
      } ->
      Parsetree.Field
        {
          name;
          type_ =
            {
              ft_type;
              ft_allowed =
                Some (Allowed_enum { id_module; id_name = "BitGravity" });
            };
        }
  | Parsetree.Field
      {
        name = "win_gravity" as name;
        type_ =
          {
            ft_type;
            ft_allowed = Some (Allowed_enum { id_module; id_name = "Gravity" });
          };
      } ->
      Parsetree.Field
        {
          name;
          type_ =
            {
              ft_type;
              ft_allowed =
                Some (Allowed_enum { id_module; id_name = "WinGravity" });
            };
        }
  | Field_switch { sw_name; sw_cond; sw_cases } ->
      let sw_cases =
        ListLabels.map sw_cases ~f:(fun c ->
            {
              c with
              Parsetree.cs_fields =
                List.map fix_bit_win_gravity_in_field c.Parsetree.cs_fields;
            })
      in
      Field_switch { sw_name; sw_cond; sw_cases }
  | f -> f

let fix_bit_win_gravity = function
  | Parsetree.Request { name; fields; opcode; combine_adjacent; reply; doc } ->
      let fields = List.map fix_bit_win_gravity_in_field fields in
      let reply =
        Option.map
          (fun r ->
            {
              r with
              Parsetree.fields =
                List.map fix_bit_win_gravity_in_field r.Parsetree.fields;
            })
          reply
      in
      Parsetree.Request { name; fields; opcode; combine_adjacent; reply; doc }
  | item -> item

(* Major and minor opcodes are parsed earlier than the decode phase
   so we don't really need those *)
let remove_opcodes_from_errors_in_field = function
  | Parsetree.Field { name = "minor_opcode"; _ } ->
      Parsetree.Field_pad { pad = Pad_bytes 2; serialize = false }
  | Parsetree.Field { name = "major_opcode"; _ } ->
      Field_pad { pad = Pad_bytes 1; serialize = false }
  | f -> f

let remove_opcodes_from_errors_in_decl = function
  | Parsetree.Error { name; number; fields } ->
      let fields = List.map remove_opcodes_from_errors_in_field fields in
      Parsetree.Error { name; number; fields }
  | d -> d

(* A lot of requests in xinput start with XI and that sucks. *)
(* TODO we should have an original name field because the request name
        in the module also gets affected by this change. *)
let fix_xinput_remove_xi_prefix = function
  | Parsetree.Request { name = "XIGrabDevice" | "XIUngrabDevice"; _ } as d -> d
  | Parsetree.Request ({ name; _ } as req)
    when String.starts_with name ~prefix:"XI" ->
      Parsetree.Request
        {
          req with
          name = StringLabels.sub name ~pos:2 ~len:(String.length name - 2);
        }
  | d -> d

(* Apply the fixes. *)
let ( %> ) f g x = g (f x)

let apply_to expected_file_name file_name fix declarations =
  if file_name = expected_file_name then List.map fix declarations
  else declarations

let apply_fixes = function
  | Parsetree.Extension
      { name; file_name; query_name; multiword; version; declarations } ->
      let declarations =
        declarations
        |> apply_to "xinput" file_name
             (fix_xinput_modifier_mask %> fix_xinput_event_struct
            %> fix_xinput_remove_xi_prefix)
        |> apply_to "dri2" file_name fix_dri2_attachments_length
        |> List.map (fix_bit_win_gravity %> remove_opcodes_from_errors_in_decl)
      in
      Parsetree.Extension
        { name; file_name; query_name; multiword; version; declarations }
  | Core declarations ->
      let declarations =
        declarations |> fix_xproto_declaration_order
        |> List.concat_map fix_split_xproto_gravity
        |> List.map (fix_bit_win_gravity %> remove_opcodes_from_errors_in_decl)
      in
      Core declarations
