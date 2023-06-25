(* Fix an allowed_alt_enum that actually refers to a mask.
   TODO is this correct? Maybe this is on purpose. *)
let fix_modifier_mask = function
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

let fix_xinput_modifier_mask = function
  | Parsetree.Extension
      {
        name;
        file_name = "xinput";
        query_name;
        multiword;
        version;
        declarations;
      } ->
      let declarations = List.map fix_modifier_mask declarations in
      Parsetree.Extension
        {
          name;
          file_name = "xinput";
          query_name;
          multiword;
          version;
          declarations;
        }
  | xcb -> xcb
