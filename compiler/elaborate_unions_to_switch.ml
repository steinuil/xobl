(** Turn unions into switches. *)

(** Since unions are a bad feature of the X11 spec and (excluding those in xkb)
    there's only two in the entire spec, this module takes care of turning them
    into proper switches simply by special-casing them.

    {2 Why are unions bad?}

    Because they're effectively switches, but with ad-hoc logic required to
    discriminate between the cases. Both uses in the spec also have another
    field in the same event that serves to discriminate the branch of the union
    that should be used, so this module simply makes this connection explicit
    by getting rid of the union and turning it into a switch. *)

open Parsetree

let xproto_clientmessage_to_switch decls =
  let union =
    List.find_map
      (function
        | Union { name; members } when name = "ClientMessageData" ->
            Some members
        | _ -> None)
      decls
    |> Option.get
  in
  decls
  |> List.map (function
       | Union _ ->
           Enum
             {
               name = "ClientMessageDataFormat";
               items =
                 [
                   ("data8", Item_value 8L);
                   ("data16", Item_value 16L);
                   ("data32", Item_value 32L);
                 ];
               doc = None;
             }
       | Event ({ name; fields; _ } as ev) when name = "ClientMessage" ->
           let format_enum =
             { id_module = Some "xproto"; id_name = "ClientMessageDataFormat" }
           in
           let fields =
             List.map
               (function
                 | Field { name; type_ = { ft_type; _ } } when name = "format"
                   ->
                     let ft_allowed = Some (Allowed_enum format_enum) in
                     Field { name; type_ = { ft_type; ft_allowed } }
                 | Field { name; _ } when name = "data" ->
                     Field_switch
                       {
                         sw_name = "data";
                         sw_cond = Cond_eq (Field_ref "format");
                         sw_cases =
                           union
                           |> List.map (function
                                | Field_list { name; _ } as f ->
                                    {
                                      cs_name = None;
                                      cs_cond =
                                        [
                                          Enum_ref
                                            { enum = format_enum; item = name };
                                        ];
                                      cs_fields = [ f ];
                                    }
                                | _ -> failwith "unexpected");
                       }
                 | f -> f)
               fields
           in
           Event { ev with fields }
       | decl -> decl)

let%expect_test _ =
  List.iter (fun x -> show_declaration x |> print_endline)
  @@ xproto_clientmessage_to_switch
       [
         Union
           {
             name = "ClientMessageData";
             members =
               [
                 Field_list
                   {
                     name = "data8";
                     type_ =
                       { ft_type = Type_primitive Card8; ft_allowed = None };
                     length = Some (Expr_value 20L);
                   };
                 Field_list
                   {
                     name = "data16";
                     type_ =
                       { ft_type = Type_primitive Card16; ft_allowed = None };
                     length = Some (Expr_value 10L);
                   };
                 Field_list
                   {
                     name = "data32";
                     type_ =
                       { ft_type = Type_primitive Card32; ft_allowed = None };
                     length = Some (Expr_value 5L);
                   };
               ];
           };
         Event
           {
             name = "ClientMessage";
             number = 33;
             is_generic = false;
             no_sequence_number = false;
             fields =
               [
                 Field
                   {
                     name = "format";
                     type_ =
                       { ft_type = Type_primitive Card8; ft_allowed = None };
                   };
                 Field
                   {
                     name = "window";
                     type_ =
                       {
                         ft_type =
                           Type_ref { id_module = None; id_name = "WINDOW" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "type";
                     type_ =
                       {
                         ft_type =
                           Type_ref { id_module = None; id_name = "ATOM" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "data";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "ClientMessageData" };
                         ft_allowed = None;
                       };
                   };
               ];
             doc = Some Doc;
           };
       ];
  [%expect
    {|
    Parsetree.Enum {name = "ClientMessageDataFormat";
      items =
      [("data8", (Parsetree.Item_value 8L));
        ("data16", (Parsetree.Item_value 16L));
        ("data32", (Parsetree.Item_value 32L))];
      doc = None}
    Parsetree.Event {name = "ClientMessage"; number = 33; is_generic = false;
      no_sequence_number = false;
      fields =
      [Parsetree.Field {name = "format";
         type_ =
         { Parsetree.ft_type = (Parsetree.Type_primitive Parsetree.Card8);
           ft_allowed =
           (Some (Parsetree.Allowed_enum
                    { Parsetree.id_module = (Some "xproto");
                      id_name = "ClientMessageDataFormat" }))
           }};
        Parsetree.Field {name = "window";
          type_ =
          { Parsetree.ft_type =
            (Parsetree.Type_ref
               { Parsetree.id_module = None; id_name = "WINDOW" });
            ft_allowed = None }};
        Parsetree.Field {name = "type";
          type_ =
          { Parsetree.ft_type =
            (Parsetree.Type_ref { Parsetree.id_module = None; id_name = "ATOM" });
            ft_allowed = None }};
        (Parsetree.Field_switch
           { Parsetree.sw_name = "data";
             sw_cond = (Parsetree.Cond_eq (Parsetree.Field_ref "format"));
             sw_cases =
             [{ Parsetree.cs_name = None;
                cs_cond =
                [Parsetree.Enum_ref {
                   enum =
                   { Parsetree.id_module = (Some "xproto");
                     id_name = "ClientMessageDataFormat" };
                   item = "data8"}
                  ];
                cs_fields =
                [Parsetree.Field_list {name = "data8";
                   type_ =
                   { Parsetree.ft_type =
                     (Parsetree.Type_primitive Parsetree.Card8);
                     ft_allowed = None };
                   length = (Some (Parsetree.Expr_value 20L))}
                  ]
                };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "xproto");
                      id_name = "ClientMessageDataFormat" };
                    item = "data16"}
                   ];
                 cs_fields =
                 [Parsetree.Field_list {name = "data16";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_primitive Parsetree.Card16);
                      ft_allowed = None };
                    length = (Some (Parsetree.Expr_value 10L))}
                   ]
                 };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "xproto");
                      id_name = "ClientMessageDataFormat" };
                    item = "data32"}
                   ];
                 cs_fields =
                 [Parsetree.Field_list {name = "data32";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_primitive Parsetree.Card32);
                      ft_allowed = None };
                    length = (Some (Parsetree.Expr_value 5L))}
                   ]
                 }
               ]
             })
        ];
      doc = (Some Parsetree.Doc)} |}]

let randr_notify_to_switch decls =
  let union =
    decls
    |> List.find_map (function
         | Union { name; members } when name = "NotifyData" -> Some members
         | _ -> None)
    |> Option.get
  in
  decls
  |> List.filter (function Union _ -> false | _ -> true)
  |> List.map (function
       | Event ({ name; fields; _ } as ev) when name = "Notify" ->
           let enum = { id_module = Some "randr"; id_name = "Notify" } in
           let fields =
             List.map
               (function
                 | Field { name; type_ = { ft_type; _ } } when name = "subCode"
                   ->
                     Field
                       {
                         name;
                         type_ =
                           { ft_type; ft_allowed = Some (Allowed_enum enum) };
                       }
                 | Field { name; _ } when name = "u" ->
                     Field_switch
                       {
                         sw_name = "u";
                         sw_cond = Cond_eq (Field_ref "subCode");
                         sw_cases =
                           union
                           |> List.map (function
                                | Field
                                    {
                                      type_ =
                                        { ft_type = Type_ref { id_name; _ }; _ };
                                      _;
                                    } as f ->
                                    {
                                      cs_name = None;
                                      cs_cond =
                                        [ Enum_ref { enum; item = id_name } ];
                                      cs_fields = [ f ];
                                    }
                                | _ -> failwith "unexpected");
                       }
                 | f -> f)
               fields
           in
           Event { ev with fields }
       | decl -> decl)

let%expect_test _ =
  List.iter (fun x -> show_declaration x |> print_endline)
  @@ randr_notify_to_switch
       [
         Union
           {
             name = "NotifyData";
             members =
               [
                 Field
                   {
                     name = "cc";
                     type_ =
                       {
                         ft_type =
                           Type_ref { id_module = None; id_name = "CrtcChange" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "oc";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "OutputChange" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "op";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "OutputProperty" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "pc";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "ProviderChange" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "pp";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "ProviderProperty" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "rc";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "ResourceChange" };
                         ft_allowed = None;
                       };
                   };
                 Field
                   {
                     name = "lc";
                     type_ =
                       {
                         ft_type =
                           Type_ref
                             { id_module = None; id_name = "LeaseNotify" };
                         ft_allowed = None;
                       };
                   };
               ];
           };
         Event
           {
             name = "Notify";
             number = 1;
             is_generic = false;
             no_sequence_number = false;
             fields =
               [
                 Field
                   {
                     name = "subCode";
                     type_ =
                       {
                         ft_type = Type_primitive Card8;
                         ft_allowed =
                           Some
                             (Allowed_enum
                                { id_module = None; id_name = "Notify" });
                       };
                   };
                 Field
                   {
                     name = "u";
                     type_ =
                       {
                         ft_type =
                           Type_ref { id_module = None; id_name = "NotifyData" };
                         ft_allowed = None;
                       };
                   };
               ];
             doc = None;
           };
       ];
  [%expect
    {|
    Parsetree.Event {name = "Notify"; number = 1; is_generic = false;
      no_sequence_number = false;
      fields =
      [Parsetree.Field {name = "subCode";
         type_ =
         { Parsetree.ft_type = (Parsetree.Type_primitive Parsetree.Card8);
           ft_allowed =
           (Some (Parsetree.Allowed_enum
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" }))
           }};
        (Parsetree.Field_switch
           { Parsetree.sw_name = "u";
             sw_cond = (Parsetree.Cond_eq (Parsetree.Field_ref "subCode"));
             sw_cases =
             [{ Parsetree.cs_name = None;
                cs_cond =
                [Parsetree.Enum_ref {
                   enum =
                   { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                   item = "CrtcChange"}
                  ];
                cs_fields =
                [Parsetree.Field {name = "cc";
                   type_ =
                   { Parsetree.ft_type =
                     (Parsetree.Type_ref
                        { Parsetree.id_module = None; id_name = "CrtcChange" });
                     ft_allowed = None }}
                  ]
                };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                    item = "OutputChange"}
                   ];
                 cs_fields =
                 [Parsetree.Field {name = "oc";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_ref
                         { Parsetree.id_module = None; id_name = "OutputChange" });
                      ft_allowed = None }}
                   ]
                 };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                    item = "OutputProperty"}
                   ];
                 cs_fields =
                 [Parsetree.Field {name = "op";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_ref
                         { Parsetree.id_module = None; id_name = "OutputProperty"
                           });
                      ft_allowed = None }}
                   ]
                 };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                    item = "ProviderChange"}
                   ];
                 cs_fields =
                 [Parsetree.Field {name = "pc";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_ref
                         { Parsetree.id_module = None; id_name = "ProviderChange"
                           });
                      ft_allowed = None }}
                   ]
                 };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                    item = "ProviderProperty"}
                   ];
                 cs_fields =
                 [Parsetree.Field {name = "pp";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_ref
                         { Parsetree.id_module = None;
                           id_name = "ProviderProperty" });
                      ft_allowed = None }}
                   ]
                 };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                    item = "ResourceChange"}
                   ];
                 cs_fields =
                 [Parsetree.Field {name = "rc";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_ref
                         { Parsetree.id_module = None; id_name = "ResourceChange"
                           });
                      ft_allowed = None }}
                   ]
                 };
               { Parsetree.cs_name = None;
                 cs_cond =
                 [Parsetree.Enum_ref {
                    enum =
                    { Parsetree.id_module = (Some "randr"); id_name = "Notify" };
                    item = "LeaseNotify"}
                   ];
                 cs_fields =
                 [Parsetree.Field {name = "lc";
                    type_ =
                    { Parsetree.ft_type =
                      (Parsetree.Type_ref
                         { Parsetree.id_module = None; id_name = "LeaseNotify" });
                      ft_allowed = None }}
                   ]
                 }
               ]
             })
        ];
      doc = None} |}]

let unions_to_switch = function
  | Core decls -> Core (xproto_clientmessage_to_switch decls)
  | Extension ({ file_name = n; declarations = decls; _ } as ext)
    when n = "randr" ->
      Extension { ext with declarations = randr_notify_to_switch decls }
  | Extension _ as ext -> ext
