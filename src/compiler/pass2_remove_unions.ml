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
open Ext

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
  let clientmessage =
    Sexplib.Sexp.of_string_many_conv_exn
      {sexp|
      (Union (name ClientMessageData)
       (members
        ((Field_list (name data8)
          (type_ ((ft_type (Type_primitive Card8)) (ft_allowed ())))
          (length ((Expr_value 20))))
         (Field_list (name data16)
          (type_ ((ft_type (Type_primitive Card16)) (ft_allowed ())))
          (length ((Expr_value 10))))
         (Field_list (name data32)
          (type_ ((ft_type (Type_primitive Card32)) (ft_allowed ())))
          (length ((Expr_value 5)))))))
      (Event (name ClientMessage) (number 33) (is_generic false)
       (is_serializable false) (no_sequence_number false)
       (fields
        ((Field (name format)
          (type_ ((ft_type (Type_primitive Card8)) (ft_allowed ()))))
         (Field (name window)
          (type_
           ((ft_type (Type_ref ((id_module (xproto)) (id_name WINDOW))))
            (ft_allowed ()))))
         (Field (name type)
          (type_
           ((ft_type (Type_ref ((id_module (xproto)) (id_name ATOM))))
            (ft_allowed ()))))
         (Field (name data)
          (type_
           ((ft_type
             (Type_ref ((id_module (xproto)) (id_name ClientMessageData))))
            (ft_allowed ()))))))
       (doc (Doc))) |sexp}
      declaration_of_sexp
  in
  xproto_clientmessage_to_switch clientmessage
  |> List.iter (SexpExt.print_hum ~conv:sexp_of_declaration);
  [%expect
    {|
    (Enum (name ClientMessageDataFormat)
     (items
      ((data8 (Item_value 8)) (data16 (Item_value 16)) (data32 (Item_value 32))))
     (doc ()))
    (Event (name ClientMessage) (number 33) (is_generic false)
     (is_serializable false) (no_sequence_number false)
     (fields
      ((Field (name format)
        (type_
         ((ft_type (Type_primitive Card8))
          (ft_allowed
           ((Allowed_enum
             ((id_module (xproto)) (id_name ClientMessageDataFormat))))))))
       (Field (name window)
        (type_
         ((ft_type (Type_ref ((id_module (xproto)) (id_name WINDOW))))
          (ft_allowed ()))))
       (Field (name type)
        (type_
         ((ft_type (Type_ref ((id_module (xproto)) (id_name ATOM))))
          (ft_allowed ()))))
       (Field_switch
        ((sw_name data) (sw_cond (Cond_eq (Field_ref format)))
         (sw_cases
          (((cs_name ())
            (cs_cond
             ((Enum_ref
               (enum ((id_module (xproto)) (id_name ClientMessageDataFormat)))
               (item data8))))
            (cs_fields
             ((Field_list (name data8)
               (type_ ((ft_type (Type_primitive Card8)) (ft_allowed ())))
               (length ((Expr_value 20)))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref
               (enum ((id_module (xproto)) (id_name ClientMessageDataFormat)))
               (item data16))))
            (cs_fields
             ((Field_list (name data16)
               (type_ ((ft_type (Type_primitive Card16)) (ft_allowed ())))
               (length ((Expr_value 10)))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref
               (enum ((id_module (xproto)) (id_name ClientMessageDataFormat)))
               (item data32))))
            (cs_fields
             ((Field_list (name data32)
               (type_ ((ft_type (Type_primitive Card32)) (ft_allowed ())))
               (length ((Expr_value 5)))))))))))))
     (doc (Doc))) |}]

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
                                        [
                                          Enum_ref
                                            {
                                              enum;
                                              item =
                                                (if id_name = "LeaseNotify" then
                                                   "Lease"
                                                 else id_name);
                                            };
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
  let randr_notify =
    Sexplib.Sexp.of_string_many_conv_exn
      {sexp|
      (Union (name NotifyData)
       (members
        ((Field (name cc)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name CrtcChange))))
            (ft_allowed ()))))
         (Field (name oc)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name OutputChange))))
            (ft_allowed ()))))
         (Field (name op)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name OutputProperty))))
            (ft_allowed ()))))
         (Field (name pc)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name ProviderChange))))
            (ft_allowed ()))))
         (Field (name pp)
          (type_
           ((ft_type
             (Type_ref ((id_module (randr)) (id_name ProviderProperty))))
            (ft_allowed ()))))
         (Field (name rc)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name ResourceChange))))
            (ft_allowed ()))))
         (Field (name lc)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name LeaseNotify))))
            (ft_allowed ())))))))
      (Event (name Notify) (number 1) (is_generic false)
       (is_serializable false) (no_sequence_number false)
       (fields
        ((Field (name subCode)
          (type_
           ((ft_type (Type_primitive Card8))
            (ft_allowed ((Allowed_enum ((id_module (randr)) (id_name Notify))))))))
         (Field (name u)
          (type_
           ((ft_type (Type_ref ((id_module (randr)) (id_name NotifyData))))
            (ft_allowed ()))))))
       (doc ()))
      |sexp}
      declaration_of_sexp
  in
  randr_notify_to_switch randr_notify
  |> List.iter (SexpExt.print_hum ~conv:sexp_of_declaration);
  [%expect
    {|
    (Event (name Notify) (number 1) (is_generic false) (is_serializable false)
     (no_sequence_number false)
     (fields
      ((Field (name subCode)
        (type_
         ((ft_type (Type_primitive Card8))
          (ft_allowed ((Allowed_enum ((id_module (randr)) (id_name Notify))))))))
       (Field_switch
        ((sw_name u) (sw_cond (Cond_eq (Field_ref subCode)))
         (sw_cases
          (((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item CrtcChange))))
            (cs_fields
             ((Field (name cc)
               (type_
                ((ft_type (Type_ref ((id_module (randr)) (id_name CrtcChange))))
                 (ft_allowed ())))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item OutputChange))))
            (cs_fields
             ((Field (name oc)
               (type_
                ((ft_type
                  (Type_ref ((id_module (randr)) (id_name OutputChange))))
                 (ft_allowed ())))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item OutputProperty))))
            (cs_fields
             ((Field (name op)
               (type_
                ((ft_type
                  (Type_ref ((id_module (randr)) (id_name OutputProperty))))
                 (ft_allowed ())))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item ProviderChange))))
            (cs_fields
             ((Field (name pc)
               (type_
                ((ft_type
                  (Type_ref ((id_module (randr)) (id_name ProviderChange))))
                 (ft_allowed ())))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item ProviderProperty))))
            (cs_fields
             ((Field (name pp)
               (type_
                ((ft_type
                  (Type_ref ((id_module (randr)) (id_name ProviderProperty))))
                 (ft_allowed ())))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item ResourceChange))))
            (cs_fields
             ((Field (name rc)
               (type_
                ((ft_type
                  (Type_ref ((id_module (randr)) (id_name ResourceChange))))
                 (ft_allowed ())))))))
           ((cs_name ())
            (cs_cond
             ((Enum_ref (enum ((id_module (randr)) (id_name Notify)))
               (item Lease))))
            (cs_fields
             ((Field (name lc)
               (type_
                ((ft_type (Type_ref ((id_module (randr)) (id_name LeaseNotify))))
                 (ft_allowed ())))))))))))))
     (doc ())) |}]

let unions_to_switch = function
  | Core decls -> Core (xproto_clientmessage_to_switch decls)
  | Extension ({ file_name = n; declarations = decls; _ } as ext)
    when n = "randr" ->
      Extension { ext with declarations = randr_notify_to_switch decls }
  | Extension _ as ext -> ext
