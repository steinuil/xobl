open Parsetree

let mark_eventstruct_events_as_serializable xcbs =
  let allowed_events_by_ext_name = Hashtbl.create 2 in
  ListLabels.concat_map xcbs ~f:(function
    | Core declarations -> declarations
    | Extension { declarations; _ } -> declarations)
  |> List.iter (function
       | Event_struct { allowed_events; _ } ->
           ListLabels.iter allowed_events
             ~f:(fun { ae_module; ae_opcode_range; _ } ->
               Hashtbl.add allowed_events_by_ext_name ae_module ae_opcode_range)
       | _ -> ());
  ListLabels.map xcbs ~f:(function
    | Core _ as core -> core
    | Extension
        { name; file_name; query_name; multiword; version; declarations } as ext
      -> (
        match Hashtbl.find_opt allowed_events_by_ext_name name with
        | None -> ext
        | Some { min; max } ->
            let declarations =
              ListLabels.map declarations ~f:(function
                | Event
                    {
                      name;
                      number;
                      is_generic;
                      is_serializable = _;
                      no_sequence_number;
                      fields;
                      doc;
                    }
                  when number >= min && number <= max ->
                    Event
                      {
                        name;
                        number;
                        is_generic;
                        is_serializable = true;
                        no_sequence_number;
                        fields;
                        doc;
                      }
                | d -> d)
            in
            Extension
              { name; file_name; query_name; multiword; version; declarations }))
