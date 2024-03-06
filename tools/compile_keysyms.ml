open ContainersLabels

type unicode_kind = Specific | Unspecified

let unicode_kind_to_yojson = function
  | Specific -> `String "SPECIFIC"
  | Unspecified -> `String "UNSPECIFIED"

type unicode_info = { code : string; name : string; kind : unicode_kind }
[@@deriving to_yojson]

type alias_info = { name : string; info : string option } [@@deriving to_yojson]

module Parser = struct
  let ( let& ) = Option.bind
  let skip_whitespace = String.drop_while ~f:(Char.equal ' ')
  let rskip_whitespace = String.rdrop_while ~f:(Char.equal ' ')
  let strip = Fun.compose skip_whitespace rskip_whitespace

  let chop ~l ~r str =
    let& str = String.chop_prefix ~pre:l str in
    String.chop_suffix ~suf:r str

  type keysym_def = {
    name : string;
    keysym : int;
    unicode_char : unicode_info option;
    alias_for : alias_info option;
    is_deprecated : bool;
    is_canonical : bool;
    explicitly_non_deprecated : bool;
    comment : string option;
  }
  [@@deriving to_yojson]

  let parse_line line =
    let& rest = String.chop_prefix ~pre:"#define XK_" line in
    let& name, rest = String.Split.left ~by:" " rest in
    let rest = skip_whitespace rest in
    let code, rest =
      String.Split.left ~by:" " rest |> Option.value ~default:(rest, "")
    in
    let keysym = Int.of_string_exn code in
    let rest = strip rest in
    let comment =
      let& rest = chop ~l:"/*" ~r:"*/" rest in
      let rest = strip rest in
      Some rest
    in
    match comment with
    | None ->
        Some
          {
            name;
            keysym;
            unicode_char = None;
            alias_for = None;
            is_deprecated = false;
            is_canonical = false;
            explicitly_non_deprecated = false;
            comment = None;
          }
    | Some comment ->
        let is_deprecated, comment =
          match String.chop_prefix ~pre:"deprecated" comment with
          | Some rest -> (true, strip rest)
          | None -> (false, comment)
        in
        let explicitly_non_deprecated, comment =
          match String.chop_prefix ~pre:"non-deprecated" comment with
          | Some rest -> (true, strip rest)
          | None -> (false, comment)
        in
        let alias_for, comment =
          match String.Split.left ~by:"alias for " comment with
          | None -> (None, comment)
          | Some (before, after) ->
              let alias_for, before =
                match String.Split.left ~by:" " after with
                | None -> (Some { name = after; info = None }, before)
                | Some (alias_name, after) ->
                    let info =
                      chop ~l:"(" ~r:")" after |> Option.get_exn_or "alias info"
                    in
                    (Some { name = alias_name; info = Some info }, before)
              in
              let before = strip before in
              (alias_for, before)
        in
        let unicode_char, is_deprecated, comment =
          if String.starts_with ~prefix:"<U+" comment then
            let unicode =
              chop ~l:"<" ~r:">" comment |> Option.get_exn_or "specific unicode"
            in
            let code, name = String.Split.left_exn ~by:" " unicode in
            (Some { code; name; kind = Specific }, false, None)
          else if String.starts_with ~prefix:"(U+" comment then
            let unicode =
              chop ~l:"(" ~r:")" comment
              |> Option.get_exn_or "deprecated unicode"
            in
            let code, name = String.Split.left_exn ~by:" " unicode in
            (Some { code; name; kind = Unspecified }, true, None)
          else if String.starts_with ~prefix:"U+" comment then
            let unicode = comment in
            let code, name = String.Split.left_exn ~by:" " unicode in
            (Some { code; name; kind = Unspecified }, is_deprecated, None)
          else
            let comment =
              if String.(comment = "") then None else Some comment
            in
            (None, is_deprecated, comment)
        in

        Some
          {
            name;
            keysym;
            is_deprecated;
            unicode_char;
            is_canonical = false;
            explicitly_non_deprecated;
            alias_for;
            comment;
          }

  let line_dispenser ch =
    let rec loop () =
      match In_channel.input_line ch with
      | None -> None
      | Some line -> (
          match parse_line line with
          | Some keysym -> Some keysym
          | None -> loop ())
    in
    loop

  let parse_lines ch = Seq.of_dispenser (line_dispenser ch) |> Seq.to_list
end

type status = Canonical | Alias of alias_info

let status_to_yojson : status -> Yojson.Safe.t = function
  | Canonical -> `Assoc [ ("type", `String "CANONICAL") ]
  | Alias { name; info } ->
      `Assoc
        [
          ("type", `String "ALIAS");
          ("name", `String name);
          ("info", match info with Some info -> `String info | None -> `Null);
        ]

type keysym_def = {
  name : string;
  keysym : int;
  unicode_char : unicode_info option;
  status : status;
  is_deprecated : bool;
  comment : string option;
}
[@@deriving to_yojson]

let keysym_def ~status ~is_deprecated
    Parser.{ name; keysym; unicode_char; comment; _ } =
  { name; keysym; unicode_char; comment; status; is_deprecated }

let resolve_single_keysym by_code k =
  let aliases =
    Hashtbl.get by_code k.Parser.keysym |> Option.get_exn_or "keysym"
  in
  match aliases with
  | [ _ ] -> keysym_def k ~status:Canonical ~is_deprecated:k.is_deprecated
  | _ ->
      let deprecated =
        List.count aliases ~f:(fun alias -> alias.Parser.is_deprecated)
      in
      let have_alias_for =
        List.count aliases ~f:(fun alias ->
            Option.is_some alias.Parser.alias_for)
      in
      if deprecated = List.length aliases - 1 then
        (* If all other aliases are deprecated,
           The only non-deprecated one is canonical. *)
        let non_deprecated =
          List.find aliases ~f:(fun alias -> not alias.Parser.is_deprecated)
        in
        if String.(non_deprecated.name = k.name) then
          keysym_def k ~status:Canonical ~is_deprecated:false
        else
          let alias =
            Option.value k.alias_for
              ~default:{ name = non_deprecated.name; info = None }
          in
          keysym_def k ~status:(Alias alias) ~is_deprecated:true
      else if have_alias_for = List.length aliases - 1 then
        (* If all other aliases are explicitly marked as alias,
           the only one not marked as alias is canonical. *)
        let non_alias =
          List.find aliases ~f:(fun alias ->
              Option.is_none alias.Parser.alias_for)
        in
        if String.(non_alias.name = k.name) then
          keysym_def k ~status:Canonical ~is_deprecated:k.is_deprecated
        else
          (* If the alias is not explicitly marked as non-deprecated,
             it is implicitly deprecated. *)
          keysym_def k
            ~status:(Alias (k.alias_for |> Option.get_exn_or "alias_for"))
            ~is_deprecated:(not k.explicitly_non_deprecated)
      else
        (* Apparently the list in Hashtbl is reversed. *)
        let first = List.last_opt aliases |> Option.get_exn_or "last_opt" in
        (* If all else fails, consider the first defined alias
           as canonical. *)
        if String.(first.name = k.name) then
          keysym_def k ~status:Canonical ~is_deprecated:k.is_deprecated
        else
          let alias =
            Option.value k.alias_for ~default:{ name = first.name; info = None }
          in
          keysym_def k ~status:(Alias alias)
            ~is_deprecated:(not k.explicitly_non_deprecated)

let resolve_all_keysyms keysyms =
  let by_code = Hashtbl.create 2200 in
  List.iter keysyms ~f:(fun keysym ->
      Hashtbl.add_list by_code keysym.Parser.keysym keysym);
  List.map keysyms ~f:(resolve_single_keysym by_code)

let () =
  let keysyms = In_channel.with_open_text Sys.argv.(1) Parser.parse_lines in
  let keysyms = resolve_all_keysyms keysyms in
  List.iter keysyms ~f:(fun keysym ->
      keysym_def_to_yojson keysym |> Yojson.Safe.to_string |> print_endline)
