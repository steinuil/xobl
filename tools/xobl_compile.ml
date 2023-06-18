let parse_module m =
  let f = open_in m in
  try
    let m = Xobl_compiler.Parser.parse (`Channel f) |> Result.get_ok in
    close_in f;
    m
  with exn ->
    close_in f;
    raise exn

let sort_topological (nodes : (string * string list) list) =
  let rec dfs out (node, dependencies) =
    if List.mem node out then out
    else
      node
      :: List.fold_left
           (fun out node ->
             let dependencies = List.assoc node nodes in
             dfs out (node, dependencies))
           out dependencies
  in
  List.fold_left dfs [] nodes |> List.rev

let sort_xcbs xcbs =
  xcbs
  |> List.map (function
       | Xobl_compiler__Elaboratetree.Core _ -> ("xproto", [])
       | Extension { file_name; imports; _ } -> (file_name, imports))
  |> sort_topological
  |> List.map (fun name ->
         xcbs
         |> List.find (function
              | Xobl_compiler__Elaboratetree.Core _ when name = "xproto" -> true
              | Extension { file_name; _ } when file_name = name -> true
              | _ -> false))

let () =
  let m = Sys.argv.(1) |> parse_module in
  let x = Xobl_compiler.Elaborate.unions_to_switches m in
  let xcb = Xobl_compiler.Elaborate.resolve_idents [ x ] in
  let xcb = List.map (Xobl_compiler.Elaborate.do_stuff xcb) xcb in
  let xcb = sort_xcbs xcb in

  let out = open_out Sys.argv.(2) in
  output_string out "[@@@warning \"-27\"]\n\n";
  output_string out "[@@@warning \"-11\"]\n\n";
  output_string out "open Codec\n\n";
  Xobl_compiler__.Generate_ocaml.gen out xcb
