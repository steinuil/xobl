let parse_module m =
  In_channel.with_open_text m (fun f -> Xobl_compiler.Parser.parse (`Channel f))
  |> Result.get_ok

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
       | Xobl_compiler__Hir.Core _ -> ("xproto", [])
       | Extension { file_name; imports; _ } -> (file_name, imports))
  |> sort_topological
  |> List.map (fun name ->
         xcbs
         |> List.find (function
              | Xobl_compiler__Hir.Core _ when name = "xproto" -> true
              | Extension { file_name; _ } when file_name = name -> true
              | _ -> false))

let () =
  let m = Sys.argv.(1) |> parse_module in
  let xcbs = Xobl_compiler.Hir.of_parsetree [ m ] in
  let xcbs = sort_xcbs xcbs in

  let out = open_out Sys.argv.(2) in
  output_string out "[@@@warning \"-27\"]\n\n";
  output_string out "[@@@warning \"-11\"]\n\n";
  output_string out "open Codec\n";
  Xobl_compiler__.Generate_ocaml.gen out xcbs
