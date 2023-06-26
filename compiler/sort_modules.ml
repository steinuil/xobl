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

let sort_hir xcbs =
  xcbs
  |> List.map (function
       | Hir.Core _ -> ("xproto", [])
       | Extension { file_name; imports; _ } -> (file_name, imports))
  |> sort_topological
  |> List.map (fun name ->
         xcbs
         |> List.find (function
              | Hir.Core _ when name = "xproto" -> true
              | Extension { file_name; _ } when file_name = name -> true
              | _ -> false))
