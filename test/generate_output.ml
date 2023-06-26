let modules =
  [
    "bigreq";
    "composite";
    "damage";
    "dpms";
    "dri2";
    "dri3";
    "ge";
    "glx";
    "present";
    "randr";
    "record";
    "render";
    "res";
    "screensaver";
    "shape";
    "shm";
    "sync";
    "xc_misc";
    (* "xevie"; *)
    "xf86dri";
    "xf86vidmode";
    "xfixes";
    "xinerama";
    "xinput";
    (* "xkb"; *)
    "xprint";
    "xproto";
    "xselinux";
    "xtest";
    "xv";
    "xvmc";
  ]

let parse_module m =
  let fname = Printf.sprintf "../xml-xcb/%s.xml" m in
  In_channel.with_open_text fname (fun f ->
      Xobl_compiler.Parser.parse (`Channel f))
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
  modules |> List.map parse_module |> Xobl_compiler.Hir.of_parsetree
  |> sort_xcbs
  |> Xobl_compiler__.Generate_ocaml.gen stdout
