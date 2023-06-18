(* Reference implementation: https://gitlab.freedesktop.org/xorg/lib/libxau *)

open Sexplib.Conv

let xauth_path_from_env () = Sys.getenv_opt "XAUTHORITY"

let xauth_path_from_unix_home () =
  Sys.getenv_opt "HOME"
  |> Option.map (fun home -> Filename.concat home ".Xauthority")

let xauth_path_from_windows_home () =
  Sys.getenv_opt "USERNAME"
  |> Option.map (fun username ->
         Filename.concat (Filename.concat "Users" username) ".Xauthority")

let get_path () =
  match xauth_path_from_env () with
  | Some path -> Some path
  | None -> (
      match xauth_path_from_unix_home () with
      | Some path -> Some path
      | None when Sys.win32 || Sys.cygwin -> xauth_path_from_windows_home ()
      | None -> None)

module Family = struct
  type t = Local | Wild | Netname | Krb5_principal | Local_host
  [@@deriving sexp]

  let of_int = function
    | 256 -> Local
    | 254 -> Netname
    | 253 -> Krb5_principal
    | 252 -> Local_host
    | 0xFFFF -> Wild
    | _ -> failwith "Unknown family"
end

type entry = {
  xau_family : Family.t;
  xau_address : string;
  xau_dpynum : int option;
  xau_type : string;
  xau_data : string;
}
[@@deriving sexp]

type cursor = { data : string; mutable pos : int }

let cursor_is_empty { data; pos } = pos >= String.length data

let read_byte inp =
  let byte = inp.data.[inp.pos] in
  inp.pos <- inp.pos + 1;
  Char.code byte

let read_string inp len =
  try
    let str = String.sub inp.data inp.pos len in
    inp.pos <- inp.pos + len;
    str
  with Invalid_argument _ -> raise End_of_file

let read_uint16_be inp =
  let b0 = read_byte inp in
  let b1 = read_byte inp in
  (b0 lsl 8) lor b1

(** Read length-prefixed string (Pascal string).
      length : uint16 big-endian *)
let read_pstring inp =
  let len = read_uint16_be inp in
  read_string inp len

(* Entry format:
   family       : uint16
   address_len  : uint16
   address      : char[address_len]
   dpynum_len   : uint16
   dpynum       : char[dpynum_len]
   type_len     : uint16
   type         : char[type_len]
   data_len     : uint16
   data         : char[data_len] *)
let read_entry inp =
  let xau_family = read_uint16_be inp |> Family.of_int in
  let xau_address = read_pstring inp in
  let xau_dpynum =
    let len = read_uint16_be inp in
    if len = 0 then None else Some (read_string inp len |> int_of_string)
  in
  let xau_type = read_pstring inp in
  let xau_data = read_pstring inp in
  { xau_family; xau_address; xau_dpynum; xau_type; xau_data }

let rec read_all_entries stream acc =
  let acc = read_entry stream :: acc in
  if cursor_is_empty stream then List.rev acc else read_all_entries stream acc

let read_all_entries stream = read_all_entries stream []
let entries_from_string data = read_all_entries { data; pos = 0 }

let entries_from_file path =
  let data = In_channel.with_open_bin path In_channel.input_all in
  read_all_entries { data; pos = 0 }

let%expect_test _ =
  let data =
    "\x01\x00\x00\tsick-hack\x00\x00\x00\x12MIT-MAGIC-COOKIE-1\x00\x10?\xF65iW\xE7?\xE8\xB0%\x11kcu\xC6\x90\xFF\xFF\x00\tsick-hack\x00\x00\x00\x12MIT-MAGIC-COOKIE-1\x00\x10?\xF65iW\xE7?\xE8\xB0%\x11kcu\xC6\x90"
  in
  let auth = entries_from_string data in
  print_string @@ Sexplib.Sexp.to_string_hum @@ sexp_of_list sexp_of_entry auth;
  [%expect
    {|
    (((xau_family Local) (xau_address sick-hack) (xau_dpynum ())
      (xau_type MIT-MAGIC-COOKIE-1)
      (xau_data "?\2465iW\231?\232\176%\017kcu\198\144"))
     ((xau_family Wild) (xau_address sick-hack) (xau_dpynum ())
      (xau_type MIT-MAGIC-COOKIE-1)
      (xau_data "?\2465iW\231?\232\176%\017kcu\198\144"))) |}]

let rec find_some ~f = function
  | [] -> None
  | head :: tail -> (
      match f head with Some v -> Some v | None -> find_some ~f tail)

(** Find an authentication entry matching [family], [address] and [display]. *)
let get_best ~family ~address ~display ?(types = [ "MIT-MAGIC-COOKIE-1" ])
    entries =
  assert (List.length types > 0);
  let matches =
    List.filter
      (fun { xau_family; xau_address; xau_dpynum; xau_type; _ } ->
        (* Match when:
             either family or entry->family are FamilyWild or
              family and entry->family are the same and
               address and entry->address are the same
            and
             either number or entry->number are empty or
              number and entry->number are the same
            and
             either name or entry->name are empty or
              name and entry->name are the same *)
        (family = Family.Wild || xau_family = Family.Wild
        || (family = xau_family && address = xau_address))
        && display = xau_dpynum && List.mem xau_type types)
      entries
  in
  find_some types ~f:(fun typ ->
      find_some matches ~f:(fun { xau_type; xau_data; _ } ->
          if typ = xau_type then Some (xau_type, xau_data) else None))
