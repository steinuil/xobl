(* Reference implementation: https://gitlab.freedesktop.org/xorg/lib/libxcb *)

open Sexplib.Conv

(* Xorg listens on port 6000 + n, where n is the display number.
   https://www.x.org/archive/X11R6.8.0/doc/Xorg.1.html#sect4 *)
let xorg_tcp_port = 6000

type hostname =
  | Unix_domain_socket of string
  | Internet_domain of ([ `Ipv4 | `Ipv6 ] * string * int)
[@@deriving sexp]

type t = { hostname : hostname; display : int; screen : int } [@@deriving sexp]

let default =
  { hostname = Unix_domain_socket "/tmp/.X11-unix/X0"; display = 0; screen = 0 }

let ( let& ) = Option.bind

(* The display name has form [hostname]:displaynumber[.screennumber].
   The parts between brackets can be omitted.
   https://gitlab.freedesktop.org/xorg/lib/libxcb/-/blob/master/src/xcb_util.c
   https://cgit.freedesktop.org/xorg/app/xauth/tree/parsedpy.c
   https://www.x.org/releases/X11R7.7/doc/man/man7/X.7.xhtml#heading5 *)
let parse_name name =
  let& colon = String.rindex_opt name ':' in
  let hostname = String.sub name 0 colon in
  let last_half =
    String.sub name (colon + 1) (String.length name - colon - 1)
  in
  let dot = String.rindex_opt last_half '.' in
  let display, screen =
    match dot with
    | None -> (last_half, None)
    | Some dot ->
        let display = String.sub last_half 0 dot in
        let screen =
          String.sub last_half (dot + 1) (String.length last_half - dot - 1)
        in
        (display, Some screen)
  in
  let& display = int_of_string_opt display in
  match Option.map int_of_string_opt screen with
  | Some None -> None
  | screen_opt -> Some (hostname, display, Option.join screen_opt)

let%test _ = parse_name ":0.1" = Some ("", 0, Some 1)
let%test _ = parse_name "x.org:0" = Some ("x.org", 0, None)
let%test _ = parse_name "[::1]:0" = Some ("[::1]", 0, None)
let%test _ = parse_name "198.112.45.11:0.1" = Some ("198.112.45.11", 0, Some 1)

let%test "DECnet addresses terminate in :: but we don't do DECnet" =
  parse_name "hydra::0.1" = Some ("hydra:", 0, Some 1)

let%test "no colon" = parse_name "ayy lmao" = None
let%test "no display number" = parse_name ":" = None
let%test "invalid display number" = parse_name ":ayy" = None
let%test "invalid screen number" = parse_name "0.ayy" = None

(* Host name parser that tries to be compliant with the libxcb
   implementation. *)
let parse_hostname ~display = function
  | "" | "unix" ->
      Some (Unix_domain_socket ("/tmp/.X11-unix/X" ^ string_of_int display))
  | path when String.length path >= 5 && String.sub path 0 5 = "unix/" ->
      Some (Unix_domain_socket ("/tmp/.X11-unix/X" ^ string_of_int display))
  | hostname -> (
      let port = xorg_tcp_port + display in
      let len = String.length hostname in
      match String.index_opt hostname '/' with
      | None ->
          if len >= 2 && hostname.[0] = '[' && hostname.[len - 1] = ']' then
            let hostname = String.sub hostname 1 (len - 2) in
            Some (Internet_domain (`Ipv6, hostname, port))
          else Some (Internet_domain (`Ipv4, hostname, port))
      | Some slash -> (
          let protocol = String.sub hostname 0 slash in
          let hostname = String.sub hostname (slash + 1) (len - slash - 1) in
          match protocol with
          | "tcp" | "inet" -> Some (Internet_domain (`Ipv4, hostname, port))
          | "inet6" -> Some (Internet_domain (`Ipv6, hostname, port))
          | _ -> None))

(* https://www.x.org/archive/X11R6.8.0/doc/Xorg.1.html#sect5 *)
let%test "unix is a special hostname" =
  parse_hostname "unix" ~display:1
  = Some (Unix_domain_socket "/tmp/.X11-unix/X1")

let%test "empty means unix domain socket" =
  parse_hostname "" ~display:0 = Some (Unix_domain_socket "/tmp/.X11-unix/X0")

(* This is an XGB extension; it'd be relatively harmless to include but we
   already provide a better API for connecting to arbitrary sockets. *)
let%test "custom path for domain socket is invalid" =
  parse_hostname "/tmp/socket" ~display:0 = None

let%test "ipv6 address" =
  parse_hostname "[::1]" ~display:3
  = Some (Internet_domain (`Ipv6, "::1", 6003))

(* The reference implementation does this and it's pretty dumb, but we're
   staying compliant. *)
let%test "if the protocol is unix/ ignore everything past the slash" =
  parse_hostname "unix/ayylmao" ~display:0
  = Some (Unix_domain_socket "/tmp/.X11-unix/X0")

let%test "tcp protocol" =
  parse_hostname "tcp/x.org" ~display:0
  = Some (Internet_domain (`Ipv4, "x.org", 6000))

let%test "inet6 protocol" =
  parse_hostname "inet6/::1" ~display:0
  = Some (Internet_domain (`Ipv6, "::1", 6000))

let%test "should this be invalid?" =
  parse_hostname "tcp/" ~display:0 = Some (Internet_domain (`Ipv4, "", 6000))

let%test "hostname" =
  parse_hostname "megane" ~display:0
  = Some (Internet_domain (`Ipv4, "megane", 6000))

let%test "garbage" = parse_hostname "/" ~display:0 = None
let%test "invalid protocol" = parse_hostname "udp/x.org" ~display:0 = None

let parse name =
  let& hostname, display, screen = parse_name name in
  let& hostname = parse_hostname ~display hostname in
  Some { hostname; display; screen = Option.value ~default:0 screen }
