val path_from_env : unit -> string option
(** Get the path of the .Xauthority file from (in order of precedence):
    - $XAUTHORITY
    - $HOME/.Xauthority
    - /Users/$USERNAME/.Xauthority (when on Windows) *)

module Family : sig
  type t =
    | Internet
    | Decnet
    | Chaos
    | Server_interpreted
    | Internet6
    | Local
    | Netname
    | Krb5_principal
    | Local_host
    | Wild
end

type entry = {
  xau_family : Family.t;
  xau_address : string;
  xau_dpynum : int option;
  xau_type : string;
  xau_data : string;
}

val parse : string -> entry list

type auth = { auth_name : string; auth_data : string }

val default_auth : auth

val select_best :
  family:Family.t ->
  address:string ->
  display:int option ->
  ?types:string list ->
  entry list ->
  auth option
(** Find an authentication entry matching [family], [address] and [display]. *)
