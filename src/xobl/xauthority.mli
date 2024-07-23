(** Functions for handling Xauthority files and entries. *)

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

val select_best :
  family:Family.t ->
  address:string ->
  ?display:int ->
  ?types:string list ->
  entry list ->
  Authorization.t option
(** Find an Authorization entry matching [family], [address] and [display]. *)
