val path : unit -> string option
(** Get the path of the .Xauthority file from (in order of precedence):
    - $XAUTHORITY
    - $HOME/.Xauthority
    - /Users/$USERNAME/.Xauthority (when on Windows) *)

module Family : sig
  type t = Local | Wild | Netname | Krb5_principal | Local_host
end

type entry = {
  xau_family : Family.t;
  xau_address : string;
  xau_dpynum : int option;
  xau_type : string;
  xau_data : string;
}

val entries_from_string : string -> entry list
val entries_from_file : string -> entry list

type auth = { auth_name : string; auth_data : string }

val select_best :
  family:Family.t ->
  address:string ->
  display:int option ->
  ?types:string list ->
  entry list ->
  auth option
(** Find an authentication entry matching [family], [address] and [display]. *)
