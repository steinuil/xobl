val get_path : unit -> string option

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

val get_best :
  family:Family.t ->
  address:string ->
  display:int option ->
  ?types:string list ->
  entry list ->
  (string * string) option
