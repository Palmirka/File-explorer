type path = string * string list
type _ file =
    Regular_file : path * (path -> string list) -> 'a file
  | Directory : path * (path -> 'a file list) -> 'a file
val get_name : 'a file -> string
val get_path : 'a file -> path
val get_content : 'a file -> 'a file list
val temp_dir : string -> 'a file list -> 'a file
val path_to_string : path -> string
val show_preview : 'a file -> string list
val get_dir_content : path -> 'a file list
val confirm : (string -> unit) -> string -> bool -> unit
val create : 'a file -> bool -> string -> int -> 'a file
val remove : path -> bool -> unit
val change_directory : 'a file -> string -> 'a file
val file_info : 'a file -> string list
val copy : 'a file -> string -> unit
val move : 'a file -> string -> unit
val init : unit -> 'a file
type 'a m = unit -> 'a option
val find_files : 'a file -> string -> 'a file list m
