open Monad

(* Path is pair of root path and names of dirs on way to root *)

type path = string * string list

type _ file =
  | Regular_file : path * (path -> string list) -> 'a file
  | Directory : path * (path -> 'a file list) -> 'a file

let temp_dir (path : string) (content : 'a file list) : 'a file =
  Directory ((path, []), (fun _a -> content))

let get_name (f : 'a file) : string =
  let path = match f with
    | Regular_file ((home_path, path), content) -> path
    | Directory ((home_path, path) , content) -> path
  in match path with
  | [] -> ""
  | p :: ps -> p 

let get_path (f : 'a file) : path =
  match f with
    | Regular_file (path, content) -> path
    | Directory (path, content) -> path


let get_content (f : 'a file) : 'a file list =
  match f with
    | Directory (path, content) -> content path
    | _ -> []

let home_path (file : 'a file) : string =
  match get_path file with
  | (path, _) -> path

let path_to_string (path : path) : string = 
  let (home_path, path_list) = path in
    home_path ^ (List.fold_right (fun x y -> y ^ "/" ^ x) path_list "") 

let show_preview (f : 'a file) : string list =
  match f with
  | Regular_file (name, content) -> content name
  | Directory (name, content) -> 
    List.map (fun x -> get_name x) (content name)

let read_file (path : path) : string list =
  let input_channel = open_in (path_to_string path) in
    let rec read (res : string list) : string list = 
      try 
        let line = input_line input_channel in
          read (line :: res)
      with End_of_file -> res
    in 
    let content = read [] in
      close_in input_channel;
    content

let rec get_dir_content (path : path) : 'a file list =
  let (home_path, dir_path) = path in 
  let string_path = path_to_string path in
    let content = Array.to_list (Sys.readdir string_path) in
    let to_file (name : string) : 'a file = 
      match Sys.is_directory (string_path ^ "/" ^ name) with
      | false -> Regular_file ((home_path, name :: dir_path), read_file)
      | true -> Directory ((home_path, name :: dir_path), fun x -> get_dir_content x) in
    List.map to_file content

let confirm (f : string -> unit) (s : string) : bool -> unit =
  fun (cond : bool) ->
    if cond then f s
    else () 

let create (parent : 'a file) (is_directory : bool) (name : string) (perm : int) : 'a file =
  let (home_path, parent_path) = get_path parent in
    match is_directory with
    | true -> 
      Sys.mkdir ((path_to_string (get_path parent)) ^ "/" ^ name) perm;
      Directory ((home_path, name :: parent_path), fun _ -> [])
    | false -> 
      close_out (open_out ((path_to_string (get_path parent)) ^ "/" ^ name));
      Regular_file ((home_path, name :: parent_path), fun _ -> [])
  
let rec remove (path : path) : bool -> unit = 
  let string_path = path_to_string path in 
    match Sys.is_directory string_path with
    | false -> confirm Sys.remove string_path
    | true ->
      let rec aux (content : 'a file list) =
        match content with
        | [] -> confirm Sys.rmdir string_path 
        | c :: cs -> 
          let filepath = (get_path c) in
            fun confirmed -> remove filepath confirmed; aux cs confirmed
      in
      aux (get_dir_content path)

let change_directory (directory : 'a file) (new_destination : string) : 'a file =
  match directory with
  | Regular_file (_, _) -> directory
  | Directory (name, content) -> 
    match List.filter (fun n -> (get_name n) = new_destination) (content name) with
    | [] -> directory
    | x :: xs -> x

let shell_command : string -> int =
  Sys.command

let string_of_fk (fk : Unix.file_kind) : string =
  match fk with
  |S_REG -> "S_REG"
  |S_DIR -> "S_DIR"
  |S_CHR -> "S_CHR"
  |S_BLK -> "S_BLK"
  |S_LNK -> "S_LNK"
  |S_FIFO -> "S_FIFO"
  |S_SOCK -> "S_SOCK"

let format_stats (stats : Unix.stats) : string list=
  ["Dev: "  ^ string_of_int stats.st_dev;     
  "Inode: " ^ string_of_int stats.st_ino;      
  "Kind: "  ^ string_of_fk stats.st_kind;
  "Perm: "  ^ string_of_int stats.st_perm;
  "Nlink: " ^ string_of_int stats.st_nlink;
  "UID: "   ^ string_of_int stats.st_uid;
  "GID: "   ^ string_of_int stats.st_gid;
  "Size: "  ^ string_of_int stats.st_size;
  "Atime: " ^ string_of_float stats.st_atime;
  "Mtime: " ^ string_of_float stats.st_mtime;
  "Ctime: " ^ string_of_float stats.st_ctime]

let file_info (file : 'a file) : string list =
  let stats = Unix.stat (path_to_string (get_path file)) in
    format_stats stats

let rec copy (file : 'a file) (dest : string) : unit =
    match String.starts_with ~prefix:(home_path file) dest with
    | false -> ()
    | true -> 
      let string_path = dest ^ "/" ^ get_name file in
      let src_stat = Unix.stat (path_to_string (get_path file)) in
      let mode = src_stat.Unix.st_perm in
        match file with
        | Regular_file (path, content) -> 
          let f = open_out string_path in
            output_string f (List.fold_left (fun x acc -> acc ^ "\n" ^ x) "" (content path));
            Unix.chmod string_path mode;
            close_out f
        | Directory (path, content) -> 
          Sys.mkdir string_path mode;
          let content_files = (content path) in
          List.iter (fun i -> copy i (dest ^ "/" ^ (get_name file))) content_files

let move (file : 'a file) (dest : string) : unit =
  copy file dest;
  remove (get_path file) true

let init () : 'a file =
  Directory ((Sys.getcwd(), []), fun p -> get_dir_content p)

type 'a m = unit -> 'a option

let is_substring word text =
  let len = String.length word in
  let rec check i =
    if i + len > String.length text then false
    else
      let sub = String.sub text i len in
      if sub = word then true
      else check (i + 1)
  in check 0

let rec find_files (file : 'a file) (name : string) : 'a file list m = 
  match file with
  | Regular_file (_, _) ->
    if is_substring name (get_name file) then 
      Monad.return [file] else Monad.fail
  | Directory (path, content) ->
    let f = if is_substring name (get_name file) then 
      Monad.return [file] else Monad.fail in
    let content = get_dir_content (get_path file) in
      List.fold_left 
      (fun x y -> Monad.join x (find_files y name) (fun x y -> x @ y)) 
      f
      content


