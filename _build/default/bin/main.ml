open Explorer
open Notty
open Notty_unix

let dir_to_img (dir : 'a Explorer.file) (marked : int) : image =
  let dir_path = Explorer.get_path dir in
  let home_path = I.string A.(fg lightred) (Explorer.path_to_string dir_path) in
  let content = Explorer.get_content dir in
  let rec aux acc content marked =
    match content with
    | [] -> acc
    | c :: cs -> 
      if marked = 0 then aux (I.(<->) acc (I.string A.(bg lightblack) (Explorer.get_name c))) cs (marked - 1)
      else aux (I.(<->) acc (I.string A.empty (Explorer.get_name c))) cs (marked - 1)
    in aux home_path content marked

let stringlist_to_img (dir : 'a Explorer.file) (f : 'a Explorer.file -> string list): image =
  let data : string list = f dir in 
  let base_img : image = I.string A.(fg lightred) (Explorer.path_to_string (Explorer.get_path dir)) in
  let rec aux (lines : string list) (acc : image): image =
    match lines with
    | [] -> acc
    | l :: ls -> aux ls (I.(<->) acc (I.string A.empty l))
  in aux data base_img

let rec new_view (t : Term.t) (img : image) : unit =
  Term.image t img;
  match Term.event t with
  | `Key (`Escape, _) -> ()
  | _ -> new_view t img

let rec confirmation (t : Term.t) : bool =
  let img = I.string A.empty "Are you sure you want to delete this file? [y/n]" in
    Term.image t img;
    match Term.event t with
    | `Key (`ASCII 'y', _) -> true
    | `Key (`ASCII 'n', _) -> false
    | _ -> confirmation t

let info : image =
  let data : image list =
    [I.string A.(fg lightyellow) "Avalaible operations: ";
     I.string A.empty            "-b: back to parent directory";
     I.string A.empty            "-c: copy file";
     I.string A.empty            "-f: find files with prefix";
     I.string A.empty            "-h: help";
     I.string A.empty            "-c: move file";
     I.string A.empty            "-n: create new file";
     I.string A.empty            "-p: show preview";
     I.string A.empty            "-r: remove file";
     I.string A.empty            "-Esc: quit explorer"] 
  in I.vcat data

let copy_view (t : Term.t) : string option =
  let base_text = "Input: " in
  let rec aux (t : Term.t) (path : string) : string option =
    match Term.event t with
    | `Key (`Escape, _)     -> None
    | `Key (`Enter, _)      -> Some path
    | `Key (`Backspace, _)  -> 
      let new_path = String.sub path 0 ((String.length path) - 1) in
      Term.image t (I.string A.empty (base_text ^ new_path));
      aux t new_path
    | `Key (`ASCII l,_)     -> 
      let new_path = path ^ (Char.escaped l) in
      Term.image t (I.string A.empty (base_text ^ new_path));
      aux t new_path
    | _                     -> aux t path
  in 
  Term.image t (I.string A.empty base_text);
  aux t ""

type data = (string * string * string)
let new_file_view t =
  let text = 
    ("Is directory? [y/n] : ",
    "Name : ",
    "Permissions : ") in
  let rec create_view (t : Term.t) (text : data) (data : data) (pointer : int) (x : string) : data option =
    let (is_dir, name, perm) = data in
    let data = begin match pointer with
    | 0 -> (is_dir ^ x, name, perm)
    | 1 -> (is_dir, name ^ x, perm)
    | 2 -> (is_dir, name, perm ^ x)
    | _ -> (is_dir, name, perm) 
    end in
    let base_img = I.string A.(fg lightyellow) "Create new file" in
    let img = match (text, data) with
    | ((t1, t2, t3), (d1, d2, d3)) -> 
      I.(base_img <-> 
         (I.string A.empty (t1 ^ d1)) <->
         (I.string A.empty (t2 ^ d2)) <->
         (I.string A.empty (t3 ^ d3)))
    in
      Term.image t img; 
      change_input t text data pointer
  and change_input (t : Term.t) (text : data) (data : data) (pointer : int) : data option =
    begin match Term.event t with
    | `Key (`Arrow `Down,_) -> create_view t text data ((pointer + 1) mod 3) ""
    | `Key (`Arrow `Up,_)   -> create_view t text data ((pointer - 1) mod 3) ""
    | `Key (`ASCII l,_)     -> create_view t text data pointer (Char.escaped l)
    | `Key (`Enter, _)      -> Some data
    | `Key (`Escape, _)     -> None
    | _                     -> create_view t text data pointer ""
    end 
  in
  create_view t text ("", "", "") 0 ""

let () =
  let root_dir = Explorer.init() in 
  let current_dir = Explorer.init() in
  let terminal = Term.create ~dispose:false () in
  let content = ([], Explorer.get_dir_content (Explorer.get_path current_dir)) in
  let img = dir_to_img current_dir 0 in
  let finish = (fun t -> Term.release t) in
  
  let rec update (t : Term.t) (current_dir : 'a Explorer.file) (ancestors : 'a Explorer.file list) pointer (img : image) (marked : int) : unit = 
    Term.image terminal img;
    loop t current_dir ancestors pointer img marked
    
  and loop (t : Term.t) (current_dir : 'a Explorer.file) (ancestors : 'a Explorer.file list) (before, after) (img : image) (marked : int): unit =
    match Term.event t with
    | `Key (`Enter,_)       -> 
      begin match after with
        | [] -> 
          update t current_dir ancestors (before, after) img marked
        | marked_file :: rest -> 
          begin match marked_file with
            | Explorer.Regular_file (_, _) -> update t current_dir ancestors (before, after) img marked
            | Explorer.Directory (path, content) ->
              update t marked_file (current_dir :: ancestors) ([], Explorer.get_dir_content path) (dir_to_img marked_file 0) 0
            end
        end
    | `Key (`Arrow `Up,_)   -> 
      begin match before with
        | curr :: rest -> 
          update t current_dir ancestors (rest, curr :: after) (dir_to_img current_dir (marked - 1)) (marked - 1)
        | _ ->
          update t current_dir ancestors (before, after) img marked
      end
    | `Key (`Arrow `Down,_) -> 
      begin match after with
        | next :: nnext :: rest -> 
          update t current_dir ancestors (next :: before, nnext :: rest) (dir_to_img current_dir (marked + 1)) (marked + 1)
        | _ ->
          update t current_dir ancestors (before, after) img marked
        end
    | `Key (`Escape, _)  -> finish t
    | `Key (`ASCII 'p', _)  -> 
      begin match after with
        | [] -> 
          update t current_dir ancestors (before, after) img marked
        | dir :: rest -> 
          new_view t (stringlist_to_img dir Explorer.show_preview);
          update t current_dir ancestors (before, after) img marked
        end
    | `Key (`ASCII 'i', _)  -> 
      begin match after with
        | [] -> 
          update t current_dir ancestors (before, after) img marked
        | dir :: rest -> 
          new_view t (stringlist_to_img dir Explorer.file_info);
          update t current_dir ancestors (before, after) img marked
        end
    | `Key (`ASCII 'b', _)  ->
      begin match ancestors with
        | [] -> 
          update t current_dir ancestors (before, after) img marked
        | parent :: rest ->
          update t parent rest ([], Explorer.get_dir_content (Explorer.get_path parent)) (dir_to_img parent 0) 0
        end
    | `Key (`ASCII 'r', _)  ->
      begin match after with
        | [] -> 
          update t current_dir ancestors (before, after) img marked
        | dir :: rest -> 
          Explorer.remove (Explorer.get_path dir) (confirmation t);
          update t current_dir ancestors ([], Explorer.get_dir_content (Explorer.get_path current_dir)) (dir_to_img current_dir 0) 0
        end
    | `Key (`ASCII 'f', _)  ->
      let x = copy_view t in
      begin match x with
        | Some pattern -> 
          let content = Explorer.find_files root_dir pattern in
          let content = match content () with
            | None -> []
            | Some c -> c in
          let temp_dir = Explorer.temp_dir pattern content in
          print_endline (get_name temp_dir);
          update t temp_dir (current_dir :: ancestors) ([], content) (dir_to_img temp_dir 0) 0
        | None                        -> 
          update t current_dir ancestors (before, after) img marked
        end
    | `Key (`ASCII 'c', _)  ->
      print_endline (get_name current_dir);
      begin match (after, copy_view t) with
        | (dir :: rest, Some path) -> 
          print_endline (get_name current_dir);
          Explorer.copy dir path;
          update t current_dir ancestors (before, after) img marked
        | _                        -> 
          update t current_dir ancestors (before, after) img marked
        end
    | `Key (`ASCII 'm', _)  ->
      begin match (after, copy_view t) with
        | (dir :: rest, Some path) -> 
          Explorer.move dir path;
          update t current_dir ancestors (before, after) img marked
        | _                        -> 
          update t current_dir ancestors (before, after) img marked
        end
    
    | `Key (`ASCII 'n', _)  ->
      begin match new_file_view t with
      | None -> update t current_dir ancestors (before, after) img marked
      | Some value -> 
        let (is_dir, name, perm) = value in 
        let is_dir = begin match is_dir with
          | "y" -> true
          | _ -> false
          end in
        let perm = try int_of_string perm 
          with _ -> 0o777 in
        let _ = Explorer.create current_dir is_dir name perm in
          update t current_dir ancestors ([], Explorer.get_dir_content (Explorer.get_path current_dir)) (dir_to_img current_dir 0) 0
        end
    | `Key (`ASCII 'h', _)  ->
      new_view t info;
      update t current_dir ancestors (before, after) img marked
    | _                     -> update t current_dir ancestors (before, after) img marked 
  in
  Term.image terminal img;
  loop terminal current_dir [] content img 0
  