let return x = fun () -> Some x

let bind m f = fun () ->
  match m () with
  | None -> None
  | Some x -> f x ()

let fail = fun () -> None

let join m n f = fun () ->
  match (m (), n ()) with
  | (None, None)        -> None
  | (Some mv, None)     -> Some mv
  | (None, Some nv)     -> Some nv
  | (Some mv, Some nv)  -> Some (f mv nv)