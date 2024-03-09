val return : 'a -> unit -> 'a option
val bind :
  (unit -> 'a option) -> ('a -> unit -> 'b option) -> unit -> 'b option
val fail : unit -> 'a option
val join : (unit -> 'a option) -> (unit -> 'a option) -> ('a -> 'a -> 'a) -> unit -> 'a option