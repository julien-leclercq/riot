type t

val main : t
val zero : t
val equal : t -> t -> bool
val last : unit -> t
val next : unit -> t
val pp : Format.formatter -> t -> unit
val reset : unit -> unit
val compare : t -> t -> int
val hash : t -> int
val to_int64 : t -> int64
val of_int64 : int64 -> t

module Map : Util.Dashmap.Intf with type key = t
