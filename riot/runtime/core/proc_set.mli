open Util

type t

val create : unit -> t
val remove : t -> Process.t Weak_ptr.t -> unit
val contains : t -> Process.t Weak_ptr.t -> bool
val size : t -> int
val add : t -> Process.t Weak_ptr.t -> unit
