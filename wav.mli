open Base

val save : ?bit_depth:int -> sampling_rate:int -> string -> Signal.t list -> unit
val load : string -> Discrete_signal.t list
