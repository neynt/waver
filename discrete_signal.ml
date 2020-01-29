open Base

(* A [Discrete_signal.t] is a sequence of samples taken at a given sample rate. *)
type t = { samples : float array; sample_rate : int } [@@deriving fields]
