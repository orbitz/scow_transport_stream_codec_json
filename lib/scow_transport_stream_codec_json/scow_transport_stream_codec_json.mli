module type ELT = sig
  type t

  val to_string : t -> string
  val of_string : string -> t option
end

module Make : functor (Elt : ELT) -> sig
  type elt = Elt.t

  val to_string : elt Scow_transport_stream_codec.Msg.t -> string
  val of_string : string -> elt Scow_transport_stream_codec.Msg.t option
end
