(** interface.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_position : Interface_types.position -> Pbrt.Encoder.t -> unit
(** [encode_position v encoder] encodes [v] with the given [encoder] *)

val encode_location : Interface_types.location -> Pbrt.Encoder.t -> unit
(** [encode_location v encoder] encodes [v] with the given [encoder] *)

val encode_op : Interface_types.op -> Pbrt.Encoder.t -> unit
(** [encode_op v encoder] encodes [v] with the given [encoder] *)

val encode_expr : Interface_types.expr -> Pbrt.Encoder.t -> unit
(** [encode_expr v encoder] encodes [v] with the given [encoder] *)

val encode_bin_op : Interface_types.bin_op -> Pbrt.Encoder.t -> unit
(** [encode_bin_op v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_position : Pbrt.Decoder.t -> Interface_types.position
(** [decode_position decoder] decodes a [position] value from [decoder] *)

val decode_location : Pbrt.Decoder.t -> Interface_types.location
(** [decode_location decoder] decodes a [location] value from [decoder] *)

val decode_op : Pbrt.Decoder.t -> Interface_types.op
(** [decode_op decoder] decodes a [op] value from [decoder] *)

val decode_expr : Pbrt.Decoder.t -> Interface_types.expr
(** [decode_expr decoder] decodes a [expr] value from [decoder] *)

val decode_bin_op : Pbrt.Decoder.t -> Interface_types.bin_op
(** [decode_bin_op decoder] decodes a [bin_op] value from [decoder] *)
