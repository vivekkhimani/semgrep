(** interface.proto YoJSON Encoding *)


(** {2 Protobuf YoJson Encoding} *)

val encode_position : Interface_types.position -> Yojson.Basic.json
(** [encode_position v encoder] encodes [v] to to json*)

val encode_location : Interface_types.location -> Yojson.Basic.json
(** [encode_location v encoder] encodes [v] to to json*)

val encode_op : Interface_types.op -> Yojson.Basic.json
(** [encode_op v encoder] encodes [v] to to json*)

val encode_expr : Interface_types.expr -> Yojson.Basic.json
(** [encode_expr v encoder] encodes [v] to to json*)

val encode_bin_op : Interface_types.bin_op -> Yojson.Basic.json
(** [encode_bin_op v encoder] encodes [v] to to json*)


(** {2 JSON Decoding} *)

val decode_position : Yojson.Basic.json -> Interface_types.position
(** [decode_position decoder] decodes a [position] value from [decoder] *)

val decode_location : Yojson.Basic.json -> Interface_types.location
(** [decode_location decoder] decodes a [location] value from [decoder] *)

val decode_op : Yojson.Basic.json -> Interface_types.op
(** [decode_op decoder] decodes a [op] value from [decoder] *)

val decode_expr : Yojson.Basic.json -> Interface_types.expr
(** [decode_expr decoder] decodes a [expr] value from [decoder] *)

val decode_bin_op : Yojson.Basic.json -> Interface_types.bin_op
(** [decode_bin_op decoder] decodes a [bin_op] value from [decoder] *)
