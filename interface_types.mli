(** interface.proto Types *)



(** {2 Types} *)

type position = {
  line : int32;
  col : int32;
  offset : int32;
}

type location = {
  path : string;
  start : position option;
  end_ : position option;
  lines : string list;
}

type op =
  | Plus 
  | Minus 

type expr =
  | Var of string
  | Lit of int32
  | Bin_op of bin_op

and bin_op = {
  lhs : expr option;
  op : op;
  rhs : expr option;
}


(** {2 Default values} *)

val default_position : 
  ?line:int32 ->
  ?col:int32 ->
  ?offset:int32 ->
  unit ->
  position
(** [default_position ()] is the default value for type [position] *)

val default_location : 
  ?path:string ->
  ?start:position option ->
  ?end_:position option ->
  ?lines:string list ->
  unit ->
  location
(** [default_location ()] is the default value for type [location] *)

val default_op : unit -> op
(** [default_op ()] is the default value for type [op] *)

val default_expr : unit -> expr
(** [default_expr ()] is the default value for type [expr] *)

val default_bin_op : 
  ?lhs:expr option ->
  ?op:op ->
  ?rhs:expr option ->
  unit ->
  bin_op
(** [default_bin_op ()] is the default value for type [bin_op] *)
