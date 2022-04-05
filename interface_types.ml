[@@@ocaml.warning "-27-30-39"]


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

let rec default_position 
  ?line:((line:int32) = 0l)
  ?col:((col:int32) = 0l)
  ?offset:((offset:int32) = 0l)
  () : position  = {
  line;
  col;
  offset;
}

let rec default_location 
  ?path:((path:string) = "")
  ?start:((start:position option) = None)
  ?end_:((end_:position option) = None)
  ?lines:((lines:string list) = [])
  () : location  = {
  path;
  start;
  end_;
  lines;
}

let rec default_op () = (Plus:op)

let rec default_expr () : expr = Var ("")

and default_bin_op 
  ?lhs:((lhs:expr option) = None)
  ?op:((op:op) = default_op ())
  ?rhs:((rhs:expr option) = None)
  () : bin_op  = {
  lhs;
  op;
  rhs;
}
