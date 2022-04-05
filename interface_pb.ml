[@@@ocaml.warning "-27-30-39"]

type position_mutable = {
  mutable line : int32;
  mutable col : int32;
  mutable offset : int32;
}

let default_position_mutable () : position_mutable = {
  line = 0l;
  col = 0l;
  offset = 0l;
}

type location_mutable = {
  mutable path : string;
  mutable start : Interface_types.position option;
  mutable end_ : Interface_types.position option;
  mutable lines : string list;
}

let default_location_mutable () : location_mutable = {
  path = "";
  start = None;
  end_ = None;
  lines = [];
}

type bin_op_mutable = {
  mutable lhs : Interface_types.expr option;
  mutable op : Interface_types.op;
  mutable rhs : Interface_types.expr option;
}

let default_bin_op_mutable () : bin_op_mutable = {
  lhs = None;
  op = Interface_types.default_op ();
  rhs = None;
}


let rec decode_position d =
  let v = default_position_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.line <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(position), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.col <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(position), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.offset <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(position), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Interface_types.line = v.line;
    Interface_types.col = v.col;
    Interface_types.offset = v.offset;
  } : Interface_types.position)

let rec decode_location d =
  let v = default_location_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.lines <- List.rev v.lines;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.path <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.start <- Some (decode_position (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.end_ <- Some (decode_position (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.lines <- (Pbrt.Decoder.string d) :: v.lines;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(location), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Interface_types.path = v.path;
    Interface_types.start = v.start;
    Interface_types.end_ = v.end_;
    Interface_types.lines = v.lines;
  } : Interface_types.location)

let rec decode_op d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Interface_types.Plus:Interface_types.op)
  | 1 -> (Interface_types.Minus:Interface_types.op)
  | _ -> Pbrt.Decoder.malformed_variant "op"

let rec decode_expr d = 
  let rec loop () = 
    let ret:Interface_types.expr = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "expr"
      | Some (1, _) -> (Interface_types.Var (Pbrt.Decoder.string d) : Interface_types.expr) 
      | Some (2, _) -> (Interface_types.Lit (Pbrt.Decoder.int32_as_varint d) : Interface_types.expr) 
      | Some (3, _) -> (Interface_types.Bin_op (decode_bin_op (Pbrt.Decoder.nested d)) : Interface_types.expr) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_bin_op d =
  let v = default_bin_op_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.lhs <- Some (decode_expr (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bin_op), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.op <- decode_op d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bin_op), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.rhs <- Some (decode_expr (Pbrt.Decoder.nested d));
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(bin_op), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Interface_types.lhs = v.lhs;
    Interface_types.op = v.op;
    Interface_types.rhs = v.rhs;
  } : Interface_types.bin_op)

let rec encode_position (v:Interface_types.position) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Interface_types.line encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Interface_types.col encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Interface_types.offset encoder;
  ()

let rec encode_location (v:Interface_types.location) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Interface_types.path encoder;
  begin match v.Interface_types.start with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_position x) encoder;
  | None -> ();
  end;
  begin match v.Interface_types.end_ with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_position x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Interface_types.lines;
  ()

let rec encode_op (v:Interface_types.op) encoder =
  match v with
  | Interface_types.Plus -> Pbrt.Encoder.int_as_varint (0) encoder
  | Interface_types.Minus -> Pbrt.Encoder.int_as_varint 1 encoder

let rec encode_expr (v:Interface_types.expr) encoder = 
  begin match v with
  | Interface_types.Var x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Interface_types.Lit x ->
    Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int32_as_varint x encoder;
  | Interface_types.Bin_op x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_bin_op x) encoder;
  end

and encode_bin_op (v:Interface_types.bin_op) encoder = 
  begin match v.Interface_types.lhs with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_op v.Interface_types.op encoder;
  begin match v.Interface_types.rhs with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_expr x) encoder;
  | None -> ();
  end;
  ()
