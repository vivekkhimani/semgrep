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
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("line", json_value) -> 
      v.line <- Pbrt_yojson.int32 json_value "position" "line"
    | ("col", json_value) -> 
      v.col <- Pbrt_yojson.int32 json_value "position" "col"
    | ("offset", json_value) -> 
      v.offset <- Pbrt_yojson.int32 json_value "position" "offset"
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    Interface_types.line = v.line;
    Interface_types.col = v.col;
    Interface_types.offset = v.offset;
  } : Interface_types.position)

let rec decode_location d =
  let v = default_location_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("path", json_value) -> 
      v.path <- Pbrt_yojson.string json_value "location" "path"
    | ("start", json_value) -> 
      v.start <- Some ((decode_position json_value))
    | ("end", json_value) -> 
      v.end_ <- Some ((decode_position json_value))
    | ("lines", `List l) -> begin
      v.lines <- List.map (function
        | json_value -> Pbrt_yojson.string json_value "location" "lines"
      ) l;
    end
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    Interface_types.path = v.path;
    Interface_types.start = v.start;
    Interface_types.end_ = v.end_;
    Interface_types.lines = v.lines;
  } : Interface_types.location)

let rec decode_op json =
  match json with
  | `String "Plus" -> (Interface_types.Plus : Interface_types.op)
  | `String "Minus" -> (Interface_types.Minus : Interface_types.op)
  | _ -> Pbrt_yojson.E.malformed_variant "op"

let rec decode_expr json =
  let assoc = match json with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  let rec loop = function
    | [] -> Pbrt_yojson.E.malformed_variant "expr"
    | ("var", json_value)::_ -> 
      (Interface_types.Var (Pbrt_yojson.string json_value "expr" "Var") : Interface_types.expr)
    | ("lit", json_value)::_ -> 
      (Interface_types.Lit (Pbrt_yojson.int32 json_value "expr" "Lit") : Interface_types.expr)
    | ("binOp", json_value)::_ -> 
      (Interface_types.Bin_op ((decode_bin_op json_value)) : Interface_types.expr)
    
    | _ :: tl -> loop tl
  in
  loop assoc

and decode_bin_op d =
  let v = default_bin_op_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function 
    | ("lhs", json_value) -> 
      v.lhs <- Some ((decode_expr json_value))
    | ("op", json_value) -> 
      v.op <- (decode_op json_value)
    | ("rhs", json_value) -> 
      v.rhs <- Some ((decode_expr json_value))
    
    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    Interface_types.lhs = v.lhs;
    Interface_types.op = v.op;
    Interface_types.rhs = v.rhs;
  } : Interface_types.bin_op)

let rec encode_position (v:Interface_types.position) = 
  let open !Interface_types in
  let assoc = [] in 
  let assoc = ("line", Pbrt_yojson.make_int (Int32.to_int v.line)) :: assoc in
  let assoc = ("col", Pbrt_yojson.make_int (Int32.to_int v.col)) :: assoc in
  let assoc = ("offset", Pbrt_yojson.make_int (Int32.to_int v.offset)) :: assoc in
  `Assoc assoc

let rec encode_location (v:Interface_types.location) = 
  let open !Interface_types in
  let assoc = [] in 
  let assoc = ("path", Pbrt_yojson.make_string v.path) :: assoc in
  let assoc = match v.start with
    | None -> assoc
    | Some v -> ("start", encode_position v) :: assoc
  in
  let assoc = match v.end_ with
    | None -> assoc
    | Some v -> ("end", encode_position v) :: assoc
  in
  let assoc =
    let l = v.lines |> List.map Pbrt_yojson.make_string in
    ("lines", `List l) :: assoc 
  in
  `Assoc assoc

let rec encode_op (v:Interface_types.op) = 
  match v with
  | Interface_types.Plus -> `String "Plus"
  | Interface_types.Minus -> `String "Minus"

let rec encode_expr (v:Interface_types.expr) = 
  begin match v with
  | Interface_types.Var v -> `Assoc [("var", Pbrt_yojson.make_string v)]
  | Interface_types.Lit v -> `Assoc [("lit", Pbrt_yojson.make_int (Int32.to_int v))]
  | Interface_types.Bin_op v -> `Assoc [("binOp", encode_bin_op v)]
  end

and encode_bin_op (v:Interface_types.bin_op) = 
  let open !Interface_types in
  let assoc = [] in 
  let assoc = match v.lhs with
    | None -> assoc
    | Some v -> ("lhs", encode_expr v) :: assoc
  in
  let assoc = ("op", encode_op v.op) :: assoc in
  let assoc = match v.rhs with
    | None -> assoc
    | Some v -> ("rhs", encode_expr v) :: assoc
  in
  `Assoc assoc
