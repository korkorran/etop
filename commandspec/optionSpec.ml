

type t = {
  id : string;
  encodings : string list;
  description : string;
} [@@deriving show]


let make id description encodings = 
  if List.length encodings = 0 then failwith "at least one name for parameter should be provided" else
  {
    id;
    encodings;
    description
  }

let pp_encoding fmt encodings = 
    Format.fprintf fmt "[%a]" (Fmt.list ~sep:(Fmt.comma) Fmt.string) encodings