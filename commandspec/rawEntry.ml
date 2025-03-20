type t = string

(* trim space at the beginning and the end of the string *)
let trim s = String.trim s

(* validate command *)
let notEmpty s = if String.length @@ trim s > 0 then ignore () else failwith "command is empty"

let getRootCommand s =
  let words = String.split_on_char ' ' @@ trim s in List.hd words

let pp_t : string Fmt.t = 
  let open Fmt in
  string
