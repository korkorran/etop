type token = { 
  token : string;
  position : int;
}

type t = string

(* trim space at the beginning and the end of the string *)
let trim s = String.trim s

(* validate command *)
let notEmpty s = if String.length @@ trim s > 0 then ignore () else failwith "command is empty"

(*split raw command into tokens*)
let split c = String.split_on_char ' ' c

let parseIntoTokens c = trim c |> split |> List.mapi (fun i t -> {token=t; position=i})

let getRootCommand s =
  let words = parseIntoTokens s in (List.hd words).token

let pp_t : string Fmt.t = 
  let open Fmt in
  string
