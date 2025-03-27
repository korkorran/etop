type t = { 
  token : string;
  position : int;
}

type rawEntry = string
type entry = t list

(* trim space at the beginning and the end of the string *)
let trimRawEntry s = String.trim s

(* validate command *)
let notEmptyEntry s = if String.length @@ trimRawEntry s > 0 then ignore () else failwith "command is empty"

(*split raw command into tokens*)
let splitEntry c = String.split_on_char ' ' c

let parseIntoTokens c = trimRawEntry c |> splitEntry |> List.mapi (fun i t -> {token=t; position=i})

let getRootCommand s =
  let words = parseIntoTokens s in (List.hd words)

let isOption t = String.get t.token 0 = '-'

let pp fmt token = 
  Format.fprintf fmt "(%s:%i)" token.token token.position