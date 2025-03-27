
module StringMap = struct
  include Map.Make(String)
  let pp _ _ _= ()
end 

type t = {
  tokens : Token.t list;
  id: string
}

type options = t list StringMap.t

let makeEmptyOptions () : options = StringMap.empty

let pp fmt option = 
  Format.fprintf fmt "{%s:%a}" option.id (Fmt.list Token.pp) option.tokens

let pp_options : options Fmt.t = StringMap.pp pp