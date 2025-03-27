module StringMap = Map.Make(String)

type t = {
  path : Token.t list;
  targets : Token.t list;
  options : Option.options
}


let rec parseCommandRec (commandSpec: CommandSpec.t) (tokens: Token.t list) (command:t): t =
  match tokens with
    [] -> command
    | hd::tl -> match StringMap.find_opt hd.token commandSpec.CommandSpec.subCommands with
    | Some cs -> let c1 = {path=command.path; targets=command.targets; options=command.options} in
      parseCommandRec cs tl c1
    |  None -> failwith ("cannot find subcommand for directive : "^hd.token)

let parseCommand (commandSpec: CommandSpec.t) (tokens: Token.t list) = 
  let command = {path=[List.hd tokens]; targets=[]; options=Option.makeEmptyOptions()} in
  parseCommandRec commandSpec (List.tl tokens) command

  
let pp : t Fmt.t =
  Fmt.record [
    Fmt.field "path" (fun (t:t) -> t.path) (Fmt.list Token.pp);
    Fmt.field "targets" (fun (t:t) -> t.targets) (Fmt.list Token.pp);
    Fmt.field "options" (fun (t:t) -> t.options) Option.pp_options
  ]