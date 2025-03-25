module StringMap = Map.Make(String)

type t = {
  directive: string;
  child: t option
}

let rec getInstance command tokens =
  match tokens with
    [] -> None
    | hd::tl -> match StringMap.find_opt hd command.Command.subCommands with
      None -> failwith ("cannot find subcommand for directive : "^hd)
      | Some d -> Some {directive=d.name; child=(getInstance command tl)}

let parse binary rawInstance =
    let tokens = RawEntry.split @@ RawEntry.trim rawInstance in
    {
      directive= List.hd tokens;
      child= getInstance binary.Command.command (List.tl tokens)
    }
    

let rec pp_t fmt c = 
  Format.fprintf fmt "[%s > %a]" c.directive (Fmt.option pp_t) c.child