
module StringMap = struct
  include Map.Make(String)
  let pp _ _ _= ()
end 

type t = {
  name: string;
  description: string;
  subCommands : t StringMap.t;
  parameters: Parameter.t StringMap.t;
  instantiable: bool
} [@@deriving show]

type bin = {
  name:string;
  description: string;
  command:t
} [@@deriving show]

type commandDesc = {
  name: string;
  description: string;
} [@@deriving show]

let make ?(instantiable=true) name description = {
  name; 
  description; 
  subCommands=StringMap.empty; 
  parameters=StringMap.empty; 
  instantiable
}

let makeBin name description command = {
  name;
  description;
  command
}

let attachSubCommand (c:t) (root:t): t = {
  name=root.name;
  description=root.description;
  subCommands= StringMap.add c.name c root.subCommands;
  parameters=root.parameters;
  instantiable=root.instantiable
}

let attachParameter (p:Parameter.t) (c:t) = {
  name=c.name;
  description=c.description;
  subCommands=c.subCommands;
  parameters = StringMap.add p.Parameter.id p c.parameters;
  instantiable=c.instantiable
}

let getDirectSubCommandsFromCommand c =
  StringMap.bindings c.subCommands |> List.map (fun (_, c: string*t) -> {name=c.name; description=c.description})

let getAllVariantOfCommand (path:string) (c:t) =
  let variants = c.parameters
  |> StringMap.bindings
  |> List.map (fun (_,parameter) -> let s = Parameter.getSummary parameter in {name=path^" "^s.encoding;description=s.description})
  in
  {name=path;description=c.description}::variants

let rec getAllSubCommandsRec (path:string) (c:t) = 
  (* Logs.debug (fun m -> m "exploring at path <%s> command <%s> with description <%s>" path c.name c.description); *)
  let subCommands = c.subCommands
  |> StringMap.bindings
  |> List.map (fun (name,subcommand) -> getAllSubCommandsRec (path ^" "^name) subcommand)
  |> List.concat
  in
  if c.instantiable then (getAllVariantOfCommand path c) @ subCommands else subCommands


let getAllSubCommands (b:bin) =
  getAllSubCommandsRec b.command.name b.command



let getSubCommand c id = 
  StringMap.find id c.subCommands

let getMainCommand b = b.command

let truncate s n =
  let len = min n String.length s in
  String.sub s 0 len

(* let pp_t : t Fmt.t = 
    Fmt.hbox @@ Fmt.braces @@ Fmt.concat ?sep:(Some Fmt.comma) [
      Fmt.field "name" (fun (t:t) -> t.name) (Fmt.styled `Italic Fmt.string);
      Fmt.field "desc" (fun (t:t) -> t.description) Fmt.string
    ]
 *)
(* let pp_bin : bin Fmt.t =
  let open Fmt in
  record [
    field "bin" (fun (t:bin) -> t.name) string;
    field "desc" (fun (t:bin) -> t.description) string;
    field "com" (fun (t:bin) -> t.command) pp_t
  ] *)