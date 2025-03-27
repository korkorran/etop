type t = {
  name:string;
  description: string;
  command:CommandSpec.t
} [@@deriving show]


let make name description command : t = {
  name;
  description;
  command
}

let getMainCommand b = b.command