

type t = {
  id : string;
  shortname: string option;
  longname : string option;
  description : string;
} [@@deriving show]

type summary = {
  encoding: string;
  description: string
}

let make ?(shortname=None) ?(longname=None) id description = 
  match shortname, longname with None,None -> failwith "at least one name for parameter should be provided" | _, _->
  {
    id;
    shortname;
    longname;
    description
  }

let getSummary (p:t) =
  match p.shortname,p.longname with
  | Some s,_ -> {encoding=s; description=p.description}
  | _, Some l -> {encoding=l; description=p.description}
  | _,_ -> failwith "no encoding recognized"