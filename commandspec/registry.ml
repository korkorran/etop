type registry = Command.bin list

(* auxiliary function to check for duplicates *)
let rec dup_exist checkFun = function
  | [] -> None
  | hd::tl -> if List.exists (checkFun hd) tl then Some hd else dup_exist checkFun tl


(* check if registry has duplicates *)
let validate (r: registry) : unit =
  let dupBin = dup_exist (fun (b1:Command.bin) b2 -> b1.Command.name = b2.name) r in
  let dupCommand = dup_exist (fun b1 b2 -> b1.Command.command.name = b2.command.name) r in
  match dupBin, dupCommand with
  | Some b, _ -> failwith ("duplicate binaries in registry : " ^ b.name)
  | _, Some b -> failwith ("duplicate commands in registry : " ^ b.command.name)
  | None, None -> ignore ()

(* check if command belongs to registry *)
let findBin (r: registry) (c: RawEntry.t) =
  let root = RawEntry.getRootCommand c in
  List.find_opt (fun b -> b.Command.command.name = root) r