
module StringMap = struct
  include Map.Make(String)
  let pp _ _ _= ()
end 

type t = {
  binNametoBin: BinSpec.t StringMap.t;
  commandNameToBinName: string StringMap.t;
}

(* auxiliary function to check for duplicates *)
let rec dup_exist checkFun = function
  | [] -> None
  | hd::tl -> if List.exists (checkFun hd) tl then Some hd else dup_exist checkFun tl


(* check if registry has duplicates *)
let validate (bins: BinSpec.t list) : unit =
  let dupBin = dup_exist (fun (b1:BinSpec.t) b2 -> b1.BinSpec.name = b2.name) bins in
  let dupCommand = dup_exist (fun b1 b2 -> b1.BinSpec.command.name = b2.command.name) bins in
  match dupBin, dupCommand with
  | Some b, _ -> failwith ("duplicate binaries in registry : " ^ b.name)
  | _, Some b -> failwith ("duplicate commands in registry : " ^ b.command.name)
  | None, None -> ignore ()

let make (bins: BinSpec.t list) : t =
  {
    binNametoBin= StringMap.of_list @@ List.map (fun b -> (b.BinSpec.name,b)) bins;
    commandNameToBinName = StringMap.of_list @@ List.map (fun b -> (b.BinSpec.command.name,b.name)) bins
  }

(* check if command belongs to registry *)
let findBinbyRawCommand (r: t) (c: string) =
  let root = Token.getRootCommand c in
  match StringMap.find_opt root.token r.commandNameToBinName with
  | None -> None
  | Some binName -> StringMap.find_opt binName r.binNametoBin