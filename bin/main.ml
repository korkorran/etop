
Logs.set_reporter (Logs_fmt.reporter ());;
Logs.set_level (Some Logs.Debug);;
Printexc.record_backtrace true;;
Fmt.set_style_renderer Fmt.stdout `Ansi_tty;;

(* let s =  "Hello, World! →";;
let regexp1 = Re.Str.regexp "^H.*→$";;
let match1 = Re.Str.string_match regexp1 s 0;;
let () = Printf.printf "match 1 result : %b\n" match1;;


let output = 
    "#  switch  compiler                    description
    →  4.13.1  ocaml-base-compiler.4.13.1  4.13.1
       5.1.1                               5.1.1
       5.2.0   ocaml-base-compiler.5.2.0   5.2.0"
let regexp2 = Re.Str.regexp "\\(.*\\)"
let match2 = Re.Str.string_match regexp2 output 0;;

let () = Printf.printf "match 2 result : %b with substring %s\n" match2 (Re.Str.matched_string output);;


let regex = Re.compile Re.(seq [str "//"; rep print ]);;

let groups = Re.exec regex "// a C comment";;

(* Re.Group.pp Stdlib.Format.std_formatter groups;; *)
Logs.info (fun m -> m "groups : %a" Re.Group.pp groups);;
(* Printf.printf "groups : %a\n" Re.Group.pp groups;; *)

let re =
    let open Re in
    let word =
    alt [rg 'A' 'Z'; rg 'a' 'z'] (* [A-Za-z] *)
    |> rep1 (* [A-Za-z]+ *)
    |> group ~name:"myword" (* ([A-Za-z]+) *)
    in
    seq [word; rep1 blank;   word;  (* blank; word *)]
    |> compile

let groups2 = Re.all re output;;

List.iter (fun group -> 
Logs.info (fun m -> m "groups2 : %a" Re.Group.pp group)
) groups2;;

let () = print_endline s *)

open Commandspec



let opam = CommandSpec.make "opam" "opam manual" (CommandSpec.Instantiable Producer)
    |> CommandSpec.attachOption (OptionSpec.make "version" "get opam version" ["--version"])
    |> CommandSpec.attachSubCommand (CommandSpec.make "switch" "list installed opam switches" (CommandSpec.Instantiable Manager))
    |> BinSpec.make "opam" "ocaml package manager";;

let graphviz = CommandSpec.make "dot" "the dot interpreter" (CommandSpec.Instantiable Producer)
    |> CommandSpec.attachOption (OptionSpec.make "version" "get graphviz version" ["-V"])
    |> CommandSpec.attachOption (OptionSpec.make "help" "print graphviz help" ["-?"])
    |> BinSpec.make "graphviz" "the graphviz graph producer"

let () = Fmt.pf Fmt.stdout "%a\n" BinSpec.pp opam;;

(* let subCommands = Command.getAllSubCommands graphviz;;

List.iter (fun c -> 
    Logs.info (fun m -> m "subCommand : %s <> %s" c.Command.name c.description)
) subCommands;; *)

let registry = Registry.make [opam;graphviz]

let rawCommand = "opam switch"
(* 
let pp_string = Fmt.styled (`Fg `Blue) Fmt.string
let () = Fmt.pf Fmt.stdout "%a" pp_string "456" *)

let binOpt = Registry.findBinbyRawCommand registry rawCommand
let () = match binOpt with
  | None -> Logs.info (fun m -> m "command <%s> not found in registry" rawCommand)
  | Some b -> Logs.info (fun m -> m "command <%s> found in registry : %a" rawCommand BinSpec.pp b);
        let parsed = Command.parseCommand b.BinSpec.command @@ Token.parseIntoTokens rawCommand in
        Fmt.pf Fmt.stdout "%a\n" Command.pp parsed
