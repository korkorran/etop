
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



let opam = Command.make "opam" "opam manual" Command.Blank
    |> Command.attachOption (Option.make "version" "get opam version" ~longname:(Some "--version"))
    |> Command.attachSubCommand (Command.make "switch" "list installed opam switches" Command.Blank)
    |> Command.makeBin "opam" "ocaml package manager";;

let graphviz = Command.make "dot" "the dot interpreter" Command.Blank
    |> Command.attachOption (Option.make "version" "get graphviz version" ~shortname:(Some "-V"))
    |> Command.attachOption (Option.make "help" "print graphviz help" ~shortname:(Some "-?"))
    |> Command.makeBin "graphviz" "the graphviz graph producer"

let () = Fmt.pf Fmt.stdout "%a\n" Command.pp_bin opam;;

(* let subCommands = Command.getAllSubCommands graphviz;;

List.iter (fun c -> 
    Logs.info (fun m -> m "subCommand : %s <> %s" c.Command.name c.description)
) subCommands;; *)

let registry = [opam;graphviz]

let rawCommand = "opam switch"
(* 
let pp_string = Fmt.styled (`Fg `Blue) Fmt.string
let () = Fmt.pf Fmt.stdout "%a" pp_string "456" *)

let bin = Registry.findBin registry rawCommand
let () = match bin with
  | None -> Logs.info (fun m -> m "command <%a> not found in registry" RawEntry.pp_t rawCommand)
  | Some b -> Logs.info (fun m -> m "command <%a> found in registry : %a" RawEntry.pp_t rawCommand Command.pp_bin b);
        let parsed = Entry.parse b rawCommand in
        Fmt.pf Fmt.stdout "%a\n" Entry.pp_t parsed

let dim = Tyre.( str"dim:" *> int <&> str"x" *> int ) ;;
let dim_re = Tyre.compile dim ;;
let result = Tyre.exec dim_re "dim:3x4" ;;
let () = match result with 
    | Ok (a,b) -> Fmt.pf Fmt.stdout "dim <%i>*<%i>\n" a b 
    | Error _ -> Fmt.pf Fmt.stdout "error\n"

let () = Fmt.pf Fmt.stdout "%s\n" (Tyre.eval dim (2, 5));;