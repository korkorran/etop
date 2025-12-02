let dim = Tyre.( str"dim:" *> int <&> str"x" *> int ) ;;
let dim_re = Tyre.compile dim ;;
let result = Tyre.exec dim_re "dim:3x4" ;;
let () = match result with 
    | Ok (a,b) -> Fmt.pf Fmt.stdout "dim <%i>*<%i>\n" a b 
    | Error _ -> Fmt.pf Fmt.stdout "error\n"

let () = Fmt.pf Fmt.stdout "%s\n" (Tyre.eval dim (2, 5));;