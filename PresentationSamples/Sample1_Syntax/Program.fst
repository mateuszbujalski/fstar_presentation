module Program

open FStar.IO
module P = FStar.Printf

open Basic

let main =
    print_string (P.sprintf "Factorial of %d is %d" 5 (factorial 5))
