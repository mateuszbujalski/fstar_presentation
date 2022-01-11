module EffectSystem

open FStar.Mul

// (Error 19) Could not prove termination of this recursive call
[@@ expect_failure [19]]
let rec factorial (n:int) : int =
    if n = 0 then 1 else n * (factorial (n - 1))

let rec factorial2 (n:int) : Tot int =
    if n <= 0 then 1 else n * (factorial2 (n - 1))

let rec factorial3 (n:nat) : Tot nat =
    if n = 0 then 1 else n * (factorial3 (n - 1))

let rec factorial4 (n : int) : Dv int = 
    if n = 0 then 1 else n * (factorial4 (n - 1))


val f_tot (x : int) : Tot int
let f_tot x = factorial2 x

val f_dv (x : int) : Dv int
let f_dv x = factorial4 x

// (Error 34) Computed type "Prims.int" and effect "Dv" is not compatible with the annotated type "Prims.int" effect "Tot"
[@@ expect_failure [34]]
let g_tot (x : int) : Tot int = f_dv x

let g_dv (x : int) : Dv int = 
    let y = f_dv x in
    // It's fine to call total function inside Dv computation as Tot is sub-effect of Dv
    let z = f_tot x in
    y + z
