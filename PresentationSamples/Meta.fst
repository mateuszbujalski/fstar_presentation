module Meta

open FStar.Mul
open FStar.Tactics

let run (mult : term) : Tac unit =
    let f : term = (`(fun (x : int) -> x * (`#mult))) in
    print (term_to_ast_string f);
    print (string_of_int (unquote #int mult));
    exact f

// We can generate a function that multiplies an input by a given
// number
let f_3 : int -> int = synth_by_tactic (fun () -> run (`3))

let _ = 
    assert (f_3 2 == 6)